use anyhow::{Result, bail};
use mitsein::hash_set1::HashSet1;
use mitsein::iter1::IntoIterator1 as _;
use mitsein::vec1::Vec1;

use crate::puzzle::Name;
use crate::puzzle::grid::{Coordinate, Grid};
use crate::puzzle::hint::parsers::{Sentence, Unit, UnitInSeries};
use crate::puzzle::hint::{Hint, HintKind, Line, LineKind, Quantity, Set, WithJudgment};

#[cfg_attr(test, derive(PartialEq, Eq))]
#[derive(Debug, Clone)]
pub(crate) enum NameRecipe {
    Me,
    Other(Name),
}

impl From<&str> for NameRecipe {
    fn from(v: &str) -> Self {
        Self::Other(v.to_owned())
    }
}

pub(crate) type HintRecipe = WithJudgment<HintRecipeKind>;

impl HintRecipe {
    pub(crate) fn parse(hint: &str) -> Result<Vec<Self>> {
        Ok(Sentence::parse(&hint.replace("&nbsp;", "\u{A0}"))?.collate())
    }
}

#[cfg_attr(test, derive(PartialEq, Eq))]
#[derive(Debug)]
pub(crate) enum HintRecipeKind {
    Is(NameRecipe),
    //TODO get rid of Member, or add more general validation
    Member(NameRecipe, Unit),
    Count(SetRecipe, Quantity),
    Connected(Unit),
    EqualSize([Unit; 2]),
    BiggerThanOthers(UnitInSeries),
    Bigger { big: Unit, small: Unit },
    Majority(Unit),
    UniqueWithNeighbors(Unit, Quantity),
    UniqueLine(LineKind, Quantity),
    Not(Box<Self>),
}

impl HintRecipeKind {
    pub(crate) fn not(self) -> Self {
        if let Self::Not(reverse) = self {
            *reverse
        } else {
            Self::Not(Box::new(self))
        }
    }
}

#[cfg_attr(test, derive(PartialEq, Eq))]
#[derive(Debug)]
pub(crate) enum SetRecipe {
    Unit(Unit),
    Intersection(Vec1<Unit>),
}

impl From<Unit> for SetRecipe {
    fn from(unit: Unit) -> Self {
        Self::Unit(unit)
    }
}

impl From<Line> for SetRecipe {
    fn from(line: Line) -> Self {
        Self::Unit(Unit::Line(line))
    }
}

pub(crate) trait Recipe {
    type Output;

    fn contextualize(self, grid: &Grid, speaker: &Name) -> Result<Self::Output>;
}

impl Recipe for HintRecipe {
    type Output = Hint;

    fn contextualize(self, grid: &Grid, speaker: &Name) -> Result<Self::Output> {
        Ok(Hint {
            kind: self.kind.contextualize(grid, speaker)?,
            judgment: self.judgment,
        })
    }
}

impl Recipe for HintRecipeKind {
    type Output = HintKind;

    fn contextualize(self, grid: &Grid, speaker: &Name) -> Result<Self::Output> {
        let hint = match self {
            Self::Member(name, set) => {
                let coordinate = name.contextualize(grid, speaker)?;
                let set = set.contextualize(grid, speaker)?;
                if !set.contains(&coordinate) {
                    bail!("invalid hint: {name:?} not in {set:?}")
                }
                HintKind::Judgment(coordinate)
            }
            Self::Count(set, quantity) => {
                HintKind::Count(set.contextualize(grid, speaker)?, quantity)
            }
            Self::Connected(set) => HintKind::Connected(set.contextualize(grid, speaker)?),
            Self::Bigger { big, small } => HintKind::Bigger {
                big: big.contextualize(grid, speaker)?,
                small: small.contextualize(grid, speaker)?,
            },
            Self::UniqueWithNeighbors(unit, quantity) => {
                let Ok(set) = HashSet1::try_from(unit.contextualize(grid, speaker)?) else {
                    bail!("empty unit {unit:?} cannnot have unique member")
                };
                let sets = set
                    .into_iter1()
                    .map(|coord| Coordinate::neighbors(coord).collect())
                    .collect1();
                HintKind::UniqueWithCount(sets, quantity)
            }
            Self::UniqueLine(kind, quantity) => {
                let sets = kind.all().into_iter1().map(Set::from).collect1();
                HintKind::UniqueWithCount(sets, quantity)
            }
            Self::EqualSize(units) => {
                let [a, b] = units.map(|unit| unit.contextualize(grid, speaker));
                HintKind::Equal([a?, b?])
            }
            Self::BiggerThanOthers(big) => {
                let small = big.others(grid, speaker)?;
                let big = Unit::from(big).contextualize(grid, speaker)?;
                HintKind::BiggerThanMany { big, small }
            }
            Self::Majority(unit) => {
                let set = unit.contextualize(grid, speaker)?;
                HintKind::Majority(set)
            }
            Self::Is(name) => {
                let coord = name.contextualize(grid, speaker)?;
                HintKind::Judgment(coord)
            }
            Self::Not(reverse) => HintKind::Not(Box::new(reverse.contextualize(grid, speaker)?)),
        };
        Ok(hint)
    }
}

impl Recipe for SetRecipe {
    type Output = Set;

    fn contextualize(self, grid: &Grid, speaker: &Name) -> Result<Self::Output> {
        match self {
            Self::Unit(unit) => unit.contextualize(grid, speaker),
            Self::Intersection(sets) => sets
                .into_iter1()
                .map(|set| set.contextualize(grid, speaker))
                .reduce(|a, b| Ok(a?.intersection(&b?).copied().collect())),
        }
    }
}

impl Recipe for &Unit {
    type Output = Set;

    fn contextualize(self, grid: &Grid, speaker: &Name) -> Result<Self::Output> {
        let set = match self {
            &Unit::Line(line) => line.into(),
            Unit::Direction(direction, name) => {
                let start = name.contextualize(grid, speaker)?;
                Coordinate::direction(start, *direction).collect()
            }
            Unit::Neighbor(name) => {
                let center = name.contextualize(grid, speaker)?;
                Coordinate::neighbors(center).collect()
            }
            Unit::Profession(profession) => grid.by_profession(profession)?.clone().into_hash_set(),
            Unit::Edges => Coordinate::edges().collect(),
            Unit::Corners => Coordinate::corners().collect(),
            Unit::ProfessionShift(profession, direction) => grid
                .by_profession(profession)?
                .into_iter()
                .filter_map(|coord| coord.step(*direction))
                .collect(),
            Unit::Between(names) => {
                let [a, b] = names
                    .each_ref()
                    .map(|name| name.contextualize(grid, speaker));
                Coordinate::between([a?, b?])?
            }
            Unit::All => Coordinate::all().collect(),
            Unit::Quantified(inner, quantity) => {
                let set = inner.contextualize(grid, speaker)?;
                if !quantity.matches(set.len()) {
                    bail!("{inner:?} does not have {quantity:?} members")
                }
                set
            }
        };
        Ok(set)
    }
}

impl Recipe for &NameRecipe {
    type Output = Coordinate;

    fn contextualize(self, grid: &Grid, speaker: &Name) -> Result<Self::Output> {
        let name = match self {
            NameRecipe::Me => speaker,
            NameRecipe::Other(name) => name,
        };
        grid.coord(name)
    }
}

#[cfg(test)]
mod tests {
    use crate::puzzle::Judgment;
    use crate::puzzle::grid::{Direction, Row};
    use crate::puzzle::hint::parsers::Unit;
    use crate::puzzle::hint::{Line, Parity};

    use super::{HintRecipe, HintRecipeKind, Quantity};

    #[test]
    fn sample_26_02_05_alice() {
        assert_eq!(
            HintRecipe::parse("Tina is one of 3 criminals in row\u{A0}4").unwrap(),
            [
                HintRecipe {
                    kind: HintRecipeKind::Count(
                        Unit::Line(Line::Row(Row::Four)).into(),
                        Quantity::Exact(3)
                    ),
                    judgment: Judgment::Criminal,
                },
                HintRecipe {
                    kind: HintRecipeKind::Member("Tina".into(), Row::Four.into(),),
                    judgment: Judgment::Criminal
                },
            ]
        );
    }

    #[test]
    fn sample_26_02_05_tina() {
        let set = Unit::Direction(Direction::Above, "Xavi".into());
        assert_eq!(
            HintRecipe::parse("Both criminals above Xavi are connected").unwrap(),
            [
                HintRecipe {
                    kind: HintRecipeKind::Count(set.clone().into(), Quantity::Exact(2)),
                    judgment: Judgment::Criminal
                },
                HintRecipe {
                    kind: HintRecipeKind::Connected(set),
                    judgment: Judgment::Criminal
                }
            ]
        );
    }

    #[test]
    fn sample_26_02_05_kyle() {
        assert_eq!(
            HintRecipe::parse("An odd number of innocents above Zara neighbor Gary").unwrap(),
            [HintRecipe {
                kind: HintRecipeKind::Count(
                    Unit::Direction(Direction::Above, "Zara".into())
                        .and(Unit::Neighbor("Gary".into())),
                    Quantity::Parity(Parity::Odd)
                ),
                judgment: Judgment::Innocent,
            }],
        );
    }
}
