use linearize::StaticCopyMap;

use crate::grid::Grid;
use crate::models::{Direction, Judgment};
use crate::solver::board::coordinates::{Set, Set1, SetExpr1, SetOp, SetOp1};

struct Triple {
    variable: SetOp,
    in_: Set,
    out: Set,
}

impl Triple {
    pub(crate) fn split(set: &SetOp1, known: &Grid<Option<Judgment>>) -> Option<Self> {
        match set {
            SetOp1::Judged(inner, judgment) => {
                let Self {
                    variable: inner_variable,
                    in_: inner_fixed,
                    mut out,
                } = match inner.as_ref() {
                    &SetExpr1::Regular(set) => Self {
                        variable: SetOp::Empty,
                        in_: set.into(),
                        out: Set::empty(),
                    },
                    SetExpr1::Op(inner) => Self::split(inner, known)?,
                };
                // want to return inner_variable.judged(), inner_fixed.judged()
                let outer = inner_fixed
                    .non_empty()
                    .map_or_default(|fixed| ByJudgment::split(fixed, known));
                if outer.variable == inner_fixed {
                    return None;
                }
                out = out | outer.fixed[!*judgment];
                let in_ = outer.fixed[judgment];
                // want to return ((inner_variable | outer.variable), in_, out)
                let variable: SetOp = match (
                    outer.variable.judged(*judgment),
                    inner_variable.judged(*judgment),
                ) {
                    (SetOp::Empty, SetOp::Empty) => SetOp::Empty,
                    (SetOp::Empty, SetOp::NonEmpty(inner_variable)) => inner_variable.into(),
                    (SetOp::NonEmpty(variable), SetOp::Empty) => variable.into(),
                    (SetOp::NonEmpty(_), SetOp::NonEmpty(_)) => (in_ | out)
                        .complement()
                        .non_empty()
                        .map_or_default(|variable| {
                            inner.clone().judged(*judgment).intersect_set(variable)
                        }),
                };
                Some(Self { variable, in_, out })
            }
            SetOp1::Shift(inner, dir) => Some(Self::split(inner, known)?.shift(*dir)),
            SetOp1::Intersection(..) => todo!(),
        }
    }

    fn shift(self, dir: Direction) -> Self {
        Self {
            variable: self.variable.shift(dir),
            in_: self.in_.shift(dir),
            out: self.out.shift(dir),
        }
    }
}

#[derive(Debug)]
pub(crate) enum SetSimplify {
    Fixed(Set1),
    Variable(SetOp1),
    Split(SetOp1, Set1),
    Empty,
}

impl SetSimplify {
    pub(crate) fn split(set: &SetOp1, known: &Grid<Option<Judgment>>) -> Option<Self> {
        let Triple {
            variable,
            in_,
            out: _out,
        } = Triple::split(set, known)?;
        let split = match (variable, in_.non_empty()) {
            (SetOp::Empty, None) => Self::Empty,
            (SetOp::Empty, Some(set)) => Self::Fixed(set),
            (SetOp::NonEmpty(set), None) => Self::Variable(set),
            (SetOp::NonEmpty(variable), Some(fixed)) => Self::Split(variable, fixed),
        };
        Some(split)
    }

    pub(crate) fn from_pair(variable: SetOp, fixed: Set) -> Self {
        match (variable, fixed.non_empty()) {
            (SetOp::Empty, None) => Self::Empty,
            (SetOp::NonEmpty(variable), None) => Self::Variable(variable),
            (SetOp::Empty, Some(fixed)) => Self::Fixed(fixed),
            (SetOp::NonEmpty(variable), Some(fixed)) => Self::Split(variable, fixed),
        }
    }

    pub(crate) fn shift(self, direction: Direction) -> Self {
        match self {
            Self::Fixed(set) => set.shift(direction).into(),
            Self::Variable(inner) => inner.shift(direction).into(),
            Self::Split(variable, fixed) => {
                Self::from_pair(variable.shift(direction), fixed.shift(direction))
            }
            Self::Empty => Self::Empty,
        }
    }
}

impl From<Set> for SetSimplify {
    fn from(set: Set) -> Self {
        set.non_empty().map_or(Self::Empty, Self::Fixed)
    }
}

impl From<SetOp> for SetSimplify {
    fn from(set: SetOp) -> Self {
        match set {
            SetOp::Empty => Self::Empty,
            SetOp::NonEmpty(set) => Self::Variable(set),
        }
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub(crate) struct ByJudgment {
    variable: Set,
    fixed: StaticCopyMap<Judgment, Set>,
}

impl ByJudgment {
    pub(crate) fn split(set: Set1, known: &Grid<Option<Judgment>>) -> Self {
        let mut fixed = StaticCopyMap::default();
        let mut variable = Set::empty();
        for coord in set {
            match known[coord] {
                Some(judgment) => fixed[judgment] |= coord,
                None => variable |= coord,
            }
        }
        Self { variable, fixed }
    }

    pub(crate) fn only(self, judgment: Judgment) -> [Set; 2] {
        [self.variable, self.fixed[judgment]]
    }

    pub(crate) fn judgment(self, judgment: Judgment) -> Set {
        self.fixed[judgment]
    }

    fn empty() -> Self {
        Self {
            variable: Set::empty(),
            fixed: StaticCopyMap::default(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::macros::{coord, set1};

    use super::*;

    #[test]
    fn simplification_preserves_outer_judgment_on_overlapping_shift() {
        let mut known = Grid::default();
        known[coord!(C 3)] = Some(Judgment::Criminal);
        known[coord!(C 4)] = Some(Judgment::Criminal);
        known[coord!(B 3)] = Some(Judgment::Criminal);
        let shifted = set1!(C 3 | C 4 | C 5)
            .judged(Judgment::Criminal)
            .shift(Direction::Left);
        let SetOp::NonEmpty(shifted) = shifted else {
            panic!("expected shifted set");
        };
        let set = SetOp1::Judged(Box::new(shifted.into()), Judgment::Innocent);

        let SetSimplify::Variable(variable) =
            SetSimplify::split(&set, &known).expect("set should be splittable")
        else {
            panic!("expected variable set");
        };
        let SetOp1::Judged(inner, Judgment::Innocent) = variable else {
            panic!("expected an innocent set");
        };
        let SetExpr1::Op(SetOp1::Shift(inner, Direction::Left)) = inner.as_ref() else {
            panic!("expected a left shift");
        };
        let SetOp1::Judged(inner, Judgment::Criminal) = inner.as_ref() else {
            panic!("expected a criminal set");
        };
        let SetExpr1::Regular(inner) = inner.as_ref() else {
            panic!("expected a regular set");
        };
        assert_eq!(*inner, set1!(C 4 | C 5));
    }
}
