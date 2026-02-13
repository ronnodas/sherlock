use std::fmt;

use winnow::Parser;
use winnow::error::ParserError;

use crate::puzzle::Judgment;
use crate::puzzle::grid::{Column, Row};
use crate::puzzle::hint::{Direction, LineKind, Parity, Quantity};

use super::{NameRecipe as Name, Sentence, SentenceKind, Unit, UnitInSeries};

#[test]
fn ryan_2026_01_12() {
    sentence(
        "exactly 1 of the 2 painters has an innocent directly to the left of them",
        SentenceKind::NumberOfTraitsInUnit(
            Unit::ProfessionShift("painter".into(), Direction::Left),
            Quantity::Exact(1),
        ),
        Judgment::Innocent,
    );
}

#[test]
fn wanda_2026_01_12() {
    sentence(
        "Frank is the only one on the edges with 4 innocent neighbors",
        SentenceKind::OnlyOnePersonInUnitHasNTraitNeighbors(
            Unit::Edges,
            Quantity::Exact(4),
            Some("Frank".into()),
        ),
        Judgment::Innocent,
    );
}

#[test]
fn janet_2026_01_13() {
    sentence(
        "exactly 2 of Stella's 6 innocent neighbors also neighbor Gabe",
        SentenceKind::UnitSharesNOutOfNTraitsWithUnit {
            quantity: Quantity::Exact(6),
            quantified: Unit::neighbor("Stella"),
            other: Unit::neighbor("Gabe"),
            intersection: Quantity::Exact(2),
        },
        Judgment::Innocent,
    );
}

#[test]
fn xena_2026_01_15() {
    sentence(
        "Vince is one of 3 innocents in the corners",
        SentenceKind::IsOneOfNTraitsInUnit(Unit::Corners, "Vince".into(), Quantity::Exact(3)),
        Judgment::Innocent,
    );
}

#[test]
fn salil_2026_01_15() {
    sentence(
        "No one in row 4 has more than 2 criminal neighbors",
        SentenceKind::AtMostNTraitsInNeighborsInUnit(Row::Four.into(), 2),
        Judgment::Criminal,
    );
}

#[test]
fn uma_2026_02_03() {
    sentence(
        "exactly 1 judge has an innocent directly above them",
        SentenceKind::NumberOfTraitsInUnit(
            Unit::ProfessionShift("judge".into(), Direction::Above),
            Quantity::Exact(1),
        ),
        Judgment::Innocent,
    );
}

#[test]
fn salil_2026_02_04() {
    sentence(
        "There are as many innocent builders as there are innocent guards",
        SentenceKind::EqualNumberOfTraitsInUnits(["builder", "guard"].map(Unit::profession)),
        Judgment::Innocent,
    );
}

#[test]
fn alice_2026_02_05() {
    sentence(
        "Tina is one of 3 criminals in row\u{A0}4",
        SentenceKind::IsOneOfNTraitsInUnit(Row::Four.into(), "Tina".into(), Quantity::Exact(3)),
        Judgment::Criminal,
    );
}

#[test]
fn chuck_2026_02_05() {
    sentence(
        "exactly 2 of the 4 innocents neighboring Gary are in row\u{a0}1",
        SentenceKind::UnitSharesNOutOfNTraitsWithUnit {
            quantity: Quantity::Exact(4),
            quantified: Unit::neighbor("Gary"),
            other: Row::One.into(),
            intersection: Quantity::Exact(2),
        },
        Judgment::Innocent,
    );
}

#[test]
fn ethan_2026_02_05() {
    sentence(
        "an odd number of innocents on the edges neighbor Gary",
        SentenceKind::UnitsShareNTraits([Unit::Edges, Unit::neighbor("Gary")], Parity::Odd.into()),
        Judgment::Innocent,
    );
}

#[test]
fn gary_2026_02_05() {
    sentence(
        "exactly 1 innocent in row\u{a0}4 is neighboring Xavi",
        SentenceKind::UnitsShareNTraits(
            [Row::Four.into(), Unit::neighbor("Xavi")],
            Quantity::Exact(1),
        ),
        Judgment::Innocent,
    );
}

#[test]
fn ike_2026_02_05() {
    sentence(
        "Xavi has exactly 3 innocent neighbors",
        SentenceKind::NumberOfTraitsInUnit(Unit::neighbor("Xavi"), Quantity::Exact(3)),
        Judgment::Innocent,
    );
}

#[test]
fn kyle_2026_02_05() {
    sentence(
        "an odd number of innocents above Zara neighbor Gary",
        SentenceKind::UnitsShareNTraits(
            [
                Unit::Direction(Direction::Above, "Zara".into()),
                Unit::neighbor("Gary"),
            ],
            Parity::Odd.into(),
        ),
        Judgment::Innocent,
    );
}

#[test]
fn tina_2026_02_05() {
    sentence(
        "both criminals above Xavi are connected",
        SentenceKind::TraitsAreNeighborsInUnit(
            Unit::Direction(Direction::Above, "Xavi".into()),
            Some(Quantity::Exact(2)),
        ),
        Judgment::Criminal,
    );
}

#[test]
fn vera_2026_02_05() {
    sentence(
        "Each column has at least 3 innocents",
        SentenceKind::EachLineHasNTraits(LineKind::Column, Quantity::AtLeast(3)),
        Judgment::Innocent,
    );
}

#[test]
fn freya_2026_02_06() {
    sentence(
        "Only one of us 2 singers has exactly 2 criminal neighbors",
        SentenceKind::OnlyOnePersonInUnitHasNTraitNeighbors(
            Unit::profession("singer").quantify(Quantity::Exact(2)),
            Quantity::Exact(2),
            None,
        ),
        Judgment::Criminal,
    );
}

#[test]
fn helen_2026_02_06() {
    sentence(
        "Jason is one of Ellie's 4 innocent neighbors",
        SentenceKind::IsOneOfNTraitsInUnit(
            Unit::neighbor("Ellie"),
            "Jason".into(),
            Quantity::Exact(4),
        ),
        Judgment::Innocent,
    );
}

#[test]
fn jason_2026_02_06() {
    sentence(
        "Ellie and Noah have only one innocent neighbor in common",
        SentenceKind::UnitsShareNTraits(
            ["Ellie", "Noah"].map(Name::from).map(Unit::Neighbor),
            Quantity::Exact(1),
        ),
        Judgment::Innocent,
    );
}

#[test]
fn logan_2026_02_06() {
    sentence(
        "exactly 1 farmer has a criminal directly above them",
        SentenceKind::NumberOfTraitsInUnit(
            Unit::ProfessionShift("farmer".to_owned(), Direction::Above),
            Quantity::Exact(1),
        ),
        Judgment::Criminal,
    );
}

#[test]
fn ivan_2026_02_06() {
    sentence(
        "row 5 has more innocents than any other row",
        SentenceKind::HasMostTraits(Row::Five.into()),
        Judgment::Innocent,
    );
}

#[test]
fn scott_2026_02_06() {
    sentence(
        "There are exactly 2 innocents to the left of Noah",
        SentenceKind::NumberOfTraitsInUnit(
            Unit::Direction(Direction::Left, "Noah".into()),
            Quantity::Exact(2),
        ),
        Judgment::Innocent,
    );
}

#[test]
fn vera_2026_02_06() {
    sentence(
        "There are more innocent cooks than innocent mechs",
        SentenceKind::MoreTraitsInUnitThanUnit {
            big: Unit::profession("cook"),
            small: Unit::profession("mech"),
        },
        Judgment::Innocent,
    );
}

#[test]
fn gary_2026_02_07() {
    sentence(
        "only 1 of the 2 innocents in column\u{a0}C is Zara's neighbor",
        SentenceKind::UnitSharesNOutOfNTraitsWithUnit {
            quantity: Quantity::Exact(2),
            quantified: Column::C.into(),
            other: Unit::neighbor("Zara"),
            intersection: Quantity::Exact(1),
        },
        Judgment::Innocent,
    );
}

#[test]
fn uma_2026_02_07() {
    sentence(
        "only 1 of the 3 innocents neighboring me is to the right of Kay",
        SentenceKind::UnitSharesNOutOfNTraitsWithUnit {
            quantity: Quantity::Exact(3),
            quantified: Unit::Neighbor(Name::Me),
            other: Unit::Direction(Direction::Right, "Kay".into()),
            intersection: Quantity::Exact(1),
        },
        Judgment::Innocent,
    );
}

#[test]
fn xena_2026_02_08() {
    sentence(
        "There are no innocents in row 1 who neighbor Donna",
        SentenceKind::UnitsShareNTraits(
            [Row::One.into(), Unit::neighbor("Donna")],
            Quantity::Exact(0),
        ),
        Judgment::Innocent,
    );
}

#[test]
fn hank_2026_02_08() {
    sentence(
        "Only one person in a corner has exactly 2 innocent neighbors",
        SentenceKind::OnlyOnePersonInUnitHasNTraitNeighbors(
            Unit::Corners,
            Quantity::Exact(2),
            None,
        ),
        Judgment::Innocent,
    );
}

#[test]
fn tina_2026_02_09() {
    sentence(
        "exactly 2 innocents in column C are neighboring me",
        SentenceKind::UnitsShareNTraits(
            [Column::C.into(), Unit::Neighbor(Name::Me)],
            Quantity::Exact(2),
        ),
        Judgment::Innocent,
    );
}

#[test]
fn kumar_2026_02_09() {
    sentence(
        "exactly 2 of the 3 innocents in row 5 are Susan's neighbors",
        SentenceKind::UnitSharesNOutOfNTraitsWithUnit {
            quantity: Quantity::Exact(3),
            quantified: Row::Five.into(),
            other: Unit::neighbor("Susan"),
            intersection: Quantity::Exact(2),
        },
        Judgment::Innocent,
    );
}

#[test]
fn xavi_2026_02_09() {
    sentence(
        "There are more innocents in row 3 than row 5",
        SentenceKind::MoreTraitsInUnitThanUnit {
            big: Row::Three.into(),
            small: Row::Five.into(),
        },
        Judgment::Innocent,
    );
}

#[test]
fn ollie_2026_02_09() {
    sentence(
        "There's an equal number of innocents in rows 2 and 3",
        SentenceKind::EqualNumberOfTraitsInUnits([Row::Two, Row::Three].map(Unit::from)),
        Judgment::Innocent,
    );
}

#[test]
fn gabe_2026_02_09() {
    sentence(
        "There are at least 10 innocents on the edges",
        SentenceKind::NumberOfTraitsInUnit(Unit::Edges, Quantity::AtLeast(10)),
        Judgment::Innocent,
    );
}

#[test]
fn gary_2026_02_10() {
    sentence(
        "Ryan is one of 2 innocents in between Betty and Vicky",
        SentenceKind::IsOneOfNTraitsInUnit(
            Unit::Between(["Betty", "Vicky"].map(Name::from)),
            "Ryan".into(),
            Quantity::Exact(2),
        ),
        Judgment::Innocent,
    );
}

#[test]
fn lisa_2026_02_10() {
    sentence(
        "exactly 1 innocent on the edges is a farmer",
        SentenceKind::UnitsShareNTraits(
            [Unit::Edges, Unit::profession("farmer")],
            Quantity::Exact(1),
        ),
        Judgment::Innocent,
    );
}

#[test]
fn will_2026_02_10() {
    sentence(
        "2 of us 3 singers have an innocent directly to the left of us",
        SentenceKind::NumberOfTraitsInUnit(
            Unit::ProfessionShift("singer".into(), Direction::Left),
            Quantity::Exact(2),
        ),
        Judgment::Innocent,
    );
}

#[test]
fn janet_2026_02_10() {
    sentence(
        "There are 9 innocents in total",
        SentenceKind::NumberOfTraitsInUnit(Unit::All, Quantity::Exact(9)),
        Judgment::Innocent,
    );
}

#[test]
fn noah_2026_02_11() {
    sentence(
        "Olof is a criminal",
        SentenceKind::HasTrait("Olof".into()),
        Judgment::Criminal,
    );
}

#[test]
fn ollie_2026_02_12() {
    sentence(
        "There's an odd number of innocents neighboring Celia in row 2",
        SentenceKind::UnitsShareNTraits(
            [Unit::neighbor("Celia"), Row::Two.into()],
            Parity::Odd.into(),
        ),
        Judgment::Innocent,
    );
}

#[test]
fn olive_2026_02_13() {
    sentence(
        "2 of my neighbors on the edges are innocent",
        SentenceKind::UnitsShareNTraits(
            [Unit::Neighbor(Name::Me), Unit::Edges],
            Quantity::Exact(2),
        ),
        Judgment::Innocent,
    );
}

#[test]
fn diane_0cf47() {
    sentence(
        "Xavi has more criminal neighbors than Ben",
        SentenceKind::MoreTraitsInUnitThanUnit {
            big: Unit::neighbor("Xavi"),
            small: Unit::neighbor("Ben"),
        },
        Judgment::Criminal,
    );
}

#[test]
fn hal_0cf47() {
    sentence(
        "Emily and Tom have an equal number of criminal neighbors",
        SentenceKind::EqualNumberOfTraitsInUnits(
            ["Emily", "Tom"].map(Name::from).map(Unit::Neighbor),
        ),
        Judgment::Criminal,
    );
}

#[test]
fn paul_0cf47() {
    sentence(
        "There are more criminals among guards than any other profession",
        SentenceKind::HasMostTraits(UnitInSeries::profession("guard")),
        Judgment::Criminal,
    );
}

#[test]
fn rob_0cf47() {
    sentence(
        "There are more criminals than innocents in a corner",
        SentenceKind::MoreTraitsInUnit(Unit::Corners),
        Judgment::Criminal,
    );
}

#[test]
fn vicky_0cf47() {
    sentence(
        "Paul has the most criminal neighbors",
        SentenceKind::HasMostTraits(UnitInSeries::neighbor("Paul")),
        Judgment::Criminal,
    );
}

fn sentence(input: &str, kind: SentenceKind, judgment: Judgment) {
    parser(Sentence::any, input, &Sentence { kind, judgment });
}

fn parser<
    'input,
    P: Parser<&'input str, T, E>,
    T: PartialEq + fmt::Debug,
    E: ParserError<&'input str, Inner: fmt::Debug + ParserError<&'input str>>,
>(
    mut parser: P,
    input: &'input str,
    expected: &T,
) {
    let output = parser.parse(input).unwrap();
    assert_eq!(&output, expected);
}
