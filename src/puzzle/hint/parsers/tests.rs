use std::fmt;

use itertools::Itertools as _;
use winnow::Parser;
use winnow::error::ParserError;
use winnow::stream::{Stream, StreamIsPartial};

use crate::puzzle::Judgment;
use crate::puzzle::grid::{Column, Row};
use crate::puzzle::hint::recipes::NameRecipe as Name;
use crate::puzzle::hint::{Cardinal, Direction, LineKind, Parity};

use super::{Sentence, SentenceKind, Series, Unit, UnitInSeries};

#[test]
fn ryan_2026_01_12() {
    sentence(
        "exactly 1 of the 2 painters has an innocent directly to the left of them",
        SentenceKind::NumberOfTraitsInUnit(
            Unit::ProfessionShift("painter".into(), Direction::Left, Some(2)),
            Cardinal::Exact(1),
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
            Cardinal::Exact(4),
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
            total: 6,
            quantified: Unit::neighbor("Stella"),
            other: Unit::neighbor("Gabe"),
            intersection: 2,
        },
        Judgment::Innocent,
    );
}

#[test]
fn xena_2026_01_15() {
    sentence(
        "Vince is one of 3 innocents in the corners",
        SentenceKind::IsOneOfNTraitsInUnit(Unit::Corners, "Vince".into(), Cardinal::Exact(3)),
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
fn uma_2026_01_31() {
    sentence(
        "2 out of the 3 teachers have a criminal directly below them",
        SentenceKind::NumberOfTraitsInUnit(
            Unit::ProfessionShift("teacher".into(), Direction::Below, Some(3)),
            Cardinal::Exact(2),
        ),
        Judgment::Criminal,
    );
}

#[test]
fn zara_2026_01_31() {
    sentence(
        "Everyone has at least one innocent neighbor",
        SentenceKind::EachUnitInSeriesHasNTraits(Series::Neighbor, Cardinal::AtLeast(1)),
        Judgment::Innocent,
    );
}

#[test]
fn katie_2026_02_03() {
    sentence(
        "Ryan and I have no innocent neighbors in common",
        SentenceKind::UnitsShareNTraits(
            [Unit::neighbor("Ryan"), Unit::Neighbor(Name::Me)],
            Cardinal::Exact(0),
        ),
        Judgment::Innocent,
    );
}

#[test]
fn uma_2026_02_03() {
    sentence(
        "exactly 1 judge has an innocent directly above them",
        SentenceKind::NumberOfTraitsInUnit(
            Unit::ProfessionShift("judge".into(), Direction::Above, None),
            Cardinal::Exact(1),
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
        SentenceKind::IsOneOfNTraitsInUnit(Row::Four.into(), "Tina".into(), Cardinal::Exact(3)),
        Judgment::Criminal,
    );
}

#[test]
fn chuck_2026_02_05() {
    sentence(
        "exactly 2 of the 4 innocents neighboring Gary are in row\u{a0}1",
        SentenceKind::UnitSharesNOutOfNTraitsWithUnit {
            total: 4,
            quantified: Unit::neighbor("Gary"),
            other: Row::One.into(),
            intersection: 2,
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
            Cardinal::Exact(1),
        ),
        Judgment::Innocent,
    );
}

#[test]
fn ike_2026_02_05() {
    sentence(
        "Xavi has exactly 3 innocent neighbors",
        SentenceKind::NumberOfTraitsInUnit(Unit::neighbor("Xavi"), Cardinal::Exact(3)),
        Judgment::Innocent,
    );
}

#[test]
fn kyle_2026_02_05() {
    sentence(
        "an odd number of innocents above Zara neighbor Gary",
        SentenceKind::UnitsShareNTraits(
            [
                Unit::direction(Direction::Above, "Zara"),
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
        SentenceKind::TraitsAreNeighborsInUnit(Unit::direction(Direction::Above, "Xavi"), Some(2)),
        Judgment::Criminal,
    );
}

#[test]
fn vera_2026_02_05() {
    sentence(
        "Each column has at least 3 innocents",
        SentenceKind::EachUnitInSeriesHasNTraits(LineKind::Column.into(), Cardinal::AtLeast(3)),
        Judgment::Innocent,
    );
}

#[test]
fn freya_2026_02_06() {
    sentence(
        "only one of us 2 singers has exactly 2 criminal neighbors",
        SentenceKind::OnlyOnePersonInUnitHasNTraitNeighbors(
            Unit::profession("singer").quantify(2),
            Cardinal::Exact(2),
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
            Cardinal::Exact(4),
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
            Cardinal::Exact(1),
        ),
        Judgment::Innocent,
    );
}

#[test]
fn logan_2026_02_06() {
    sentence(
        "exactly 1 farmer has a criminal directly above them",
        SentenceKind::NumberOfTraitsInUnit(
            Unit::ProfessionShift("farmer".to_owned(), Direction::Above, None),
            Cardinal::Exact(1),
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
            Unit::direction(Direction::Left, "Noah"),
            Cardinal::Exact(2),
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
            total: 2,
            quantified: Column::C.into(),
            other: Unit::neighbor("Zara"),
            intersection: 1,
        },
        Judgment::Innocent,
    );
}

#[test]
fn uma_2026_02_07() {
    sentence(
        "only 1 of the 3 innocents neighboring me is to the right of Kay",
        SentenceKind::UnitSharesNOutOfNTraitsWithUnit {
            total: 3,
            quantified: Unit::Neighbor(Name::Me),
            other: Unit::direction(Direction::Right, "Kay"),
            intersection: 1,
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
            Cardinal::Exact(0),
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
            Cardinal::Exact(2),
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
            Cardinal::Exact(2),
        ),
        Judgment::Innocent,
    );
}

#[test]
fn kumar_2026_02_09() {
    sentence(
        "exactly 2 of the 3 innocents in row 5 are Susan's neighbors",
        SentenceKind::UnitSharesNOutOfNTraitsWithUnit {
            total: 3,
            quantified: Row::Five.into(),
            other: Unit::neighbor("Susan"),
            intersection: 2,
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
        SentenceKind::NumberOfTraitsInUnit(Unit::Edges, Cardinal::AtLeast(10)),
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
            Cardinal::Exact(2),
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
            Cardinal::Exact(1),
        ),
        Judgment::Innocent,
    );
}

#[test]
fn will_2026_02_10() {
    sentence(
        "2 of us 3 singers have an innocent directly to the left of us",
        SentenceKind::NumberOfTraitsInUnit(
            Unit::ProfessionShift("singer".into(), Direction::Left, Some(3)),
            Cardinal::Exact(2),
        ),
        Judgment::Innocent,
    );
}

#[test]
fn janet_2026_02_10() {
    sentence(
        "There are 9 innocents in total",
        SentenceKind::NumberOfTraitsInUnit(Unit::All, Cardinal::Exact(9)),
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
            Cardinal::Exact(2),
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

#[test]
fn jose_879da349c27d() {
    sentence(
        "I have exactly 5 innocent neighbors",
        SentenceKind::NumberOfTraitsInUnit(Unit::Neighbor(Name::Me), Cardinal::Exact(5)),
        Judgment::Innocent,
    );
}

#[test]
fn ryan_327a79cc5a8c() {
    sentence(
        "Zoe is the only one with exactly 1 criminal neighbors",
        SentenceKind::OnlyGivenUnitHasNTraits(UnitInSeries::neighbor("Zoe"), Cardinal::Exact(1)),
        Judgment::Criminal,
    );
}

#[test]
fn gary_dd0a4616a658() {
    sentence(
        "Nancy has only one innocent neighbor on the edges",
        SentenceKind::UnitsShareNTraits(
            [Unit::neighbor("Nancy"), (Unit::Edges)],
            Cardinal::Exact(1),
        ),
        Judgment::Innocent,
    );
}

#[test]
fn olga_d9b7f6418e96() {
    sentence(
        "2 of Gus' neighbors on the edges are innocent",
        SentenceKind::UnitsShareNTraits([Unit::neighbor("Gus"), Unit::Edges], Cardinal::Exact(2)),
        Judgment::Innocent,
    );
}

#[test]
fn julie_puzzle_pack_1_1() {
    sentence(
        "Terry is one of two or more innocents on the edges",
        SentenceKind::IsOneOfNTraitsInUnit(Unit::Edges, "Terry".into(), Cardinal::AtLeast(2)),
        Judgment::Innocent,
    );
}

#[test]
fn olof_puzzle_pack_1_1() {
    sentence(
        "the only criminal below Julie is Terry's neighbor",
        SentenceKind::UnitSharesNOutOfNTraitsWithUnit {
            total: 1,
            quantified: Unit::direction(Direction::Below, "Julie"),
            other: Unit::neighbor("Terry"),
            intersection: 1,
        },
        Judgment::Criminal,
    );
}

#[test]
fn flora_puzzle_pack_1_2() {
    sentence(
        "Nicole's only innocent neighbor is Martin's neighbor",
        SentenceKind::UnitSharesNOutOfNTraitsWithUnit {
            total: 1,
            quantified: Unit::neighbor("Nicole"),
            other: Unit::neighbor("Martin"),
            intersection: 1,
        },
        Judgment::Innocent,
    );
}

#[test]
fn xia_puzzle_pack_1_2() {
    sentence(
        "I have more innocent neighbors than Olivia",
        SentenceKind::MoreTraitsInUnitThanUnit {
            big: Unit::Neighbor(Name::Me),
            small: Unit::neighbor("Olivia"),
        },
        Judgment::Innocent,
    );
}

#[test]
fn flora_puzzle_pack_1_3() {
    sentence(
        "Emily and I share an odd number of innocent neighbors",
        SentenceKind::UnitsShareNTraits(
            [Unit::neighbor("Emily"), Unit::Neighbor(Name::Me)],
            Parity::Odd.into(),
        ),
        Judgment::Innocent,
    );
}

#[test]
fn frank_puzzle_pack_1_5() {
    sentence(
        "Alice's only innocent neighbor is to the left of Helen",
        SentenceKind::UnitSharesNOutOfNTraitsWithUnit {
            total: 1,
            quantified: Unit::neighbor("Alice"),
            other: Unit::direction(Direction::Left, "Helen"),
            intersection: 1,
        },
        Judgment::Innocent,
    );
}

#[test]
fn katie_puzzle_pack_1_6() {
    sentence(
        "both innocents in row 4 are Laura's neighbors",
        SentenceKind::UnitSharesNOutOfNTraitsWithUnit {
            total: 2,
            quantified: Row::Four.into(),
            other: Unit::neighbor("Laura"),
            intersection: 2,
        },
        Judgment::Innocent,
    );
}

#[test]
fn zara_puzzle_pack_1_6() {
    sentence(
        "neither of the 2 innocents neighboring Olive are to the left of Noah",
        SentenceKind::UnitSharesNOutOfNTraitsWithUnit {
            total: 2,
            quantified: Unit::neighbor("Olive"),
            other: Unit::direction(Direction::Left, "Noah"),
            intersection: 0,
        },
        Judgment::Innocent,
    );
}

#[test]
fn bonnie_puzzle_pack_1_13() {
    sentence(
        "none of the 7 criminals on the edges is a painter",
        SentenceKind::UnitSharesNOutOfNTraitsWithUnit {
            total: 7,
            quantified: Unit::Edges,
            other: Unit::profession("painter"),
            intersection: 0,
        },
        Judgment::Criminal,
    );
}

#[test]
fn mary_puzzle_pack_1_14() {
    sentence(
        "Zach has more criminal than innocent neighbors",
        SentenceKind::MoreTraitsInUnit(Unit::neighbor("Zach")),
        Judgment::Criminal,
    );
}

#[test]
fn tom_puzzle_pack_1_15() {
    sentence(
        "There are as many criminals as innocents below Linda",
        SentenceKind::EqualTraitsInUnit(Unit::direction(Direction::Below, "Linda")),
        Judgment::Criminal,
    );
}

#[test]
fn zoe_puzzle_pack_1_15() {
    sentence(
        "Linda and Tom have 4 innocent neighbors in total",
        SentenceKind::TotalNumberOfTraitsInUnits(
            ["Linda", "Tom"].map(Unit::neighbor),
            Cardinal::Exact(4),
        ),
        Judgment::Innocent,
    );
}

#[test]
fn zoe_puzzle_pack_1_16() {
    sentence(
        "There are more criminal than innocent guards",
        SentenceKind::MoreTraitsInUnit(Unit::profession("guard")),
        Judgment::Criminal,
    );
}

#[test]
fn will_puzzle_pack_1_17() {
    sentence(
        "I am one of Xia's 4 criminal neighbors",
        SentenceKind::IsOneOfNTraitsInUnit(Unit::neighbor("Xia"), Name::Me, Cardinal::Exact(4)),
        Judgment::Criminal,
    );
}

#[test]
fn cheryl_puzzle_pack_1_27() {
    sentence(
        "2 of Isaac's innocent neighbors are in row&nbsp;3",
        SentenceKind::UnitsShareNTraits(
            [Unit::neighbor("Isaac"), Row::Three.into()],
            Cardinal::Exact(2),
        ),
        Judgment::Innocent,
    );
}

#[test]
fn freya_puzzle_pack_1_45() {
    sentence(
        "exactly 1 innocent neighboring Wally is builder",
        SentenceKind::UnitsShareNTraits(
            [Unit::neighbor("Wally"), Unit::profession("builder")],
            Cardinal::Exact(1),
        ),
        Judgment::Innocent,
    );
}

#[test]
fn eve_puzzle_pack_1_49() {
    sentence(
        "Katie shares neither of her 2 innocent neighbors with Laura",
        SentenceKind::UnitSharesNOutOfNTraitsWithUnit {
            total: 2,
            quantified: Unit::neighbor("Katie"),
            other: Unit::neighbor("Laura"),
            intersection: 0,
        },
        Judgment::Innocent,
    );
}

#[test]
fn debra_community_49f3f1_9eb600102931a676() {
    sentence(
        "No one in row 1 has more than one criminal neighbor",
        SentenceKind::AtMostNTraitsInNeighborsInUnit(Row::One.into(), 1),
        Judgment::Criminal,
    );
}

fn sentence(input: &str, kind: SentenceKind, judgment: Judgment) {
    let input = input.split(' ').filter(|s| !s.is_empty()).collect_vec();
    parser(Sentence::any, &input, &Sentence { kind, judgment });
}

fn parser<
    I: Stream + StreamIsPartial,
    P: Parser<I, T, E>,
    T: PartialEq + fmt::Debug,
    E: ParserError<I, Inner: fmt::Debug + ParserError<I>>,
>(
    mut parser: P,
    input: I,
    expected: &T,
) {
    let output = parser.parse(input).unwrap();
    assert_eq!(&output, expected);
}
