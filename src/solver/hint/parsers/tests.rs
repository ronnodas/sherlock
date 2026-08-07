use std::fmt;

use itertools::Itertools as _;
use winnow::Parser;
use winnow::error::ParserError;
use winnow::stream::{Stream, StreamIsPartial};

use crate::models::{Column, Direction, Judgment, Row};
use crate::solver::hint::recipes::{ColumnRecipe, NameRecipe as Name};
use crate::solver::hint::{Cardinal, LineKind, Parity};

use super::{Sentence, Series, Unit, UnitInSeries};

#[test]
fn ryan_2026_01_12() {
    sentence(
        "exactly 1 of the 2 painters has an innocent directly to the left of them",
        &Sentence::UnitSize(
            Unit::profession("painter")
                .quantify(2)
                .shift(Direction::Left)
                .with_judgment(Judgment::Innocent),
            Cardinal::Exact(1),
        ),
    );
}

#[test]
fn wanda_2026_01_12() {
    sentence(
        "Frank is the only one on the edges with 4 innocent neighbors",
        &Sentence::UniqueInUnitHasNNeighbors(
            Unit::Edges,
            Cardinal::Exact(4),
            Some("Frank".into()),
            Judgment::Innocent,
        ),
    );
}

#[test]
fn janet_2026_01_13() {
    sentence(
        "exactly 2 of Stella's 6 innocent neighbors also neighbor Gabe",
        &Sentence::UnitAndIntersectionSize {
            total: 6,
            quantified: Unit::neighbor("Stella"),
            other: Unit::neighbor("Gabe"),
            intersection: 2,
            judgment: Judgment::Innocent,
        },
    );
}

#[test]
fn xena_2026_01_15() {
    sentence(
        "Vince is one of 3 innocents in the corners",
        &Sentence::IsOneOfNInUnit(
            Unit::Corners,
            "Vince".into(),
            Cardinal::Exact(3),
            Judgment::Innocent,
        ),
    );
}

#[test]
fn salil_2026_01_15() {
    sentence(
        "No one in row 4 has more than 2 criminal neighbors",
        &Sentence::EachInUnitHasAtMostNNeighbors(Row::Four.into(), 2, Judgment::Criminal),
    );
}

#[test]
fn uma_2026_01_31() {
    sentence(
        "2 out of the 3 teachers have a criminal directly below them",
        &Sentence::UnitSize(
            Unit::profession("teacher")
                .quantify(3)
                .shift(Direction::Below)
                .with_judgment(Judgment::Criminal),
            Cardinal::Exact(2),
        ),
    );
}

#[test]
fn zara_2026_01_31() {
    sentence(
        "Everyone has at least one innocent neighbor",
        &Sentence::EachUnitInSeriesHasSize(
            Series::Neighbor,
            Cardinal::AtLeast(1),
            Judgment::Innocent,
        ),
    );
}

#[test]
fn katie_2026_02_03() {
    sentence(
        "Ryan and I have no innocent neighbors in common",
        &Sentence::IntersectionSize(
            [Unit::neighbor("Ryan"), Unit::Neighbor(Name::Me)],
            Cardinal::Exact(0),
            Judgment::Innocent,
        ),
    );
}

#[test]
fn uma_2026_02_03() {
    sentence(
        "exactly 1 judge has an innocent directly above them",
        &Sentence::UnitSize(
            Unit::profession("judge")
                .shift(Direction::Above)
                .with_judgment(Judgment::Innocent),
            Cardinal::Exact(1),
        ),
    );
}

#[test]
fn salil_2026_02_04() {
    sentence(
        "There are as many innocent builders as there are innocent guards",
        &Sentence::EqualNumberOfTraitsInUnits(
            ["builder", "guard"].map(Unit::profession),
            Judgment::Innocent,
        ),
    );
}

#[test]
fn alice_2026_02_05() {
    sentence(
        "Tina is one of 3 criminals in row\u{A0}4",
        &Sentence::IsOneOfNInUnit(
            Row::Four.into(),
            "Tina".into(),
            Cardinal::Exact(3),
            Judgment::Criminal,
        ),
    );
}

#[test]
fn chuck_2026_02_05() {
    sentence(
        "exactly 2 of the 4 innocents neighboring Gary are in row\u{a0}1",
        &Sentence::UnitAndIntersectionSize {
            total: 4,
            quantified: Unit::neighbor("Gary"),
            other: Row::One.into(),
            intersection: 2,
            judgment: Judgment::Innocent,
        },
    );
}

#[test]
fn ethan_2026_02_05() {
    sentence(
        "an odd number of innocents on the edges neighbor Gary",
        &Sentence::IntersectionSize(
            [Unit::Edges, Unit::neighbor("Gary")],
            Parity::Odd.into(),
            Judgment::Innocent,
        ),
    );
}

#[test]
fn gary_2026_02_05() {
    sentence(
        "exactly 1 innocent in row\u{a0}4 is neighboring Xavi",
        &Sentence::IntersectionSize(
            [Row::Four.into(), Unit::neighbor("Xavi")],
            Cardinal::Exact(1),
            Judgment::Innocent,
        ),
    );
}

#[test]
fn ike_2026_02_05() {
    sentence(
        "Xavi has exactly 3 innocent neighbors",
        &Sentence::UnitSize(
            Unit::neighbor("Xavi").with_judgment(Judgment::Innocent),
            Cardinal::Exact(3),
        ),
    );
}

#[test]
fn kyle_2026_02_05() {
    sentence(
        "an odd number of innocents above Zara neighbor Gary",
        &Sentence::IntersectionSize(
            [
                Unit::direction(Direction::Above, "Zara"),
                Unit::neighbor("Gary"),
            ],
            Parity::Odd.into(),
            Judgment::Innocent,
        ),
    );
}

#[test]
fn tina_2026_02_05() {
    sentence(
        "both criminals above Xavi are connected",
        &Sentence::UnitIsConnected(
            Unit::direction(Direction::Above, "Xavi")
                .with_judgment(Judgment::Criminal)
                .quantify(2),
        ),
    );
}

#[test]
fn vera_2026_02_05() {
    sentence(
        "Each column has at least 3 innocents",
        &Sentence::EachUnitInSeriesHasSize(
            LineKind::Column.into(),
            Cardinal::AtLeast(3),
            Judgment::Innocent,
        ),
    );
}

#[test]
fn freya_2026_02_06() {
    sentence(
        "only one of us 2 singers has exactly 2 criminal neighbors",
        &Sentence::UniqueInUnitHasNNeighbors(
            Unit::profession("singer").quantify(2),
            Cardinal::Exact(2),
            None,
            Judgment::Criminal,
        ),
    );
}

#[test]
fn helen_2026_02_06() {
    sentence(
        "Jason is one of Ellie's 4 innocent neighbors",
        &Sentence::IsOneOfNInUnit(
            Unit::neighbor("Ellie"),
            "Jason".into(),
            Cardinal::Exact(4),
            Judgment::Innocent,
        ),
    );
}

#[test]
fn jason_2026_02_06() {
    sentence(
        "Ellie and Noah have only one innocent neighbor in common",
        &Sentence::IntersectionSize(
            ["Ellie", "Noah"].map(Name::from).map(Unit::Neighbor),
            Cardinal::Exact(1),
            Judgment::Innocent,
        ),
    );
}

#[test]
fn logan_2026_02_06() {
    sentence(
        "exactly 1 farmer has a criminal directly above them",
        &Sentence::UnitSize(
            Unit::profession("farmer")
                .shift(Direction::Above)
                .with_judgment(Judgment::Criminal),
            Cardinal::Exact(1),
        ),
    );
}

#[test]
fn ivan_2026_02_06() {
    sentence(
        "row 5 has more innocents than any other row",
        &Sentence::BiggestInSeries(Row::Five.into(), Judgment::Innocent),
    );
}

#[test]
fn scott_2026_02_06() {
    sentence(
        "There are exactly 2 innocents to the left of Noah",
        &Sentence::UnitSize(
            Unit::direction(Direction::Left, "Noah").with_judgment(Judgment::Innocent),
            Cardinal::Exact(2),
        ),
    );
}

#[test]
fn vera_2026_02_06() {
    sentence(
        "There are more innocent cooks than innocent mechs",
        &Sentence::UnitBiggerThanUnit {
            big: Unit::profession("cook").with_judgment(Judgment::Innocent),
            small: Unit::profession("mech").with_judgment(Judgment::Innocent),
            excess: None,
        },
    );
}

#[test]
fn gary_2026_02_07() {
    sentence(
        "only 1 of the 2 innocents in column\u{a0}C is Zara's neighbor",
        &Sentence::UnitAndIntersectionSize {
            total: 2,
            quantified: Column::C.into(),
            other: Unit::neighbor("Zara"),
            intersection: 1,
            judgment: Judgment::Innocent,
        },
    );
}

#[test]
fn uma_2026_02_07() {
    sentence(
        "only 1 of the 3 innocents neighboring me is to the right of Kay",
        &Sentence::UnitAndIntersectionSize {
            total: 3,
            quantified: Unit::Neighbor(Name::Me),
            other: Unit::direction(Direction::Right, "Kay"),
            intersection: 1,
            judgment: Judgment::Innocent,
        },
    );
}

#[test]
fn xena_2026_02_08() {
    sentence(
        "There are no innocents in row 1 who neighbor Donna",
        &Sentence::IntersectionSize(
            [Row::One.into(), Unit::neighbor("Donna")],
            Cardinal::Exact(0),
            Judgment::Innocent,
        ),
    );
}

#[test]
fn hank_2026_02_08() {
    sentence(
        "Only one person in a corner has exactly 2 innocent neighbors",
        &Sentence::UniqueInUnitHasNNeighbors(
            Unit::Corners,
            Cardinal::Exact(2),
            None,
            Judgment::Innocent,
        ),
    );
}

#[test]
fn tina_2026_02_09() {
    sentence(
        "exactly 2 innocents in column C are neighboring me",
        &Sentence::IntersectionSize(
            [Column::C.into(), Unit::Neighbor(Name::Me)],
            Cardinal::Exact(2),
            Judgment::Innocent,
        ),
    );
}

#[test]
fn kumar_2026_02_09() {
    sentence(
        "exactly 2 of the 3 innocents in row 5 are Susan's neighbors",
        &Sentence::UnitAndIntersectionSize {
            total: 3,
            quantified: Row::Five.into(),
            other: Unit::neighbor("Susan"),
            intersection: 2,
            judgment: Judgment::Innocent,
        },
    );
}

#[test]
fn xavi_2026_02_09() {
    sentence(
        "There are more innocents in row 3 than row 5",
        &Sentence::UnitBiggerThanUnit {
            big: Unit::from(Row::Three).with_judgment(Judgment::Innocent),
            small: Unit::from(Row::Five).with_judgment(Judgment::Innocent),
            excess: None,
        },
    );
}

#[test]
fn ollie_2026_02_09() {
    sentence(
        "There's an equal number of innocents in rows 2 and 3",
        &Sentence::EqualNumberOfTraitsInUnits(
            [Row::Two, Row::Three].map(Unit::from),
            Judgment::Innocent,
        ),
    );
}

#[test]
fn gabe_2026_02_09() {
    sentence(
        "There are at least 10 innocents on the edges",
        &Sentence::UnitSize(
            Unit::Edges.with_judgment(Judgment::Innocent),
            Cardinal::AtLeast(10),
        ),
    );
}

#[test]
fn gary_2026_02_10() {
    sentence(
        "Ryan is one of 2 innocents in between Betty and Vicky",
        &Sentence::IsOneOfNInUnit(
            Unit::Between(["Betty", "Vicky"].map(Name::from)),
            "Ryan".into(),
            Cardinal::Exact(2),
            Judgment::Innocent,
        ),
    );
}

#[test]
fn lisa_2026_02_10() {
    sentence(
        "exactly 1 innocent on the edges is a farmer",
        &Sentence::IntersectionSize(
            [Unit::Edges, Unit::profession("farmer")],
            Cardinal::Exact(1),
            Judgment::Innocent,
        ),
    );
}

#[test]
fn will_2026_02_10() {
    sentence(
        "2 of us 3 singers have an innocent directly to the left of us",
        &Sentence::UnitSize(
            Unit::profession("singer")
                .quantify(3)
                .shift(Direction::Left)
                .with_judgment(Judgment::Innocent),
            Cardinal::Exact(2),
        ),
    );
}

#[test]
fn janet_2026_02_10() {
    sentence(
        "There are 9 innocents in total",
        &Sentence::UnitSize(
            Unit::All.with_judgment(Judgment::Innocent),
            Cardinal::Exact(9),
        ),
    );
}

#[test]
fn noah_2026_02_11() {
    sentence(
        "Olof is a criminal",
        &Sentence::HasTrait("Olof".into(), Judgment::Criminal),
    );
}

#[test]
fn ollie_2026_02_12() {
    sentence(
        "There's an odd number of innocents neighboring Celia in row 2",
        &Sentence::IntersectionSize(
            [Unit::neighbor("Celia"), Row::Two.into()],
            Parity::Odd.into(),
            Judgment::Innocent,
        ),
    );
}

#[test]
fn olive_2026_02_13() {
    sentence(
        "2 of my neighbors on the edges are innocent",
        &Sentence::IntersectionSize(
            [Unit::Neighbor(Name::Me), Unit::Edges],
            Cardinal::Exact(2),
            Judgment::Innocent,
        ),
    );
}

#[test]
fn paul_2026_03_23() {
    sentence(
        "Freya has one more innocent neighbor than Olof",
        &Sentence::UnitBiggerThanUnit {
            big: Unit::neighbor("Freya").with_judgment(Judgment::Innocent),
            small: Unit::neighbor("Olof").with_judgment(Judgment::Innocent),
            excess: Some(1),
        },
    );
}

#[test]
fn vince_2026_03_24() {
    sentence(
        "There are more innocent judges than us innocent singers",
        &Sentence::UnitBiggerThanUnit {
            big: Unit::profession("judge").with_judgment(Judgment::Innocent),
            small: Unit::profession("singer").with_judgment(Judgment::Innocent),
            excess: None,
        },
    );
}

#[test]
fn nicole_2026_03_24() {
    sentence(
        "an odd number of us innocents on the edges neighbor Igor",
        &Sentence::IntersectionSize(
            [Unit::Edges, Unit::neighbor("Igor")],
            Parity::Odd.into(),
            Judgment::Innocent,
        ),
    );
}

#[test]
fn flora_2026_03_24() {
    sentence(
        "Vince is the only person in a corner with one criminal neighbor",
        &Sentence::UniqueInUnitHasNNeighbors(
            Unit::Corners,
            Cardinal::Exact(1),
            Some("Vince".into()),
            Judgment::Criminal,
        ),
    );
}

#[test]
fn flora_2026_04_05() {
    sentence(
        "2 of the 3 guards have 3 innocent neighbors",
        &Sentence::NInUnitHaveNNeighbors {
            unit: Unit::profession("guard").quantify(3),
            quantity: Cardinal::Exact(2),
            neighbors: Cardinal::Exact(3),
            judgment: Judgment::Innocent,
        },
    );
}

#[test]
fn sofia_2026_04_11() {
    sentence(
        "one innocent in column A has an innocent directly to the right of them",
        &Sentence::UnitSize(
            Unit::from(Column::A)
                .with_judgment(Judgment::Innocent)
                .shift(Direction::Right)
                .with_judgment(Judgment::Innocent),
            Cardinal::Exact(1),
        ),
    );
}

#[test]
fn xavi_2026_04_11() {
    sentence(
        "2 persons in column D have an innocent directly above them",
        &Sentence::UnitSize(
            Unit::from(Column::D)
                .shift(Direction::Above)
                .with_judgment(Judgment::Innocent),
            Cardinal::Exact(2),
        ),
    );
}

#[test]
fn janet_2026_04_13() {
    sentence(
        "There are exactly 3 criminals in my column",
        &Sentence::UnitSize(
            Unit::from(ColumnRecipe::Me).with_judgment(Judgment::Criminal),
            Cardinal::Exact(3),
        ),
    );
}

#[test]
fn bobby_2026_04_14() {
    sentence(
        "exactly 1 of us 3 mechs has an innocent directly below them",
        &Sentence::UnitSize(
            Unit::profession("mech")
                .quantify(3)
                .shift(Direction::Below)
                .with_judgment(Judgment::Innocent),
            Cardinal::Exact(1),
        ),
    );
}

#[test]
fn martin_2026_04_14() {
    sentence(
        "There are as many innocent guards as there are us innocent sleuths",
        &Sentence::EqualNumberOfTraitsInUnits(
            [Unit::profession("guard"), Unit::profession("sleuth")],
            Judgment::Innocent,
        ),
    );
}

#[test]
fn umar_2026_04_25() {
    sentence(
        "one innocent in the corners has a criminal directly to the right of them",
        &Sentence::UnitSize(
            Unit::Corners
                .with_judgment(Judgment::Innocent)
                .shift(Direction::Right)
                .with_judgment(Judgment::Criminal),
            Cardinal::Exact(1),
        ),
    );
}

#[test]
fn xena_2026_05_12() {
    sentence(
        "There is at least one innocent in each profession",
        &Sentence::EachUnitInSeriesHasSize(
            Series::Profession,
            Cardinal::AtLeast(1),
            Judgment::Innocent,
        ),
    );
}

#[test]
fn nancy_2026_06_23() {
    sentence(
        "There's an equal number of criminals above Zara and above Vince",
        &Sentence::EqualNumberOfTraitsInUnits(
            [
                Unit::direction(Direction::Above, "Zara"),
                Unit::direction(Direction::Above, "Vince"),
            ],
            Judgment::Criminal,
        ),
    );
}

#[test]
fn mary_2026_06_24() {
    sentence(
        "only 1 of the 3 criminals in row 1 doesn't neighbor Emily",
        &Sentence::UnitAndIntersectionSize {
            total: 3,
            quantified: Row::One.into(),
            other: Unit::not_neighbor("Emily"),
            intersection: 1,
            judgment: Judgment::Criminal,
        },
    );

    sentence(
        "only 1 of the 3 criminals in row 1 neighbors Emily",
        &Sentence::UnitAndIntersectionSize {
            total: 3,
            quantified: Row::One.into(),
            other: Unit::neighbor("Emily"),
            intersection: 1,
            judgment: Judgment::Criminal,
        },
    );
}

#[test]
fn xia_2026_07_06() {
    sentence(
        "Vicky is the only innocent clerk",
        &Sentence::IsOneOfNInUnit(
            Unit::profession("clerk"),
            "Vicky".into(),
            Cardinal::Exact(1),
            Judgment::Innocent,
        ),
    );
}

#[test]
fn terry_2026_07_21() {
    sentence(
        "I'm one of 2 innocents to the right of Quita",
        &Sentence::IsOneOfNInUnit(
            Unit::Direction(Direction::Right, "Quita".into()),
            Name::Me,
            Cardinal::Exact(2),
            Judgment::Innocent,
        ),
    );
}

#[test]
fn kumar_2026_08_04() {
    sentence(
        "There is at least one criminal among each profession",
        &Sentence::EachUnitInSeriesHasSize(
            Series::Profession,
            Cardinal::AtLeast(1),
            Judgment::Criminal,
        ),
    );
}

#[test]
fn zed_2026_08_07() {
    sentence(
        "There's an equal number of innocent and criminal cops",
        &Sentence::UnitEquallySplit(Unit::profession("cop")),
    );
}

#[test]
fn diane_0cf47() {
    sentence(
        "Xavi has more criminal neighbors than Ben",
        &Sentence::UnitBiggerThanUnit {
            big: Unit::neighbor("Xavi").with_judgment(Judgment::Criminal),
            small: Unit::neighbor("Ben").with_judgment(Judgment::Criminal),
            excess: None,
        },
    );
}

#[test]
fn hal_0cf47() {
    sentence(
        "Emily and Tom have an equal number of criminal neighbors",
        &Sentence::EqualNumberOfTraitsInUnits(
            ["Emily", "Tom"].map(Name::from).map(Unit::Neighbor),
            Judgment::Criminal,
        ),
    );
}

#[test]
fn paul_0cf47() {
    sentence(
        "There are more criminals among guards than any other profession",
        &Sentence::BiggestInSeries(UnitInSeries::profession("guard"), Judgment::Criminal),
    );
}

#[test]
fn rob_0cf47() {
    sentence(
        "There are more criminals than innocents in a corner",
        &Sentence::MoreTraitsInUnit(Unit::Corners, Judgment::Criminal),
    );
}

#[test]
fn vicky_0cf47() {
    sentence(
        "Paul has the most criminal neighbors",
        &Sentence::BiggestInSeries(UnitInSeries::neighbor("Paul"), Judgment::Criminal),
    );
}

#[test]
fn jose_879da349c27d() {
    sentence(
        "I have exactly 5 innocent neighbors",
        &Sentence::UnitSize(
            Unit::Neighbor(Name::Me).with_judgment(Judgment::Innocent),
            Cardinal::Exact(5),
        ),
    );
}

#[test]
fn ryan_327a79cc5a8c() {
    sentence(
        "Zoe is the only one with exactly 1 criminal neighbors",
        &Sentence::OnlyGivenUnitHasNTraits(
            UnitInSeries::neighbor("Zoe"),
            Cardinal::Exact(1),
            Judgment::Criminal,
        ),
    );
}

#[test]
fn gary_dd0a4616a658() {
    sentence(
        "Nancy has only one innocent neighbor on the edges",
        &Sentence::IntersectionSize(
            [Unit::neighbor("Nancy"), (Unit::Edges)],
            Cardinal::Exact(1),
            Judgment::Innocent,
        ),
    );
}

#[test]
fn olga_d9b7f6418e96() {
    sentence(
        "2 of Gus' neighbors on the edges are innocent",
        &Sentence::IntersectionSize(
            [Unit::neighbor("Gus"), Unit::Edges],
            Cardinal::Exact(2),
            Judgment::Innocent,
        ),
    );
}

#[test]
fn julie_puzzle_pack_1_1() {
    sentence(
        "Terry is one of two or more innocents on the edges",
        &Sentence::IsOneOfNInUnit(
            Unit::Edges,
            "Terry".into(),
            Cardinal::AtLeast(2),
            Judgment::Innocent,
        ),
    );
}

#[test]
fn olof_puzzle_pack_1_1() {
    sentence(
        "the only criminal below Julie is Terry's neighbor",
        &Sentence::UnitAndIntersectionSize {
            total: 1,
            quantified: Unit::direction(Direction::Below, "Julie"),
            other: Unit::neighbor("Terry"),
            intersection: 1,
            judgment: Judgment::Criminal,
        },
    );
}

#[test]
fn flora_puzzle_pack_1_2() {
    sentence(
        "Nicole's only innocent neighbor is Martin's neighbor",
        &Sentence::UnitAndIntersectionSize {
            total: 1,
            quantified: Unit::neighbor("Nicole"),
            other: Unit::neighbor("Martin"),
            intersection: 1,
            judgment: Judgment::Innocent,
        },
    );
}

#[test]
fn xia_puzzle_pack_1_2() {
    sentence(
        "I have more innocent neighbors than Olivia",
        &Sentence::UnitBiggerThanUnit {
            big: Unit::Neighbor(Name::Me).with_judgment(Judgment::Innocent),
            small: Unit::neighbor("Olivia").with_judgment(Judgment::Innocent),
            excess: None,
        },
    );
}

#[test]
fn flora_puzzle_pack_1_3() {
    sentence(
        "Emily and I share an odd number of innocent neighbors",
        &Sentence::IntersectionSize(
            [Unit::neighbor("Emily"), Unit::Neighbor(Name::Me)],
            Parity::Odd.into(),
            Judgment::Innocent,
        ),
    );
}

#[test]
fn frank_puzzle_pack_1_5() {
    sentence(
        "Alice's only innocent neighbor is to the left of Helen",
        &Sentence::UnitAndIntersectionSize {
            total: 1,
            quantified: Unit::neighbor("Alice"),
            other: Unit::direction(Direction::Left, "Helen"),
            intersection: 1,
            judgment: Judgment::Innocent,
        },
    );
}

#[test]
fn katie_puzzle_pack_1_6() {
    sentence(
        "both innocents in row 4 are Laura's neighbors",
        &Sentence::UnitAndIntersectionSize {
            total: 2,
            quantified: Row::Four.into(),
            other: Unit::neighbor("Laura"),
            intersection: 2,
            judgment: Judgment::Innocent,
        },
    );
}

#[test]
fn zara_puzzle_pack_1_6() {
    sentence(
        "neither of the 2 innocents neighboring Olive are to the left of Noah",
        &Sentence::UnitAndIntersectionSize {
            total: 2,
            quantified: Unit::neighbor("Olive"),
            other: Unit::direction(Direction::Left, "Noah"),
            intersection: 0,
            judgment: Judgment::Innocent,
        },
    );
}

#[test]
fn bonnie_puzzle_pack_1_13() {
    sentence(
        "none of the 7 criminals on the edges is a painter",
        &Sentence::UnitAndIntersectionSize {
            total: 7,
            quantified: Unit::Edges,
            other: Unit::profession("painter"),
            intersection: 0,
            judgment: Judgment::Criminal,
        },
    );
}

#[test]
fn mary_puzzle_pack_1_14() {
    sentence(
        "Zach has more criminal than innocent neighbors",
        &Sentence::MoreTraitsInUnit(Unit::neighbor("Zach"), Judgment::Criminal),
    );
}

#[test]
fn tom_puzzle_pack_1_15() {
    sentence(
        "There are as many criminals as innocents below Linda",
        &Sentence::UnitEquallySplit(Unit::direction(Direction::Below, "Linda")),
    );
}

#[test]
fn zoe_puzzle_pack_1_15() {
    sentence(
        "Linda and Tom have 4 innocent neighbors in total",
        &Sentence::TotalUnitsSize(
            ["Linda", "Tom"].map(Unit::neighbor),
            Cardinal::Exact(4),
            Judgment::Innocent,
        ),
    );
}

#[test]
fn zoe_puzzle_pack_1_16() {
    sentence(
        "There are more criminal than innocent guards",
        &Sentence::MoreTraitsInUnit(Unit::profession("guard"), Judgment::Criminal),
    );
}

#[test]
fn will_puzzle_pack_1_17() {
    sentence(
        "I am one of Xia's 4 criminal neighbors",
        &Sentence::IsOneOfNInUnit(
            Unit::neighbor("Xia"),
            Name::Me,
            Cardinal::Exact(4),
            Judgment::Criminal,
        ),
    );
}

#[test]
fn cheryl_puzzle_pack_1_27() {
    sentence(
        "2 of Isaac's innocent neighbors are in row&nbsp;3",
        &Sentence::IntersectionSize(
            [Unit::neighbor("Isaac"), Row::Three.into()],
            Cardinal::Exact(2),
            Judgment::Innocent,
        ),
    );
}

#[test]
fn freya_puzzle_pack_1_45() {
    sentence(
        "exactly 1 innocent neighboring Wally is builder",
        &Sentence::IntersectionSize(
            [Unit::neighbor("Wally"), Unit::profession("builder")],
            Cardinal::Exact(1),
            Judgment::Innocent,
        ),
    );
}

#[test]
fn eve_puzzle_pack_1_49() {
    sentence(
        "Katie shares neither of her 2 innocent neighbors with Laura",
        &Sentence::UnitAndIntersectionSize {
            total: 2,
            quantified: Unit::neighbor("Katie"),
            other: Unit::neighbor("Laura"),
            intersection: 0,
            judgment: Judgment::Innocent,
        },
    );
}

#[test]
fn debra_community_49f3f1_9eb600102931a676() {
    sentence(
        "No one in row 1 has more than one criminal neighbor",
        &Sentence::EachInUnitHasAtMostNNeighbors(Row::One.into(), 1, Judgment::Criminal),
    );
}

#[test]
fn linda_community_6eebae_909beebb44a88201() {
    sentence(
        "All Noah's criminal neighbors are connected",
        &Sentence::UnitIsConnected(Unit::neighbor("Noah").with_judgment(Judgment::Criminal)),
    );
}

#[test]
fn helen_community_6eebae_d5d5560b65d3f7ba() {
    sentence(
        "builder is the only profession with exactly one innocent",
        &Sentence::OnlyGivenUnitHasNTraits(
            UnitInSeries::profession("builder"),
            Cardinal::Exact(1),
            Judgment::Innocent,
        ),
    );
}

#[test]
fn flora_community_6eebae_d5d5560b65d3f7ba() {
    sentence(
        "Only one person has exactly 6 innocent neighbors",
        &Sentence::UniqueUnitInSeriesHasSize(
            Series::Neighbor,
            Cardinal::Exact(6),
            Judgment::Innocent,
        ),
    );
}

fn sentence(input: &str, sentence: &Sentence) {
    let input = input.split(' ').filter(|s| !s.is_empty()).collect_vec();
    parser(&Sentence::any, &input, sentence);
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
