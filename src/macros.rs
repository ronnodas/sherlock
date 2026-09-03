macro_rules! coord {
    ($c:ident $r:tt) => {
        $crate::models::Coord {
            row: $crate::macros::row!($r),
            col: $crate::macros::col!($c),
        }
    };
}

macro_rules! row {
    (1) => {
        $crate::models::Row::One
    };
    (2) => {
        $crate::models::Row::Two
    };
    (3) => {
        $crate::models::Row::Three
    };
    (4) => {
        $crate::models::Row::Four
    };
    (5) => {
        $crate::models::Row::Five
    };
}

macro_rules! col {
    (A) => {
        $crate::models::Column::A
    };
    (B) => {
        $crate::models::Column::B
    };
    (C) => {
        $crate::models::Column::C
    };
    (D) => {
        $crate::models::Column::D
    };
}

macro_rules! set1 {
    ($c:tt $r:tt) => {
        Set1::from_one($crate::macros::coord!($c $r))
    };

    // Recursive / iterative case:
    // Matches the first pair, followed by `|`, and then a repetition of remaining pairs
    ($c:tt $r:tt | $($rest_c:tt $rest_r:tt)|+) => {
        set1!($c $r) $(| $crate::macros::coord!($rest_c $rest_r))+
    };}

pub(crate) use col;
pub(crate) use coord;
pub(crate) use row;
pub(crate) use set1;
