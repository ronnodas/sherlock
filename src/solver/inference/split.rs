use linearize::StaticCopyMap;

use crate::grid::Grid;
use crate::models::Judgment;
use crate::solver::board::coordinates::{Set, Set1, SetExpr, SetExpr1, SetOp, SetOp1};

pub(crate) enum SetSimplify {
    Fixed(Set1),
    Variable(SetOp1),
    Split(SetOp1, Set1),
    Empty,
}

impl SetSimplify {
    pub(crate) fn split(set: &SetOp1, known: &Grid<Option<Judgment>>) -> Option<Self> {
        match set {
            SetOp1::Judged(inner, judgment) => {
                let (inner_variable, inner_fixed) = match inner.as_ref() {
                    &SetExpr1::Regular(set) => (SetOp::Empty, set),
                    SetExpr1::Op(inner) => match Self::split(inner, known) {
                        None => return None,
                        Some(Self::Fixed(fixed)) => (SetOp::Empty, fixed),
                        Some(Self::Variable(variable)) => {
                            return Some(Self::Variable(variable));
                        }
                        Some(Self::Empty) => return Some(Self::Empty),
                        Some(Self::Split(variable, fixed)) => (variable.into(), fixed),
                    },
                };

                let split = ByJudgment::split(inner_fixed, known);
                if split.variable == Set::from(inner_fixed) {
                    return None;
                }
                let [outer_variable, fixed] = split.only(*judgment);
                // drop(inner_fixed);
                let variable = match (outer_variable.non_empty(), inner_variable) {
                    (None, SetOp::Empty) => SetOp::Empty,
                    (None, SetOp::NonEmpty(inner_variable)) => inner_variable.judged(*judgment),
                    (Some(outer_variable), SetOp::Empty) => outer_variable.judged(*judgment).into(),
                    (Some(_), SetOp::NonEmpty(_)) => {
                        split.judgment(!*judgment).complement().non_empty().map_or(
                            SetOp::Empty,
                            |complement| match inner.as_ref().clone().intersect(complement.into()) {
                                SetExpr::Empty => SetOp::Empty,
                                SetExpr::NonEmpty(SetExpr1::Regular(_)) => todo!(),
                                SetExpr::NonEmpty(SetExpr1::Op(set)) => set.into(),
                            },
                        )
                    }
                };
                let split = match (fixed.non_empty(), variable) {
                    (None, SetOp::Empty) => Self::Empty,
                    (None, SetOp::NonEmpty(variable)) => Self::Variable(variable),
                    (Some(fixed), SetOp::Empty) => Self::Fixed(fixed),
                    (Some(fixed), SetOp::NonEmpty(variable)) => Self::Split(variable, fixed),
                };
                Some(split)
            }
            SetOp1::Shift(_, _) => todo!(),
            SetOp1::Intersection(_, _) => todo!(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
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

    fn judgment(self, judgment: Judgment) -> Set {
        self.fixed[judgment]
    }
}
