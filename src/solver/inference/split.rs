use linearize::StaticCopyMap;

use crate::grid::Grid;
use crate::models::Judgment;
use crate::solver::board::coordinates::{Set, Set1};

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

    /// returns `[variable, fixed]` where `fixed` is the subset with given judgment
    pub(crate) fn only(self, judgment: Judgment) -> [Set; 2] {
        [self.variable, self.fixed[judgment]]
    }
}
