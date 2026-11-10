use crate::models::{Judgment, SetEval, Solution};
use crate::solver::board::coordinates::{Set, Set1};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct Judged<T>(pub T, pub Judgment);

impl<T> Judged<T> {
    pub(crate) fn flip(self) -> Self {
        let Self(inner, judgment) = self;
        Self(inner, !judgment)
    }
}

impl Judged<Set> {
    pub(crate) fn non_empty(self) -> Option<Judged<Set1>> {
        let Self(set, judgment) = self;
        set.non_empty().map(|set| Judged(set, judgment))
    }
}

impl SetEval for Judged<Set1> {
    fn eval(&self, solution: &Solution) -> Set {
        let &Self(inner, judgment) = self;
        inner
            .into_iter()
            .filter(move |&coord| solution[coord] == judgment)
            .collect()
    }
}

impl SetEval for Judged<Set> {
    fn eval(&self, solution: &Solution) -> Set {
        let &Self(inner, judgment) = self;
        inner
            .into_iter()
            .filter(move |&coord| solution[coord] == judgment)
            .collect()
    }
}
