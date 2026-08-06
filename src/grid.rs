use std::ops::{Index, IndexMut};

use linearize::StaticMap;
use serde::{Deserialize, Serialize};

use crate::models::{Column, Coord, Row};

// TODO custom Debug
#[derive(Clone, Debug, Deserialize, Default, PartialEq, Eq)]
#[serde(from = "[[T; 4]; 5]")]
pub(crate) struct Grid<T> {
    inner: StaticMap<Row, StaticMap<Column, T>>,
}

impl<T> Grid<T> {
    pub(crate) fn filled(value: T) -> Self
    where
        T: Clone,
    {
        Self::from_fn(|_| value.clone())
    }

    pub(crate) fn map<U>(self, mut f: impl FnMut(T) -> U) -> Grid<U> {
        Grid {
            inner: self.inner.map(|_, row| row.map(|_, val| f(val))),
        }
    }

    pub(crate) fn from_fn(mut f: impl FnMut(Coord) -> T) -> Self {
        Self {
            inner: StaticMap::from_fn(|row| StaticMap::from_fn(|col| f(Coord { row, col }))),
        }
    }

    pub(crate) fn from_flattened(array: [T; 20]) -> Self {
        // This is probably optimized away, but potentially want to use unsafe code here
        let [
            a1,
            b1,
            c1,
            d1,
            a2,
            b2,
            c2,
            d2,
            a3,
            b3,
            c3,
            d3,
            a4,
            b4,
            c4,
            d4,
            a5,
            b5,
            c5,
            d5,
        ] = array;

        [
            [a1, b1, c1, d1],
            [a2, b2, c2, d2],
            [a3, b3, c3, d3],
            [a4, b4, c4, d4],
            [a5, b5, c5, d5],
        ]
        .into()
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = &T> {
        self.inner.values().flat_map(|row| row.values())
    }

    pub(crate) fn into_iter(self) -> impl Iterator<Item = T> {
        self.inner.into_values().flat_map(StaticMap::into_values)
    }

    pub(crate) fn each_ref(&self) -> Grid<&T> {
        Grid {
            inner: self
                .inner
                .each_ref()
                .into_static_map()
                .map(|_, row| row.each_ref().into()),
        }
    }

    pub(crate) fn rows(&self) -> impl Iterator<Item = &[T; 4]> {
        self.inner.each_ref().into_values().map(|row| &row.0)
    }
}

impl<T> Index<Coord> for Grid<T> {
    type Output = T;

    fn index(&self, index: Coord) -> &Self::Output {
        &self.inner[index.row][index.col]
    }
}

impl<T> IndexMut<Coord> for Grid<T> {
    fn index_mut(&mut self, index: Coord) -> &mut Self::Output {
        &mut self.inner[index.row][index.col]
    }
}

impl<T> From<[[T; 4]; 5]> for Grid<T> {
    fn from(value: [[T; 4]; 5]) -> Self {
        Self {
            inner: StaticMap(value.map(StaticMap)),
        }
    }
}

impl<T> From<Grid<T>> for [[T; 4]; 5] {
    fn from(grid: Grid<T>) -> Self {
        grid.inner.0.map(|row| row.0)
    }
}

impl<'grid, T> From<&'grid Grid<T>> for [[&'grid T; 4]; 5] {
    fn from(grid: &'grid Grid<T>) -> Self {
        grid.inner.0.each_ref().map(|row| row.0.each_ref())
    }
}

impl<T: Serialize> Serialize for Grid<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        <[[&T; 4]; 5]>::from(self).serialize(serializer)
    }
}
