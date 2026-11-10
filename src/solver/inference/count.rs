use std::cmp::Ordering;
use std::num::NonZero;
use std::ops::BitAnd;
use std::range::RangeInclusive;

use itertools::Itertools as _;
use mitsein::iter1::{IntoIterator1, Iterator1};

use crate::solver::board::coordinates::Set1;
use crate::solver::hint::{Bound, BoundOrNot, Number, Parity};
use crate::solver::inference::{Contradiction, TriState};

pub(crate) fn reduce_bound_or_not(count: BoundOrNot, rhs: Number) -> Option<BoundOrNot> {
    let count = match count {
        BoundOrNot::Exact(count) => BoundOrNot::Exact(count.checked_sub(rhs)?),
        BoundOrNot::AtLeast(count) => BoundOrNot::AtLeast(count.saturating_sub(rhs)),
        BoundOrNot::AtMost(count) => BoundOrNot::AtMost(count.checked_sub(rhs)?),
        BoundOrNot::Parity(parity) => BoundOrNot::Parity(parity ^ Parity::of(rhs)),
        BoundOrNot::NotExact(count) => count
            .checked_sub(rhs)
            .map_or(BoundOrNot::AtLeast(0), BoundOrNot::NotExact),
    };
    Some(count)
}

pub(crate) fn reduce_bound(count: Bound, rhs: Number) -> Option<Bound> {
    let count = match count {
        Bound::Exact(count) => Bound::Exact(count.checked_sub(rhs)?),
        Bound::AtLeast(count) => Bound::AtLeast(count.saturating_sub(rhs)),
        Bound::AtMost(count) => Bound::AtMost(count.checked_sub(rhs)?),
        Bound::Parity(parity) => Bound::Parity(parity ^ Parity::of(rhs)),
    };
    Some(count)
}

// TODO use `RangeInclusive1` once `mitsein` updates
#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) enum CountSet {
    Exact(Number),
    Complex {
        range: RangeInclusive<Number>,
        parity: Option<Parity>,
    },
    BitSet(BitSet),
}

impl CountSet {
    // TODO return bool to mark updated
    pub(crate) fn intersect(&mut self, rhs: Self) -> Result<bool, Contradiction> {
        let value = (*self & rhs).ok_or(Contradiction)?;
        let updated = value != *self;
        *self = value;
        Ok(updated)
    }

    pub(crate) fn minimum(self) -> Number {
        match self {
            Self::Exact(value) => value,
            Self::Complex { range, .. } => range.start,
            Self::BitSet(bits) => bits.min(),
        }
    }

    pub(crate) fn maximum(self) -> Number {
        match self {
            Self::Exact(value) => value,
            Self::Complex { range, .. } => range.last,
            Self::BitSet(bits) => bits.into_iter1().max(),
        }
    }

    pub(crate) fn matches(self, len: Number) -> bool {
        match self {
            Self::Exact(exact) => exact == len,
            Self::Complex { range, parity } => {
                range.contains(&len) && parity.is_none_or(|parity| parity.matches(len))
            }
            Self::BitSet(bits) => bits.contains(len),
        }
    }

    pub(crate) fn match_bound(self, bound: Bound) -> TriState {
        match self {
            Self::Exact(value) => bound.matches(value).into(),
            Self::Complex { range, parity } => range
                .into_iter()
                .step_by(if parity.is_some() { 2 } else { 1 })
                .map(|value| TriState::from(bound.matches(value)))
                .reduce(TriState::union)
                .expect("range non-empty"),
            Self::BitSet(bits) => bits
                .into_iter1()
                .map(|value| TriState::from(bound.matches(value)))
                .reduce(TriState::union),
        }
    }

    pub(crate) fn simplify(self) -> Option<Self> {
        match self {
            Self::Exact(value) => Some(Self::Exact(value)),
            Self::Complex { mut range, parity } => {
                if let Some(parity) = parity {
                    if !parity.matches(range.start) {
                        range.start = range.start.strict_add(1);
                    }
                    if !parity.matches(range.last) {
                        range.last = range.last.checked_sub(1)?;
                    }
                }

                match range.start.cmp(&range.last) {
                    Ordering::Less => Some(Self::Complex { range, parity }),
                    Ordering::Equal => Some(Self::Exact(range.start)),
                    Ordering::Greater => None,
                }
            }
            Self::BitSet(bits) => Some(bits.into()),
        }
    }

    pub(crate) fn checked_sub(self, rhs: impl Into<Self>) -> Option<Self> {
        match [self, rhs.into()] {
            [Self::Exact(exact), Self::Exact(rhs)] => Some(Self::Exact(exact.checked_sub(rhs)?)),
            [Self::Complex { range, parity }, Self::Exact(rhs)] => {
                let min = range.start.saturating_sub(rhs);
                let max = range.last.checked_sub(rhs)?;
                let parity = parity.map(|parity| parity ^ Parity::of(rhs));
                Self::complex(min, max, parity)
            }
            [Self::Exact(exact), Self::Complex { range, parity }] => {
                let min = exact.saturating_sub(range.last);
                let max = exact.checked_sub(range.start)?;
                let parity = parity.map(|parity| parity ^ Parity::of(exact));
                Self::complex(min, max, parity)
            }
            [
                Self::Complex { range, parity },
                Self::Complex {
                    range: rhs_range,
                    parity: rhs_parity,
                },
            ] => {
                let min = range.start.saturating_sub(rhs_range.last);
                let max = range.last.saturating_sub(rhs_range.start);
                // TODO Option::zip_with https://github.com/rust-lang/rust/issues/70086
                let parity = parity
                    .zip(rhs_parity)
                    .map(|(a, b)| if a == b { Parity::Even } else { Parity::Odd });
                Self::complex(min, max, parity)
            }
            [Self::Exact(exact), Self::BitSet(bits)] => {
                Self::bit_set(bits.into_iter().filter_map(|rhs| exact.checked_sub(rhs)))
            }
            [Self::Complex { range, parity }, Self::BitSet(bits)] => Self::bit_set(
                Self::iter_complex(range, parity)
                    .cartesian_product(bits)
                    .filter_map(|(a, b)| a.checked_sub(b)),
            ),
            [Self::BitSet(bits), Self::Exact(rhs)] => {
                Self::bit_set(bits.into_iter().filter_map(|value| value.checked_sub(rhs)))
            }
            [Self::BitSet(bits), Self::Complex { range, parity }] => Self::bit_set(
                bits.into_iter()
                    .cartesian_product(Self::iter_complex(range, parity))
                    .filter_map(|(a, b)| a.checked_sub(b)),
            ),
            [Self::BitSet(bits), Self::BitSet(rhs)] => Self::bit_set(
                bits.into_iter()
                    .cartesian_product(rhs)
                    .filter_map(|(a, b)| a.checked_sub(b)),
            ),
        }
    }

    pub(crate) fn complex(min: Number, max: Number, parity: Option<Parity>) -> Option<Self> {
        Self::Complex {
            range: (min..=max).into(),
            parity,
        }
        .simplify()
    }

    pub(crate) fn from_cardinal(cardinal: BoundOrNot, max: NonZero<Number>) -> Option<Self> {
        match cardinal {
            BoundOrNot::Exact(value) => (value <= max.get()).then_some(Self::Exact(value)),
            BoundOrNot::AtLeast(min) => Self::complex(min, max.get(), None),
            BoundOrNot::AtMost(value) => Some(Self::max(value.min(max.get()))),
            BoundOrNot::Parity(parity) => Some(Self::from_parity(parity, max)),
            BoundOrNot::NotExact(value) => {
                Self::bit_set((0..=max.get()).filter(|&len| len != value))
            }
        }
    }

    pub(crate) fn from_parity(parity: Parity, max: NonZero<Number>) -> Self {
        let min = match parity {
            Parity::Even => 0,
            Parity::Odd => 1,
        };
        let max = if parity.matches(max.get()) {
            max.get()
        } else {
            max.get().strict_sub(1)
        };

        if min == max {
            Self::Exact(min)
        } else {
            Self::Complex {
                range: (min..=max).into(),
                parity: Some(parity),
            }
        }
    }

    pub(crate) fn max(max: Number) -> Self {
        if max == 0 {
            Self::Exact(0)
        } else {
            Self::Complex {
                range: (0..=max).into(),
                parity: None,
            }
        }
    }

    /// Returns `true` if the count is [`Exact`].
    ///
    /// [`Exact`]: Count::Exact
    #[must_use]
    pub(crate) fn is_exact(self) -> bool {
        matches!(self, Self::Exact(..))
    }

    pub(crate) fn parity(self) -> Option<Parity> {
        match self {
            Self::Exact(exact) => Some(Parity::of(exact)),
            Self::Complex { parity, .. } => parity,
            Self::BitSet(bits) => bits.parity(),
        }
    }

    pub(crate) fn strict_add(self, rhs: Self) -> Self {
        match [self, rhs] {
            [Self::Exact(this), Self::Exact(rhs)] => Self::Exact(this.strict_add(rhs)),
            [Self::Complex { range, parity }, Self::Exact(exact)]
            | [Self::Exact(exact), Self::Complex { range, parity }] => Self::Complex {
                range: (range.start.strict_add(exact)..=range.last.strict_add(exact)).into(),
                parity: parity.map(|parity| parity ^ Parity::of(exact)),
            },
            [
                Self::Complex { range, parity },
                Self::Complex {
                    range: range_rhs,
                    parity: parity_rhs,
                },
            ] => {
                let parity = parity.zip(parity_rhs).map(|(this, rhs)| this ^ rhs);
                let range = (range.start.strict_add(range_rhs.start)
                    ..=range.last.strict_add(range_rhs.last))
                    .into();
                Self::Complex { range, parity }
            }
            [Self::Exact(exact), Self::BitSet(bits)] | [Self::BitSet(bits), Self::Exact(exact)] => {
                BitSet::try_from_iter(bits.into_iter().map(|rhs| exact.strict_add(rhs)))
                    .expect("possible len should fit in `u32`")
                    .into()
            }
            [Self::Complex { range, parity }, Self::BitSet(bits)]
            | [Self::BitSet(bits), Self::Complex { range, parity }] => BitSet::try_from_iter(
                Self::iter_complex(range, parity)
                    .cartesian_product(bits)
                    .map(|(a, b)| a.strict_add(b)),
            )
            .expect("possible len should fit in `u32`")
            .into(),
            [Self::BitSet(bits), Self::BitSet(rhs)] => BitSet::try_from_iter(
                bits.into_iter()
                    .cartesian_product(rhs)
                    .map(|(a, b)| a.strict_add(b)),
            )
            .expect("possible len should fit in `u32`")
            .into(),
        }
    }

    pub(crate) fn tight(self, set: Set1) -> Result<Option<EndPoint>, Contradiction> {
        match set.len().get().cmp(&self.minimum()) {
            Ordering::Less => return Err(Contradiction),
            Ordering::Equal => return Ok(Some(EndPoint::Minimum)),
            Ordering::Greater => {}
        }
        if self.maximum() == 0 {
            return Ok(Some(EndPoint::Maximum));
        }
        Ok(None)
    }

    fn bit_set(values: impl IntoIterator<Item = Number>) -> Option<Self> {
        Some(BitSet::try_from_iter(values)?.into())
    }

    fn iter_complex(
        range: RangeInclusive<Number>,
        parity: Option<Parity>,
    ) -> impl Iterator<Item = Number> + Clone {
        range
            .into_iter()
            .step_by(if parity.is_some() { 2 } else { 1 })
    }
}

impl BitAnd for CountSet {
    type Output = Option<Self>;

    fn bitand(self, rhs: Self) -> Self::Output {
        let intersection = match [self, rhs] {
            [Self::Exact(exact), other] | [other, Self::Exact(exact)] => {
                if !other.matches(exact) {
                    return None;
                }
                Self::Exact(exact)
            }
            [
                Self::Complex { range, parity },
                Self::Complex {
                    range: range_1,
                    parity: parity_1,
                },
            ] => {
                let min = range.start.max(range_1.start);
                let max = range.last.min(range_1.last);
                // TODO use Option::reduce https://github.com/rust-lang/rust/issues/144273
                let parity = match [parity, parity_1] {
                    [None, None] => None,
                    [None, Some(parity)] | [Some(parity), None] => Some(parity),
                    [Some(parity), Some(parity_1)] if parity == parity_1 => Some(parity),
                    [Some(_), Some(_)] => return None,
                };
                Self::complex(min, max, parity)?
            }
            [Self::Complex { range, parity }, Self::BitSet(bits)]
            | [Self::BitSet(bits), Self::Complex { range, parity }] => {
                Self::bit_set(Self::iter_complex(range, parity).filter(|&val| bits.contains(val)))?
            }
            [Self::BitSet(bits), Self::BitSet(rhs)] => (bits & rhs)?.into(),
        };
        Some(intersection)
    }
}

impl From<Number> for CountSet {
    fn from(exact: Number) -> Self {
        Self::Exact(exact)
    }
}

impl From<BitSet> for CountSet {
    fn from(bits: BitSet) -> Self {
        bits.singleton().map_or(Self::BitSet(bits), Self::Exact)
    }
}

pub(crate) enum EndPoint {
    Minimum,
    Maximum,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Interval<T> {
    range: RangeInclusive<T>,
}

impl<T: Ord + Copy> Interval<T> {
    pub(crate) fn matches(&self, value: &T) -> bool {
        self.range.contains(value)
    }

    pub(crate) fn intersection(self, rhs: Self) -> Option<Self> {
        Self::range(
            self.range.start.max(rhs.range.start),
            self.range.last.min(rhs.range.last),
        )
    }

    pub(crate) fn range(min: T, max: T) -> Option<Self> {
        (min <= max).then(|| Self {
            range: (min..=max).into(),
        })
    }

    pub(crate) fn exact(exact: T) -> Self {
        Self {
            range: (exact..=exact).into(),
        }
    }

    pub(crate) fn max(&self) -> T {
        self.range.last
    }

    pub(crate) fn min(&self) -> T {
        self.range.start
    }

    pub(crate) fn intersect(&mut self, rhs: Self) -> Result<bool, Contradiction> {
        let intersection = self.intersection(rhs).ok_or(Contradiction)?;
        let updated = intersection != *self;
        *self = intersection;
        Ok(updated)
    }
}

impl Interval<Number> {
    pub(crate) fn zero_to(max: Number) -> Self {
        let range = (0..=max).into();
        Self { range }
    }

    pub(crate) fn checked_div_2(self) -> Option<Self> {
        Self::range(self.range.start.div_ceil(2), self.range.last / 2)
    }

    pub(crate) fn checked_sub(self, rhs: Number) -> Option<Self> {
        let range =
            (self.range.start.saturating_sub(rhs)..=self.range.last.checked_sub(rhs)?).into();
        Some(Self { range })
    }
}

impl Interval<i8> {
    pub(crate) fn strict_sub_unsigned(self, rhs: Number) -> Self {
        let range = (self.range.start.strict_sub_unsigned(rhs)
            ..=self.range.last.strict_sub_unsigned(rhs))
            .into();
        Self { range }
    }

    pub(crate) fn strict_add_unsigned(self, rhs: Number) -> Self {
        let range = (self.range.start.strict_add_unsigned(rhs)
            ..=self.range.last.strict_add_unsigned(rhs))
            .into();
        Self { range }
    }

    pub(crate) fn non_negative(self) -> Option<Interval<Number>> {
        match self {
            Self { range } => Interval::range(
                range.start.try_into().unwrap_or(0),
                range.last.try_into().ok()?,
            ),
        }
    }

    pub(crate) fn strict_neg(self) -> Self {
        let range = (self.range.last.strict_neg()..=self.range.start.strict_neg()).into();
        Self { range }
    }

    pub(crate) fn neg_to_pos(neg_min: u8, max: u8) -> Self {
        let min = 0_i8.strict_sub_unsigned(neg_min);
        let max = max.try_into().expect("at most i8::MAX");
        let range = (min..=max).into();
        Self { range }
    }
}

impl From<Interval<Number>> for CountSet {
    fn from(value: Interval<Number>) -> Self {
        if value.range.start == value.range.last {
            Self::Exact(value.range.start)
        } else {
            Self::Complex {
                range: value.range,
                parity: None,
            }
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct BitSet(NonZero<u32>);

impl BitSet {
    fn contains(self, len: Number) -> bool {
        self.0.get() & (1 << len) != 0
    }

    fn singleton(self) -> Option<Number> {
        (self.0.count_ones().get() == 1).then(|| self.min())
    }

    fn min(self) -> Number {
        self.0.trailing_zeros().try_into().expect("at most 20")
    }

    fn try_from_iter(values: impl IntoIterator<Item = Number>) -> Option<Self> {
        let bits = values.into_iter().try_fold(0, |bits, value| {
            Some(bits | 1_u32.checked_shl(value.into())?)
        })?;
        let bits = NonZero::new(bits)?;
        Some(Self(bits))
    }

    fn parity(self) -> Option<Parity> {
        if self.0.get() & 0x5555_5555 == 0 {
            Some(Parity::Odd)
        } else if self.0.get() & 0xAAAA_AAAA == 0 {
            Some(Parity::Even)
        } else {
            None
        }
    }
}

impl BitAnd for BitSet {
    type Output = Option<Self>;

    fn bitand(self, rhs: Self) -> Self::Output {
        Some(Self(NonZero::new(self.0.get() & rhs.0.get())?))
    }
}

impl IntoIterator for BitSet {
    type Item = Number;

    type IntoIter = IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter { bits: self.0.get() }
    }
}

impl IntoIterator1 for BitSet {
    #[expect(
        unsafe_code,
        reason = "There is no other way to infallibly create an Iterator1<BitSetIter>"
    )]
    fn into_iter1(self) -> Iterator1<Self::IntoIter> {
        // SAFETY yields self.0.count_ones() many items
        unsafe { Iterator1::from_iter_unchecked(self) }
    }
}

#[derive(Clone)]
pub(crate) struct IntoIter {
    bits: u32,
}

impl Iterator for IntoIter {
    type Item = Number;

    fn next(&mut self) -> Option<Self::Item> {
        if self.bits == 0 {
            return None;
        }
        let index = self.bits.trailing_zeros();
        self.bits ^= 1 << index;
        Some(index.try_into().expect("at most 20"))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.bits.count_ones().try_into().expect("at most 20");
        (len, Some(len))
    }
}

impl ExactSizeIterator for IntoIter {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn count_set_classifies_bound_matches() {
        assert!(matches!(
            CountSet::Exact(2).match_bound(Bound::Exact(2)),
            TriState::Yes
        ));
        assert!(matches!(
            CountSet::complex(1, 3, None)
                .unwrap()
                .match_bound(Bound::Exact(2)),
            TriState::Maybe
        ));
        assert!(matches!(
            CountSet::complex(1, 3, None)
                .unwrap()
                .match_bound(Bound::AtLeast(1)),
            TriState::Yes
        ));
        assert!(matches!(
            CountSet::complex(0, 2, None)
                .unwrap()
                .match_bound(Bound::AtLeast(3)),
            TriState::No
        ));
        assert!(matches!(
            CountSet::complex(0, 4, Some(Parity::Even))
                .unwrap()
                .match_bound(Bound::Parity(Parity::Even)),
            TriState::Yes
        ));
    }
}
