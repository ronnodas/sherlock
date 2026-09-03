use std::cmp::Ordering;
use std::num::NonZero;
use std::ops::BitAnd;
use std::range::RangeInclusive;

use crate::solver::board::coordinates::Set1;
use crate::solver::hint::{BoundOrNot, Number, Parity};
use crate::solver::inference::Contradiction;

pub(crate) fn reduce_cardinal(count: BoundOrNot, rhs: Number) -> Option<BoundOrNot> {
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

#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) enum CountSet {
    Exact(Number),
    Complex {
        range: RangeInclusive<Number>,
        parity: Option<Parity>,
    },
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
        }
    }

    pub(crate) fn maximum(self) -> Number {
        match self {
            Self::Exact(value) => value,
            Self::Complex { range, .. } => range.last,
        }
    }

    pub(crate) fn matches(self, len: Number) -> bool {
        match self {
            Self::Exact(exact) => exact == len,
            Self::Complex { range, parity } => {
                range.contains(&len) && parity.is_none_or(|parity| parity.matches(len))
            }
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
            BoundOrNot::NotExact(_) => todo!(),
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
}

impl BitAnd for CountSet {
    type Output = Option<Self>;

    fn bitand(self, rhs: Self) -> Self::Output {
        match [self, rhs] {
            [Self::Exact(exact), rhs] | [rhs, Self::Exact(exact)] => {
                rhs.matches(exact).then_some(Self::Exact(exact))
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
                Self::complex(min, max, parity)
            }
        }
    }
}

impl From<Number> for CountSet {
    fn from(exact: Number) -> Self {
        Self::Exact(exact)
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
