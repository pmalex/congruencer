pub mod iterator;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LongNaturalNumber<const SIZE: usize, const RADIX: usize> {
    pub digits: [usize; SIZE],
}

impl<const SIZE: usize, const RADIX: usize> LongNaturalNumber<SIZE, RADIX> {
    #[inline]
    /// Creates a long zero.
    pub fn zero() -> Self {
        assert!(RADIX > 0 && SIZE > 0);

        Self { digits: [0; SIZE] }
    }

    #[inline]
    /// Creates a long number from a raw slice.
    pub fn from_raw_slice(digits: &[usize; SIZE]) -> Self {
        Self { digits: *digits }
    }
}
