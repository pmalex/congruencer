pub mod iterator;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LongNaturalNumber {
    pub digits: Vec<usize>,
    pub radix: usize,
}

impl LongNaturalNumber {
    #[inline]
    /// Creates a long zero.
    pub fn zero(digits_number: usize, radix: usize) -> Self {
        assert!(radix > 0 && digits_number > 0);

        Self {
            digits: vec![0; digits_number],
            radix,
        }
    }

    #[inline]
    /// Creates a long number from a raw slice.
    pub fn from_raw_slice(digits: &[usize], radix: usize) -> Self {
        Self {
            digits: digits.to_owned(),
            radix,
        }
    }
}
