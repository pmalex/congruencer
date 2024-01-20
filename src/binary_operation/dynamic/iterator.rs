use super::BinaryOperation;
use crate::long_number::dynamic::{iterator::LongNaturalNumberIterator, LongNaturalNumber};

pub struct BinaryOperationIterator(LongNaturalNumberIterator);

impl BinaryOperationIterator {
    #[inline]
    pub fn new(set_size: usize) -> Self {
        assert!(set_size > 0);

        BinaryOperationIterator(LongNaturalNumberIterator::new(
            set_size * set_size,
            set_size,
        ))
    }
}

impl Iterator for BinaryOperationIterator {
    type Item = BinaryOperation;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(BinaryOperation::from)
    }
}

impl From<LongNaturalNumber> for BinaryOperation {
    fn from(long_number: LongNaturalNumber) -> Self {
        let LongNaturalNumber { digits, radix } = long_number;

        // The number of elements in a binary operation.
        let set_size = {
            let sqrt = (digits.len() as f32).sqrt() as usize;

            assert_eq!(sqrt * sqrt, digits.len());

            sqrt
        };

        assert!(radix >= set_size);

        Self {
            cayley_table: digits,
            set_size: radix,
        }
    }
}
