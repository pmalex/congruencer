use super::Magma;
use crate::long_number::iterator::LongNumberIterator;

/// Итератор, перебирающий всевозможные бинарные операции конченого множества.
pub struct MagmaIterator(LongNumberIterator);

impl MagmaIterator {
    pub fn new(set_size: usize) -> Self {
        assert!(set_size > 0);

        MagmaIterator(LongNumberIterator::new(set_size * set_size, set_size))
    }
}

impl Iterator for MagmaIterator {
    type Item = Magma;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(Magma::from)
    }
}
