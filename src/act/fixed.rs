use rayon::prelude::{IntoParallelIterator, ParallelIterator};

use crate::{congruence::Congruence, partition::Partition, ElementIndex};

/// Полигон над полугруппой.
pub struct Act<const ACT_SIZE: usize, const SEMIGROUP_SIZE: usize>(
    [[ElementIndex; SEMIGROUP_SIZE]; ACT_SIZE],
);

impl<const ACT_SIZE: usize, const SEMIGROUP_SIZE: usize> Act<ACT_SIZE, SEMIGROUP_SIZE> {
    /// Создание структуры полигона из его таблицы умножения (таблицы Кэли).
    pub fn new(cayley_table: [[ElementIndex; SEMIGROUP_SIZE]; ACT_SIZE]) -> Self {
        assert!(!cayley_table.is_empty());

        Self(cayley_table)
    }

    /// Multiply a set of an act's elements by a semigroup element `s`.
    pub fn m(&self, mut class: ElementIndex, s: usize) -> ElementIndex {
        if cfg!(debug_assertions) {
            let rows_number = self.0.len();
            let columns_number = self.0[0].len();

            // Check that the number (class) and the table are compatible.
            assert!(
                ((ElementIndex::BITS - class.leading_zeros() - 1) as usize) // Position of the most significant unity.
                <
                rows_number,
                "The class({:b}) is too big, the table does not contains enough values",
                class
            );

            assert!(s < columns_number);
        }

        let mut result = 0;

        for i in 0..ElementIndex::BITS as usize {
            if class == 0 {
                break;
            }

            if class & 0b1 == 0b1 {
                result |= 1 << self.0[i][s];
            }

            class >>= 1;
        }

        result
    }
}

impl<const ACT_SIZE: usize, const SEMIGROUP_SIZE: usize> Congruence
    for Act<ACT_SIZE, SEMIGROUP_SIZE>
{
    fn is_congruence(&self, partition: &Partition) -> bool {
        let act = self;

        partition.par_iter().all(|&class| {
            debug_assert!(class > 0);

            (0..SEMIGROUP_SIZE).into_par_iter().all(|s| {
                let new_class = act.m(class as ElementIndex, s) as u16;

                debug_assert!(new_class > 0);

                partition
                    .par_iter()
                    .any(|&base_class| (new_class | base_class) == base_class)
            })
        })
    }

    #[inline(always)]
    fn size(&self) -> usize {
        ACT_SIZE
    }
}

#[cfg(test)]
mod test {
    use crate::{congruence::Congruence, partition::Partition};

    use super::Act;

    #[test]
    fn act_multiplication() {
        #[rustfmt::skip]
        let cayley_table = [
            /* 0 */ [1, 0, 3, 2, 2],
            /* 1 */ [3, 1, 1, 0, 2],
            /* 2 */ [2, 3, 3, 1, 2],
            /* 3 */ [3, 3, 2, 3, 2],
        ];

        let act = Act::new(cayley_table);

        assert_eq!(act.m(0b01010, 0), 0b01000);
        assert_eq!(act.m(0b01010, 1), 0b01010);
        assert_eq!(act.m(0b01010, 2), 0b00110);
        assert_eq!(act.m(0b01010, 3), 0b01001);
        assert_eq!(act.m(0b01101, 4), 0b00100);
    }

    #[test]
    fn is_congruence_1() {
        #[rustfmt::skip]
        let cayley_table = [
            /* 0 */ [1, 0, 3, 2, 2],
            /* 1 */ [3, 1, 1, 0, 2],
            /* 2 */ [2, 3, 3, 1, 2],
            /* 3 */ [3, 3, 2, 3, 2],
        ];

        let act = Act::new(cayley_table);

        let partition = Partition::new(&[0b1010, 0b0101], 4);

        assert_eq!(act.is_congruence(&partition), false);
    }

    #[test]
    fn is_congruence_2() {
        #[rustfmt::skip]
        let cayley_table = [
            /* 0 */ [1, 3, 2, 2],
            /* 1 */ [3, 0, 2, 1],
            /* 2 */ [3, 1, 2, 0],
            /* 3 */ [3, 2, 0, 3],
        ];

        let act = Act::new(cayley_table);

        let partition = Partition::new(&[0b1010, 0b0101], 4);

        assert_eq!(act.is_congruence(&partition), true);
    }
}
