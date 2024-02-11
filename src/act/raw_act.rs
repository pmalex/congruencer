use rayon::iter::{IntoParallelIterator, ParallelIterator};

use crate::{
    congruence::Congruence,
    partition::{self, Partition},
    ElementIndex,
};

/// Базовая структура полигона над полугруппой.
pub struct RawAct {
    cayley_table: Vec<ElementIndex>,

    /// Число столбцов в таблице Кэли, или, что то же самое - число элементов
    /// полугруппы, над которой определён полигон.
    columns: usize,
}

impl RawAct {
    /// Создание структуры полигона из его таблицы умножения (таблицы Кэли).
    /// Её строки обозначают элементы полигона, столбцы - элементы полугруппы.
    pub fn new(cayley_table: Vec<ElementIndex>, act_size: usize) -> Self {
        assert!(
            !cayley_table.is_empty() && cayley_table.len() % act_size == 0,
            "Из данного массива невозможно сделать таблицу с указанным числом элементов полигона."
        );

        // То же самое, что число элементов полугруппы.
        let columns = cayley_table.len() / act_size;

        assert!(columns > 0 && columns <= cayley_table.len());

        Self {
            cayley_table,
            columns,
        }
    }

    /// Multiply a set of an act's elements by a semigroup element `s`.
    pub fn m(&self, mut class: ElementIndex, s: usize) -> ElementIndex {
        // Check that the number (class) and the table are compatible.
        debug_assert!(
            ((ElementIndex::BITS - class.leading_zeros() - 1) as usize) // Position of the most significant unity.
            <
            self.cayley_table.len() / self.columns, // Number of rows.
            "The class({:b}) is too big, the table does not contains enough values",
            class
        );

        debug_assert!(s < self.columns);

        let mut result = 0;

        for i in 0..ElementIndex::BITS as usize {
            if class == 0 {
                break;
            }

            if class & 0b1 == 0b1 {
                result |= 1 << self.cayley_table[i * self.columns + s];
            }

            class >>= 1;
        }

        result
    }
}

impl Congruence for RawAct {
    fn is_congruence(&self, partition: &Partition) -> bool {
        let act = self;
        let semigroup_size = self.columns;

        partition.par_iter().all(|&class| {
            debug_assert!(class > 0);

            (0..semigroup_size).into_par_iter().all(|s| {
                let new_class = act.m(class as ElementIndex, s) as partition::BaseDataType;

                debug_assert!(new_class > 0);

                partition
                    .par_iter()
                    .any(|&base_class| (new_class | base_class) == base_class)
            })
        })
    }

    #[inline(always)]
    fn size(&self) -> usize {
        self.cayley_table.len() / self.columns
    }
}

#[cfg(test)]
mod test {
    use super::RawAct;
    use crate::{congruence::Congruence, partition::Partition};

    #[test]
    fn act_multiplication() {
        #[rustfmt::skip]
        let cayley_table = [
            /* 0 */ 1, 0, 3, 2, 2,
            /* 1 */ 3, 1, 1, 0, 2,
            /* 2 */ 2, 3, 3, 1, 2,
            /* 3 */ 3, 3, 2, 3, 2,
        ];

        let act = RawAct::new(cayley_table.to_vec(), 4);

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
            /* 0 */ 1, 0, 3, 2, 2,
            /* 1 */ 3, 1, 1, 0, 2,
            /* 2 */ 2, 3, 3, 1, 2,
            /* 3 */ 3, 3, 2, 3, 2,
        ];

        let act = RawAct::new(cayley_table.to_vec(), 4);

        let partition = Partition::new(&[0b1010, 0b0101], 4);

        assert_eq!(act.is_congruence(&partition), false);
    }

    #[test]
    fn is_congruence_2() {
        #[rustfmt::skip]
        let cayley_table = [
            /* 0 */ 1, 3, 2, 2,
            /* 1 */ 3, 0, 2, 1,
            /* 2 */ 3, 1, 2, 0,
            /* 3 */ 3, 2, 0, 3,
        ];

        let act = RawAct::new(cayley_table.to_vec(), 4);

        let partition = Partition::new(&[0b1010, 0b0101], 4);

        assert_eq!(act.is_congruence(&partition), true);
    }
}
