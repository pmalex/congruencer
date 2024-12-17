use rayon::iter::{IntoParallelIterator, ParallelIterator};

use crate::{
    congruence::Congruence,
    partition::raw_partition::{self, RawPartition},
};

/// Полигон над полугруппой.
pub struct RawAct {
    pub(crate) cayley_table: Vec<u32>,

    pub semigroup_size: usize,
    pub act_size: usize,
}

impl RawAct {
    /// Создание структуры полигона из его таблицы умножения (таблицы Кэли).
    ///
    /// Её строки обозначают элементы полигона, столбцы - элементы полугруппы.
    pub fn new(cayley_table: &[u32], act_size: usize) -> Self {
        assert!(
            !cayley_table.is_empty(),
            "Таблица Кэли пуста, нечего создавать"
        );

        assert!(act_size > 0);

        let semigroup_size = cayley_table.len() / act_size;

        assert_eq!(
            act_size * semigroup_size,
            cayley_table.len(),
            "Размер полигона и полугруппы не соответствуют переданной таблице Кэли"
        );

        Self {
            cayley_table: cayley_table.to_vec(),
            act_size,
            semigroup_size,
        }
    }

    /// Умножает множество элементов полигона на элемент полугруппы, указанный при помощи индекса.
    pub fn m(&self, elements: u32, s_index: usize) -> u32 {
        // Проверяем, что переданное множество элементов совместимо с таблицей Кэли полигона.
        debug_assert!(
            ((u32::BITS - elements.leading_zeros() - 1) as usize) // Position of the most significant unity.
            <
            self.act_size,
            "The class({:b}) is too big, the table does not contains enough values",
            elements
        );

        debug_assert!(s_index < self.semigroup_size);

        let mut result = 0;
        let mut elements = elements;

        for i in 0..u32::BITS as usize {
            if elements == 0 {
                break;
            }

            // Если бит выставлен, то элемент представлен в классе
            if elements & 0b1 == 0b1 {
                // Индекс i обозначает индекс элемента полигона
                result |= 1
                    << unsafe {
                        self.cayley_table
                            .get_unchecked(i * self.semigroup_size + s_index)
                    };
            }

            // Сдвигаем весь класс на единицу вправо
            elements >>= 1;
        }

        result
    }

    /// Создание решётки конгруэнций из всевозможных разбиений элементов полигона.
    pub fn new_congruence_set(&self) -> Vec<RawPartition>
    where
        Self: Sync,
    {
        raw_partition::new_partitions_set(self.act_size)
            .into_par_iter()
            .filter(|partition| self.is_congruence(partition))
            .collect()
    }
}

impl Congruence<RawPartition> for RawAct {
    /// Возвращает true, если переданное разбиение является конгруэнцией.
    fn is_congruence<'a>(&self, partition: &RawPartition) -> bool {
        let act = self;
        let semigroup_size = self.semigroup_size;

        // Обходим каждый независимый кусок (класс) разбиения.
        partition.to_iter().all(|&class| {
            debug_assert!(class > 0);

            (0..semigroup_size).into_par_iter().all(|index| {
                // Умножаем все элементы класса на элемент (индекс элемента) полугруппы.
                let new_class = act.m(class, index) as raw_partition::BaseDataType;

                debug_assert!(new_class > 0);

                // Смотрим, не вышел ли класс за пределы разбиения
                // (является ли подмножеством в каком-либо другом классе).
                partition
                    .to_iter()
                    .any(|&base_class| (new_class | base_class) == base_class)
            })
        })
    }
}

#[cfg(test)]
mod test {
    use super::RawAct;
    use crate::{congruence::Congruence, partition::raw_partition::RawPartition};

    #[test]
    fn act_multiplication() {
        #[rustfmt::skip]
        let cayley_table = [
            //      s_1  s_2  s_3  s_4  s_5
            /* 0 */  1,   0,   3,   2,   2,
            /* 1 */  3,   1,   1,   0,   2,
            /* 2 */  2,   3,   3,   1,   2,
            /* 3 */  3,   3,   2,   3,   2,
        ];

        let act = RawAct::new(&cayley_table, 4);

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

        let act = RawAct::new(&cayley_table, 5);

        let partition = RawPartition::new(&[0b1010, 0b0101], 4);

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

        let act = RawAct::new(&cayley_table, 4);

        let partition = RawPartition::new(&[0b1010, 0b0101], 4);

        assert_eq!(act.is_congruence(&partition), true);
    }
}
