use crate::partition::{self, Partition};

/// Именованное разбиение конечного множества.
pub struct NamedPartition<'a> {
    pub(crate) partition: Partition,

    /// Имена элеменов множества в текстовом (понятном человеку) виде.
    pub(crate) elements_names: &'a [String],
}

impl<'a> NamedPartition<'a> {
    #[inline]
    pub fn from_partition(partition: Partition, elements_names: &'a [String]) -> Self {
        Self {
            partition,
            elements_names,
        }
    }
}

#[inline]
/// Возвращает вектор, содержащий всевозможные разбиения конечного множества.
pub fn new_partitions_set(elements_names: &[String]) -> Vec<NamedPartition> {
    // Удостоверяемся, что `act_elements` содержит только уникальные элементы.
    {
        let mut set_elements_names_vec = elements_names.to_vec();
        set_elements_names_vec.sort_unstable(); // Сортировка поставит одинаковые элементы рядом друг с другом
        set_elements_names_vec.dedup(); // Удаляем стоящие рядом одинаковые элементы

        // Если длины списков разные, то значит исходный список содержит повторяющиеся элементы.
        assert_eq!(
            elements_names.len(),
            set_elements_names_vec.len(),
            "Список элементов полигона содержит повторяющиеся!"
        )
    }

    let partition_set = super::new_partitions_set(elements_names.len());

    // Переупаковываем результат
    partition_set
        .into_iter()
        .map(|partition| NamedPartition {
            partition,
            elements_names,
        })
        .collect()
}

impl<'a> std::fmt::Display for NamedPartition<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        /// Символ, отделяющий друг от друга элементы множества
        const DELIMITER: char = ',';

        if self.partition.partition_size == self.partition.data.len() {
            write!(f, "Δ")
        } else if self.partition.data.len() == 1 {
            write!(f, "∇")
        } else {
            for &class in &self.partition.data {
                if class.is_power_of_two() {
                    continue; // Do not print the powers of two, because it's a one-element class.
                }

                write!(f, "(")?;

                // Вычисляем самый старший бит для данного класса. Это нужно для того, чтобы
                // не печатать символ-разделитель в самом конце, перед закрывающей скобкой,
                // т.е. чтобы не засорять вывод.
                let leading_bit_mask =
                    1 << (partition::BaseDataType::BITS - class.leading_zeros() - 1);

                let mut mask = 1 as partition::BaseDataType;

                for s in self.elements_names {
                    // Если соответствующий по номеру бит установлен, то печатаем символ
                    // из алфавита, соответствующий ему.
                    if class & mask != 0 {
                        write!(f, "{}", s)?;

                        // Если печатаем не последний символ, то добавляем символ-разделитель.
                        if mask & leading_bit_mask == 0 {
                            write!(f, "{}", DELIMITER)?;
                        }
                    }

                    mask <<= 1;
                }

                write!(f, ")")?;
            }

            Ok(())
        }
    }
}

impl<'a> PartialEq for NamedPartition<'a> {
    #[inline(always)]
    fn eq(&self, other: &Self) -> bool {
        self.partition.eq(&other.partition)
    }
}

impl<'a> PartialOrd for NamedPartition<'a> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.partition.partial_cmp(&other.partition)
    }

    #[inline(always)]
    fn le(&self, other: &Self) -> bool {
        self.partition.le(&other.partition)
    }

    #[inline(always)]
    fn ge(&self, other: &Self) -> bool {
        other.le(self)
    }
}
