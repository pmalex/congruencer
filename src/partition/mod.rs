pub mod raw_partition;

/// Разбиение конечного множества.
pub struct Partition<'a> {
    pub(crate) raw_partition: raw_partition::RawPartition,

    /// Имена элеменов множества в текстовом (понятном человеку) виде.
    pub(crate) set_elements_names: &'a [&'a str],
}

#[inline]
/// Возвращает вектор, содержащий всевозможные разбиения конечного множества.
pub fn new_partitions_set<'a>(set_elements_names: &'a [&'a str]) -> Vec<Partition> {
    // Удостоверяемся, что `act_elements` содержит только уникальные элементы.
    {
        let mut set_elements_names_vec = set_elements_names.to_vec();
        set_elements_names_vec.sort(); // Сортировка поставит одинаковые элементы рядом друг с другом
        set_elements_names_vec.dedup(); // Удаляем стоящие рядом одинаковые элементы

        // Если длины списков разные, то значит исходный список содержит повторяющиеся элементы.
        assert_eq!(
            set_elements_names.len(),
            set_elements_names_vec.len(),
            "Список элементов полигона содержит повторяющиеся!"
        )
    }

    let raw_partition_set = raw_partition::new_partitions_set(set_elements_names.len());

    // Переупаковываем результат
    raw_partition_set
        .into_iter()
        .map(|raw_partition| Partition {
            raw_partition,
            set_elements_names,
        })
        .collect()
}

impl<'a> std::fmt::Display for Partition<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        /// Символ, отделяющий друг от друга элементы множества
        const DELIMITER: char = ',';

        if self.raw_partition.partition_size == self.raw_partition.data.len() {
            write!(f, "Δ")
        } else if self.raw_partition.data.len() == 1 {
            write!(f, "∇")
        } else {
            for &class in &self.raw_partition.data {
                if class.is_power_of_two() {
                    continue; // Do not print the powers of two, because it's a one-element class.
                }

                write!(f, "(")?;

                // Вычисляем самый старший бит для данного класса. Это нужно для того, чтобы
                // не печатать символ-разделитель в самом конце, перед закрывающей скобкой,
                // т.е. чтобы не засорять вывод.
                let leading_bit_mask =
                    1 << raw_partition::BaseDataType::BITS - class.leading_zeros() - 1;

                let mut mask = 1 as raw_partition::BaseDataType;

                for &s in self.set_elements_names {
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

impl<'a> PartialEq for Partition<'a> {
    #[inline(always)]
    fn eq(&self, other: &Self) -> bool {
        self.raw_partition.eq(&other.raw_partition)
    }
}

impl<'a> PartialOrd for Partition<'a> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.raw_partition.partial_cmp(&other.raw_partition)
    }

    #[inline(always)]
    fn le(&self, other: &Self) -> bool {
        self.raw_partition.le(&other.raw_partition)
    }

    #[inline(always)]
    fn ge(&self, other: &Self) -> bool {
        other.le(self)
    }
}
