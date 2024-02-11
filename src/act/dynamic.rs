//! Динамическая реализация структуры полигона.

use crate::{congruence::Congruence, partition::Partition, ElementIndex};
use rayon::prelude::{IntoParallelIterator, ParallelIterator};

/// Полигон над полугруппой.
pub struct Act<'a> {
    raw_act: RawAct,

    /// Элементы полигона в текстовом (понятном человеку) виде.
    act_elements: &'a [&'a str],
}

impl<'a> Act<'a> {
    pub fn new(act_elements: &'a [&'a str], cayley_table: &[&'a str], columns: usize) -> Act<'a> {
        // Удостоверяемся, что `act_elements` содержит только уникальные элементы.
        if cfg!(debug_assertions) {
            let mut act_elements_vec = act_elements.to_vec();
            act_elements_vec.sort();
            act_elements_vec.dedup(); // Удаляем стоящие рядом одинаковые элементы

            assert_eq!(
                act_elements.len(),
                act_elements_vec.len(),
                "Список элементов полигона содержит повторяющиеся!"
            )
        }

        // Формируем таблицу Кэли, состоящую из чисел, а не из строк,
        // то есть нам нужно заменить строчки на их уникальные коды.
        let new_cayley_table = cayley_table
            .iter()
            .map(|&s| {
                act_elements.iter().position(|&t| *t == *s).expect(&format!(
                    "В таблице присутствует символ `{}`, не указанный в списке элементов полигона",
                    s
                ))
            })
            .collect::<Vec<usize>>();

        dbg!(&new_cayley_table);

        Self {
            raw_act: RawAct::new(&new_cayley_table, columns),
            act_elements,
        }
    }
}

/// Проверяем, что полигон, заданный буквами формируется правильно.
#[test]
fn characters_assignment_test() {
    #[rustfmt::skip]
    let cayley_table = [
        //       a   a^2  a^3
        /* x */ "y", "z", "u",
        /* y */ "z", "u", "u",
        /* z */ "u", "u", "u",
        /* u */ "u", "u", "u",
        ];

    Act::new(&["x", "y", "z", "u"], &cayley_table, 3);
}

/// Проверяем, что при создании полигона с повторяющимися элементами невозможно.
#[test]
#[should_panic]
fn unique_elements_test() {
    #[rustfmt::skip]
    let cayley_table = [
        //       a   a^2  a^3
        /* x */ "y", "z", "u",
        /* y */ "z", "u", "u",
        /* z */ "u", "u", "u",
        /* u */ "u", "u", "u",
        ];

    Act::new(&["x", "y", "u", "z", "u"], &cayley_table, 3);
}

/// Проверяем, что попытка создать полигон с таблцией Кэли содержащей элемент
/// не указанный в списке элементов полигона провалится.
#[test]
#[should_panic]
fn table_correctness_test() {
    #[rustfmt::skip]
    let cayley_table = [
        //       a   a^2  a^3
        /* x */ "y", "z", "u",
        /* y */ "z", "u", "u",
        /* z */ "u", "u", "t",
        /* u */ "u", "u", "u",
        ];

    Act::new(&["x", "y", "u", "z"], &cayley_table, 3);
}

/// Базовая структура полигона над полугруппой.
struct RawAct {
    cayley_table: Vec<ElementIndex>,

    /// Число столбцов в таблице Кэли.
    columns: usize,
}

impl RawAct {
    /// Создание структуры полигона из его таблицы умножения (таблицы Кэли).
    /// Строки обозначают элементы полигона, столбцы - элементы полугруппы.
    pub fn new(cayley_table: &[ElementIndex], columns: usize) -> Self {
        assert!(columns > 0 && columns <= cayley_table.len());
        assert!(
            !cayley_table.is_empty() && cayley_table.len() % columns == 0,
            "Из данного массива невозможно сделать таблицу: число столбцов указано некорректно."
        );

        Self {
            cayley_table: cayley_table.to_owned(),
            columns,
        }
    }

    #[inline(always)]
    pub fn get_semigroup_size(&self) -> usize {
        self.columns
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

        partition.par_iter().all(|&class| {
            debug_assert!(class > 0);

            (0..act.get_semigroup_size()).into_par_iter().all(|s| {
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
        self.cayley_table.len() / self.columns
    }
}

#[cfg(test)]
mod test {
    use crate::{act::dynamic::RawAct, congruence::Congruence, partition::Partition};

    #[test]
    fn act_multiplication() {
        #[rustfmt::skip]
        let cayley_table = [
            /* 0 */ 1, 0, 3, 2, 2,
            /* 1 */ 3, 1, 1, 0, 2,
            /* 2 */ 2, 3, 3, 1, 2,
            /* 3 */ 3, 3, 2, 3, 2,
        ];

        let act = RawAct::new(&cayley_table, 5);

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

        let act = RawAct::new(&cayley_table, 4);

        let partition = Partition::new(&[0b1010, 0b0101], 4);

        assert_eq!(act.is_congruence(&partition), true);
    }
}
