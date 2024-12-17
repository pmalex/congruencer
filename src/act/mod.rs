use rayon::iter::{IntoParallelIterator, ParallelIterator};

use crate::{
    congruence::Congruence,
    partition::{self, Partition},
};
use raw_act::RawAct;

pub mod raw_act;

pub struct Act {
    raw_act: RawAct,

    /// Имена элеменов множества в текстовом (понятном человеку) виде.
    elements_names: Vec<String>,
}

impl Act {
    /// Создаёт полигон из списка элементов и таблицы, в которых элементы заданы строками, то есть удобными
    /// для чтения человеком.
    ///
    /// ## Внимание
    /// Число строк интерпретируется как число элементов полигона, а число
    /// столбцов - как число элементов в полугруппе.
    pub fn from_str_table(elements_names: &[&str], cayley_table: &[&str]) -> Self {
        // Удостоверяемся, что `act_elements` содержит только уникальные элементы.
        {
            let mut act_elements_names_vec = elements_names.to_vec();
            act_elements_names_vec.sort_unstable(); // Сортировка поставит одинаковые элементы рядом друг с другом
            act_elements_names_vec.dedup(); // Удаляем стоящие рядом одинаковые элементы

            assert_eq!(
                elements_names.len(),
                act_elements_names_vec.len(),
                "Список элементов полигона содержит повторяющиеся элементы"
            )
        }

        let elements_names = elements_names
            .iter()
            .map(|&x| String::from(x))
            .collect::<Vec<String>>();

        // Формируем таблицу Кэли, состоящую из чисел, а не из строк,
        // то есть нам нужно заменить строчки на их уникальные коды.
        let new_cayley_table = cayley_table
            .iter()
            .map(|&s| {
                elements_names
                    .iter()
                    .position(|t| *t == *s)
                    .unwrap_or_else(|| panic!("В таблице присутствует символ `{}`, не указанный в списке элементов полигона", s))
            })
            .map(|l| l as u32)
            .collect::<Vec<u32>>();

        Self {
            raw_act: RawAct::new(&new_cayley_table, elements_names.len()),
            elements_names,
        }
    }

    /// Создание решётки конгруэнций из всевозможных разбиений элементов полигона.
    pub fn new_congruence_set(&self) -> Vec<Partition>
    where
        Self: Sync,
    {
        partition::new_partitions_set(&self.elements_names)
            .into_par_iter()
            .filter(|partition| self.is_congruence(partition))
            .collect()
    }
}

impl<'a> Congruence<Partition<'a>> for Act {
    #[inline(always)]
    fn is_congruence(&self, named_partition: &Partition<'a>) -> bool {
        self.raw_act.is_congruence(&named_partition.raw_partition)
    }
}

impl std::fmt::Display for Act {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let semigroup_size = self.raw_act.semigroup_size;
        let act_size = self.raw_act.act_size;

        // Определяем строчку с максимальной длиной (для выравнивания печати)
        let max_len = self
            .elements_names
            .iter()
            .max_by(|&x, &y| x.len().cmp(&y.len()))
            .unwrap()
            .len();

        for i in 0..semigroup_size {
            // Сначала печатаем название элемента
            write!(f, "{: >width$} | ", self.elements_names[i], width = max_len)?;

            // Теперь печатаем оставшуюся строчку
            for j in 0..act_size {
                write!(
                    f,
                    "{: >width$} ",
                    self.elements_names[self.raw_act.cayley_table[i * act_size + j] as usize],
                    width = max_len
                )?;
            }

            writeln!(f)?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::Act;

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

        let named_act = Act::from_str_table(&["x", "y", "z", "u"], &cayley_table);

        #[rustfmt::skip]
        assert_eq!(
            named_act.raw_act.cayley_table,
            [
                1, 2, 3,
                2, 3, 3,
                3, 3, 3,
                3, 3, 3
            ]
        );
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

        Act::from_str_table(&["x", "y", "u", "z", "u"], &cayley_table);
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

        Act::from_str_table(&["x", "y", "u", "z"], &cayley_table);
    }
}
