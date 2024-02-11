use crate::{congruence::Congruence, partition::Partition};

pub mod raw_act;

/// Полигон над полугруппой.
pub struct Act<'a> {
    raw_act: raw_act::RawAct,

    /// Имена элеменов полигона в текстовом (понятном человеку) виде.
    _act_elements_names: &'a [&'a str],
}

impl<'a> Act<'a> {
    pub fn new(act_elements_names: &'a [&'a str], cayley_table: &[&'a str]) -> Act<'a> {
        // Удостоверяемся, что `act_elements` содержит только уникальные элементы.
        {
            let mut act_elements_names_vec = act_elements_names.to_vec();
            act_elements_names_vec.sort(); // Сортировка поставит одинаковые элементы рядом друг с другом
            act_elements_names_vec.dedup(); // Удаляем стоящие рядом одинаковые элементы

            // Если длины списков разные, то значит исходный список содержит повторяющиеся элементы.
            assert_eq!(
                act_elements_names.len(),
                act_elements_names_vec.len(),
                "Список элементов полигона содержит повторяющиеся!"
            )
        }

        // Формируем таблицу Кэли, состоящую из чисел, а не из строк,
        // то есть нам нужно заменить строчки на их уникальные коды.
        let new_cayley_table = cayley_table
            .iter()
            .map(|&s| {
                act_elements_names
                    .iter()
                    .position(|&t| *t == *s)
                    .expect(&format!(
                    "В таблице присутствует символ `{}`, не указанный в списке элементов полигона",
                    s
                ))
            })
            .collect::<Vec<usize>>();

        Self {
            raw_act: raw_act::RawAct::new(new_cayley_table, act_elements_names.len()),
            _act_elements_names: act_elements_names,
        }
    }
}

impl<'a> Congruence for Act<'a> {
    #[inline(always)]
    fn is_congruence(&self, partition: &Partition) -> bool {
        self.raw_act.is_congruence(&partition.raw_partition)
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

        Act::new(&["x", "y", "z", "u"], &cayley_table);
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

        Act::new(&["x", "y", "u", "z", "u"], &cayley_table);
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

        Act::new(&["x", "y", "u", "z"], &cayley_table);
    }
}
