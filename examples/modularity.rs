//! Пример построения решётки конгруэнций полигона над прямоугольной связкой и проверка
//! выполнения в данной решётке тождества модулярности.
//!
//! Пример взят из статьи: И. Б. Кожухов, А. М. Пряничников, А. Р. Симакова,
//! “Условия модулярности решетки конгруэнций полигона над прямоугольной связкой”,
//! Изв. РАН. Сер. матем., 84:2 (2020), 90–125.
//! DOI: https://doi.org/10.1070/IM8869
//! URL: https://www.mathnet.ru/rus/im8869

use congruencer::{act::Act, lattice::Lattice, partition::Partition, poset};

#[rustfmt::skip]
/// Полигон над прямогольной связкой №1, соответствует таблице I (с.121).
const ACT: [&str; 55] = [
    //       (l_1, r_1)  (l_1, r_2)  (l_2, r_3)  (l_2, r_1)  (l_2, r_2)
    /* a */     "1",        "2",        "6",        "5",        "6",
    /* b */     "4",        "5",        "3",        "1",        "2",
    /* 1 */     "1",        "2",        "3",        "1",        "2",
    /* 2 */     "1",        "2",        "3",        "1",        "2",
    /* 3 */     "1",        "2",        "3",        "1",        "2",
    /* 4 */     "4",        "5",        "6",        "5",        "6",
    /* 5 */     "4",        "5",        "6",        "5",        "6",
    /* 6 */     "4",        "5",        "6",        "5",        "6",
    /* 7 */     "7",        "8",        "9",        "9",        "7",
    /* 8 */     "7",        "8",        "9",        "9",        "7",
    /* 9 */     "7",        "8",        "9",        "9",        "7",
];

fn main() {
    let elements = ["a", "b", "1", "2", "3", "4", "5", "6", "7", "8", "9"];

    // Создаём полигон
    let act = Act::from_str_table(&elements, &ACT);

    let congruence_set = act.new_congruence_set();

    let lattice = Lattice::from(congruence_set.as_slice());

    // Печатаем решётку конгруэнций как частично упорядоченное множество.
    _partitions_set_print_dot(&congruence_set);

    println!("Решётка содержит {} элементов", congruence_set.len());
    println!("Тест на модулярность: {}", lattice.is_modular());
}

/// Prints a partitions set in the Graphiz Dot format.
fn _partitions_set_print_dot(partitions_set: &[Partition]) {
    println!("graph lattice {{");
    println!("\trankdir = TB;");
    println!("\tratio = 0.75;");
    println!("\tnode[shape = none];");
    println!();

    for partition in partitions_set {
        for nearest_upper_bound in
            poset::nearest_incomparable_lower_bounds(partitions_set, partition)
        {
            println!("\t\"{}\" -- \"{}\"", partition, nearest_upper_bound)
        }
    }

    println!("}}");
}
