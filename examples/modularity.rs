//! Примеры построения решётки конгруэнций полигона над прямоугольной связкой и проверка
//! выполнения в данной решётке тождества модулярности.

use congruencer::{act::Act, lattice::Lattice, partition::Partition, poset};

#[rustfmt::skip]
/// Пример взят из статьи: И. Б. Кожухов, А. М. Пряничников, А. Р. Симакова,
/// “Условия модулярности решётки конгруэнций полигона над прямоугольной связкой”,
/// Изв. РАН. Сер. матем., 84:2 (2020), 90–125.
/// DOI: https://doi.org/10.1070/IM8869
/// URL: https://www.mathnet.ru/rus/im8869
///
/// Данный полигон соответствует таблице I (с.121).
/// 
/// `Замечание`: в статье есть ошибка в таблице при умножении на s5, должно быть: a*s5 = 2, b*s5 = 6.
/// Здесь она исправлена.
const ACT: [&str; 55] = [
    //        (l2, r1)    (l2, r2)    (l1, r3)    (l1, r4)    (l2, r5)
    /* a */     "1",        "2",        "6",        "5",        "2",
    /* b */     "4",        "5",        "3",        "1",        "6",
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

#[allow(unused)]
#[rustfmt::skip]
/// Пример таблицы Кэли простого полигона над полугруппой левых нулей (соответствует таблице I)
const SIMPLE_ACT: [&str; 8] = [
    //           l1    l2
    /* a */     "2",  "1",
    /* b */     "1",  "2",
    /* 1 */     "1",  "1",
    /* 2 */     "2",  "2"
];

fn main() {
    let elements = ["a", "b", "1", "2", "3", "4", "5", "6", "7", "8", "9"];

    // Создаём полигон
    let act = Act::from_str_table(&elements, &ACT);

    let congruence_set = act.new_congruence_set();

    let lattice = Lattice::from(congruence_set.as_slice());

    partitions_set_print_dot(&congruence_set);

    println!("Решётка содержит {} элементов", congruence_set.len());
    println!("Тест на модулярность: {}", lattice.is_modular());
}

/// Печатает решётку конгруэнций в формате Graphiz Dot
fn partitions_set_print_dot(partitions_set: &[Partition]) {
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
