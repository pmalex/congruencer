//! Унар - это полигон над свободной циклической подугрупой S = {a, a^2, a^3, ...}.
//!
//! В данном примере строится решётка конгруэнций простейших унаров - полуцепей.

use congruencer::act::Act;

#[rustfmt::skip]
/// Унар из четырёх элементов:
/// 
/// x → y → z → u ↺
const UNAR_4: [&str; 12] = [
    //       a   a^2  a^3
    /* x */ "y", "z", "u",
    /* y */ "z", "u", "u",
    /* z */ "u", "u", "u",
    /* u */ "u", "u", "u",
];

#[rustfmt::skip]
/// Унар из пяти элементов:
/// 
/// x → y → z → u → v ↺
const UNAR_5: [&str; 20] = [
    //        a   a^2  a^3  a^4
    /* x */  "y", "z", "u", "v",
    /* y */  "z", "u", "v", "v",
    /* z */  "u", "v", "v", "v",
    /* u */  "v", "v", "v", "v",
    /* v */  "v", "v", "v", "v",
];

#[rustfmt::skip]
/// Унар из шести элементов:
/// 
/// x → y → z → u → v → w ↺
const UNAR_6: [&str; 30] = [
    //        a   a^2  a^3  a^4  a^5
    /* x */  "y", "z", "u", "v", "w",
    /* y */  "z", "u", "v", "w", "w",
    /* z */  "u", "v", "w", "w", "w",
    /* u */  "v", "w", "w", "w", "w",
    /* v */  "w", "w", "w", "w", "w",
    /* w */  "w", "w", "w", "w", "w",
];

#[rustfmt::skip]
/// Цикл из трёх элементов
const CYCLE3: [&str; 3] = [
    //         a
    /* x1 */  "x2",
    /* x2 */  "x3",
    /* x3 */  "x1",
];

#[rustfmt::skip]
/// Цикл из четырёх элементов
const CYCLE4: [&str; 8] = [
    //         a     a^2
    /* x1 */  "x2", "x3",
    /* x2 */  "x3", "x4",
    /* x3 */  "x4", "x1",
    /* x4 */  "x1", "x2"
];

/// Функция, порождающая конечный унар-полуцепь из n элементов:
///
/// x_1 → x_2 → x_3 → ... → x_n ↺
fn gen_semichain(n: usize, prefix: &str) -> Act {
    assert!(n > 0);

    // Формируем алфавит
    let unar_elements_names = (1..=n)
        .map(|k| format!("{prefix}{k}"))
        .collect::<Vec<String>>();

    // Преобразовываем Vec<String> -> Vec<&str>
    let unar_elements_names_ref = unar_elements_names
        .iter()
        .map(String::as_str)
        .collect::<Vec<&str>>();

    let unar_table: Vec<String> = (1..n)
        .map(|k| format!("{prefix}{}", k + 1))
        .chain(std::iter::once(format!("{prefix}{n}")))
        .collect();

    // Преобразовываем String -> &str
    let unar_table_ref = unar_table.iter().map(String::as_str).collect::<Vec<&str>>();

    Act::from_str_table(&unar_elements_names_ref, &unar_table_ref)
}

/// Порождает цикл (унар) длиной n.
fn gen_cycle(n: usize, prefix: &str) -> Act {
    assert!(n > 0);
    assert!(!prefix.is_empty());

    // Формируем алфавит
    let unar_elements_names = (1..=n)
        .map(|k| format!("{prefix}{k}"))
        .collect::<Vec<String>>();

    // Преобразовываем Vec<String> -> Vec<&str>
    let unar_elements_names_ref = unar_elements_names
        .iter()
        .map(String::as_str)
        .collect::<Vec<&str>>();

    let unar_table: Vec<String> = (1..n)
        .map(|k| format!("{prefix}{}", k + 1))
        .chain(std::iter::once(format!("{prefix}1")))
        .collect();

    // Преобразовываем String -> &str
    let unar_table_ref = unar_table.iter().map(String::as_str).collect::<Vec<&str>>();

    Act::from_str_table(&unar_elements_names_ref, &unar_table_ref)
}

/// Печатает множество конгруэнций унара.
fn print_unar_congruences(unar: &Act) {
    let unar_congruence_set = unar.new_congruence_set();

    partitions_set_print_dot(&unar_congruence_set);

    print!("{{");

    for named_partition in unar_congruence_set {
        print!("{}, ", named_partition);
    }

    println!("}}");
}

fn main() {
    let unar_elements = ["x", "y", "z", "u", "v", "w"];

    print!("Конгруэнции унара из четырёх элементов: ");
    print_unar_congruences(&Act::from_str_table(&unar_elements[0..4], &UNAR_4));

    print!("Конгруэнции унара из пяти элементов: ");
    print_unar_congruences(&Act::from_str_table(&unar_elements[0..5], &UNAR_5));

    print!("Конгруэнции унара из шести элементов: ");
    print_unar_congruences(&Act::from_str_table(&unar_elements[0..6], &UNAR_6));

    print!("Конгруэнции унара-полуцепи из 9 элементов: ");
    print_unar_congruences(&gen_semichain(9, "x"));

    print!("Конгруэнции 3-цикла: ");
    print_unar_congruences(&Act::from_str_table(&["x1", "x2", "x3"], &CYCLE3));

    print!("Конгруэнции 4-цикла: ");
    print_unar_congruences(&Act::from_str_table(&["x1", "x2", "x3", "x4"], &CYCLE4));

    for k in 1..13 {
        print!("Конгруэнции {}-цикла: ", k);
        print_unar_congruences(&gen_cycle(k, "x"));
    }

    println!("Копроизведение 3-цикла и 4-цикла: ");
    let mut act_1 = gen_cycle(3, "x");
    let act_2 = gen_cycle(4, "y");
    act_1.coproduct(&act_2);
    println!("{}", act_1);
    print_unar_congruences(&act_1);

    println!("Копроизведение полуцепей: ");
    let mut act_1 = gen_semichain(3, "x");
    let act_2 = gen_semichain(3, "y");
    act_1.coproduct(&act_2);
    println!("{}", act_1);
    print_unar_congruences(&act_1);
}

/// Prints a partitions set in the Graphiz Dot format.
fn partitions_set_print_dot(partitions_set: &[congruencer::partition::Partition]) {
    println!("graph lattice {{");
    println!("\trankdir = TB;");
    println!("\tratio = 0.75;");
    println!("\tnode[shape = none];");
    println!();

    for partition in partitions_set {
        for nearest_upper_bound in
            congruencer::poset::nearest_incomparable_lower_bounds(partitions_set, partition)
        {
            println!("\t\"{}\" -- \"{}\"", partition, nearest_upper_bound)
        }
    }

    println!("}}");
}
