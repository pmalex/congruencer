//! Унар - это полигон над свободной циклической подугрупой S = {a, a^2, a^3, ...}.
//!
//! В данном примере строится решётка конгруэнций простейших унаров - полуцепей.

use congruencer::act::named_act::NamedAct;

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

/// Функция, порождающая унар-полуцепь из n элементов:
/// 
/// x_1 → x_2 → x_3 → ... → x_n ↺
fn get_unar_semichain(n: usize) -> NamedAct {
    assert!(n > 0);

    // Формируем алфавит
    let unar_elements_names = (0..n).map(|k| format!("x{}", k)).collect::<Vec<String>>();

    let mut unar_table = Vec::<String>::with_capacity(n * (n - 1));

    // Формируем таблицу унара
    for i in 0..n {
        for j in i..n - 1 {
            unar_table.push(format!("x{}", j + 1));
        }

        // Добавляем остаток
        for _ in 0..i {
            unar_table.push(format!("x{}", n - 1));
        }
    }

    assert_eq!(unar_table.len(), n * (n - 1));

    // Преобразовываем String -> &str
    let unar_table_ref = unar_table.iter().map(|s| s.as_str()).collect::<Vec<&str>>();

    NamedAct::from_string_table(unar_elements_names, &unar_table_ref)
}

/// Печатает множество конгруэнций унара.
fn print_unar_congruences(unar: NamedAct) {
    let unar_congruence_set = unar.new_congruence_set();

    print!("{{");

    for named_partition in unar_congruence_set {
        print!("{}, ", named_partition);
    }

    println!("}}");
}

fn main() {
    let unar_elements = vec![
        "x".to_string(),
        "y".to_string(),
        "z".to_string(),
        "u".to_string(),
        "v".to_string(),
        "w".to_string(),
    ];

    print!("Конгруэнции унара из четырёх элементов: ");
    print_unar_congruences(NamedAct::from_string_table(
        unar_elements[0..4].to_vec(),
        &UNAR_4,
    ));

    print!("Конгруэнции унара из пяти элементов: ");
    print_unar_congruences(NamedAct::from_string_table(
        unar_elements[0..5].to_vec(),
        &UNAR_5,
    ));

    print!("Конгруэнции унара из шести элементов: ");
    print_unar_congruences(NamedAct::from_string_table(
        unar_elements[0..6].to_vec(),
        &UNAR_6,
    ));

    print_unar_congruences(get_unar_semichain(9));
}
