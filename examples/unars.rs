//! Унар - это полигон над свободной циклической подугрупой S = {a, a^2, a^3, ...}.
//!
//! В данном примере строится решётка конгруэнций различных унаров.

use congruencer::{act::Act, congruence::Congruence};

#[rustfmt::skip]
/// Унар из четырёх элементов.
const UNAR_4: [&str; 12] = [
    //       a   a^2  a^3
    /* x */ "y", "z", "u",
    /* y */ "z", "u", "u",
    /* z */ "u", "u", "u",
    /* u */ "u", "u", "u",
];

#[rustfmt::skip]
/// Унар из пяти элементов.
const UNAR_5: [&str; 20] = [
    //        a   a^2  a^3  a^4
    /* x */  "y", "z", "u", "v",
    /* y */  "z", "u", "v", "v",
    /* z */  "u", "v", "v", "v",
    /* u */  "v", "v", "v", "v",
    /* v */  "v", "v", "v", "v",
];

#[rustfmt::skip]
/// Унар из шести элементов.
const UNAR_6: [&str; 30] = [
    //        a   a^2  a^3  a^4  a^5
    /* x */  "y", "z", "u", "v", "w",
    /* y */  "z", "u", "v", "w", "w",
    /* z */  "u", "v", "w", "w", "w",
    /* u */  "v", "w", "w", "w", "w",
    /* v */  "w", "w", "w", "w", "w",
    /* w */  "w", "w", "w", "w", "w",
];

fn print_unar_congruences(act_elements: &[&str], unar_table: &[&str]) {
    let unar = Act::new(act_elements, unar_table);

    let congruence_set = unar.new_congruence_set(act_elements);

    print!("{{");

    for partition in congruence_set {
        print!("{}, ", partition);
    }

    println!("}}");
}

fn main() {
    print!("Конгруэнции унара из четырёх элементов: ");
    print_unar_congruences(&["x", "y", "z", "u"], &UNAR_4);

    print!("Конгруэнции унара из пяти элементов: ");
    print_unar_congruences(&["x", "y", "z", "u", "v"], &UNAR_5);

    print!("Конгруэнции унара из шести элементов: ");
    print_unar_congruences(&["x", "y", "z", "u", "v", "w"], &UNAR_6);
}
