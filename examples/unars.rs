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
const UNAR_5: [[usize; 4]; 5] = [
    //        a   a^2  a^3  a^4
    /* x */ [ 1,   2,   3,   4 ],
    /* y */ [ 2,   3,   4,   4 ],
    /* z */ [ 3,   4,   4,   4 ],
    /* u */ [ 4,   4,   4,   4 ],
    /* v */ [ 4,   4,   4,   4 ],
];

#[rustfmt::skip]
/// Унар из шести элементов.
const UNAR_6: [[usize; 5]; 6] = [
    //        a   a^2  a^3  a^4  a^5
    /* x */ [ 1,   2,   3,   4,   5 ],
    /* y */ [ 2,   3,   4,   5,   5 ],
    /* z */ [ 3,   4,   5,   5,   5 ],
    /* u */ [ 4,   5,   5,   5,   5 ],
    /* v */ [ 5,   5,   5,   5,   5 ],
    /* w */ [ 5,   5,   5,   5,   5 ],
];

// fn print_unar_congruences(act_elements: &[&str], unar_table: &[&str]) {
//     let unar = Act::new(act_elements, unar_table);

//     let congruence_set = unar.new_congruence_set();

//     print!("{{");

//     for partition in congruence_set {
//         partition.print(act_elements);
//         print!("; ")
//     }

//     println!("}}");
// }

fn main() {
    print!("Конгруэнции унара из четырёх элементов: ");
    // print_unar_congruences(UNAR_4, "xyzu");

    // print!("Конгруэнции унара из пяти элементов: ");
    // print_unar_congruences(UNAR_5, "xyzuv");

    // print!("Конгруэнции унара из шести элементов: ");
    // print_unar_congruences(UNAR_6, "xyzuvw");
}
