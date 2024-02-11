//! Унар - это полигон над свободной циклической подугрупой S = {a, a^2, a^3, ...}.
//!
//! В данном примере строится решётка конгруэнций различных унаров.

use congruencer::{act::fixed::Act, congruence::Congruence};

#[rustfmt::skip]
/// Унар из четырёх элементов.
const UNAR_4: [[usize; 3]; 4] = [
    //        a   a^2  a^3
    /* x */ [ 1,   2,   3 ],
    /* y */ [ 2,   3,   3 ],
    /* z */ [ 3,   3,   3 ],
    /* u */ [ 3,   3,   3 ],
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

fn print_unar_congruences<const ACT_SIZE: usize, const SEMIGROUP_SIZE: usize>(
    unar_table: [[usize; SEMIGROUP_SIZE]; ACT_SIZE],
    alphabet: &str,
) {
    let unar = Act::new(unar_table);

    let congruence_set = unar.new_congruence_set();

    print!("{{");

    for partition in congruence_set {
        partition.print(alphabet);
        print!("; ")
    }

    println!("}}");
}

fn main() {
    print!("Конгруэнции унара из четырёх элементов: ");
    print_unar_congruences(UNAR_4, "xyzu");

    print!("Конгруэнции унара из пяти элементов: ");
    print_unar_congruences(UNAR_5, "xyzuv");

    print!("Конгруэнции унара из шести элементов: ");
    print_unar_congruences(UNAR_6, "xyzuvw");
}
