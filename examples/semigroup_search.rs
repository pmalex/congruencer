//! Поиск полугрупп.

use congruencer::magma::{iterator::MagmaIterator, Magma};

const SEMIGROUP_SIZE: usize = 5;

/// Случайный поиск. Эффективен при большом размере полугруппы.
fn _random_search() {
    for _ in 0..10_000_000 {
        let magma = Magma::random(SEMIGROUP_SIZE);

        if magma.is_associative() {
            // print!("+");
            // std::io::Write::flush(&mut std::io::stdout()).expect("Coder error");

            println!("{}", magma);
        }
    }
}

/// Поиск полным перебором. Эффективен при малом размере полгруппы.
fn _brute_force() {
    for _ in MagmaIterator::new(SEMIGROUP_SIZE)
        .filter(|f| f.is_associative())
        .take(100)
    {
        print!("+");
        std::io::Write::flush(&mut std::io::stdout()).expect("Coder error");
    }
    println!();
}

fn main() {
    println!("Поиск полугрупп размера {}", SEMIGROUP_SIZE);

    _random_search();
    // _brute_force();
}
