//! Binary operation implementation with dynamic size.

pub mod iterator;

use rayon::prelude::{IntoParallelIterator, ParallelIterator};
use std::fmt::Display;

use crate::{long_number::LongNumber, ElementIndex};

/// Magma, Binar or Groupoid.
pub struct Magma {
    pub cayley_table: Vec<ElementIndex>,

    /// Размер множества, на котором определена бинарная операция.
    pub set_size: usize,
}

impl Magma {
    pub fn zero(set_size: usize) -> Self {
        Self {
            cayley_table: vec![0; set_size * set_size],
            set_size,
        }
    }

    /// Создаёт бинарную операцию из переданной таблицы Кэли.
    pub fn from_cayley_table(table: &[ElementIndex]) -> Self {
        let set_size = (table.len() as f32).sqrt() as usize;

        assert_eq!(set_size * set_size, table.len());

        Self {
            cayley_table: table.to_owned(),
            set_size,
        }
    }

    #[inline]
    /// Создаёт случайную бинарную операцию.
    pub fn random(set_size: usize) -> Self {
        LongNumber::random(set_size * set_size, set_size).into()
    }

    #[inline(always)]
    /// Applies a binary operation to an arguments.
    pub fn call(&self, a: ElementIndex, b: ElementIndex) -> ElementIndex {
        debug_assert!(a < self.set_size);
        debug_assert!(b < self.set_size);
        debug_assert!(a * self.set_size + b < self.cayley_table.len());

        unsafe { *self.cayley_table.get_unchecked(a * self.set_size + b) }
    }

    pub fn is_idempotent(&self) -> bool {
        (0..self.set_size)
            .into_par_iter()
            .all(|a| self.cayley_table[a * self.set_size + a] == a)
    }

    pub fn is_commutative(&self) -> bool {
        (1..self.set_size).into_par_iter().all(|a| {
            (0..a)
                .into_par_iter()
                .all(|b| self.call(a, b) == self.call(b, a))
        })
    }

    pub fn is_associative(&self) -> bool {
        let number_of_elements_squared = self.set_size * self.set_size;

        (0..number_of_elements_squared * self.set_size)
            .into_par_iter()
            .all(|index_3d| {
                let c = index_3d / number_of_elements_squared;

                let index_2d = index_3d % number_of_elements_squared;

                let a = index_2d / self.set_size;
                let b = index_2d % self.set_size;

                self.call(self.call(a, b), c) == self.call(a, self.call(b, c))
            })
    }
}

impl From<LongNumber> for Magma {
    fn from(long_number: LongNumber) -> Self {
        let LongNumber { digits, radix } = long_number;

        // The number of elements in a binary operation.
        let set_size = {
            let sqrt = (digits.len() as f32).sqrt() as usize;

            debug_assert_eq!(sqrt * sqrt, digits.len());

            sqrt
        };

        debug_assert!(radix >= set_size);

        Self {
            cayley_table: digits,
            set_size: radix,
        }
    }
}

impl Display for Magma {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.cayley_table.len() < self.set_size * self.set_size {
            Err(std::fmt::Error)
        } else {
            let width = (self.set_size as f32).log10().ceil() as usize + 1;

            // Printing the first row.
            write!(f, "{:>width$}", "", width = width + 2).unwrap();
            for column in 0..self.set_size {
                write!(
                    f,
                    "{:>width$}",
                    (b'a' + column as u8) as char,
                    width = width
                )
                .unwrap();
            }
            writeln!(f).unwrap();

            write!(f, "{:->width$}", "", width = width + 2).unwrap();
            for _ in 0..self.set_size {
                write!(f, "{:->width$}", "", width = width).unwrap();
            }
            writeln!(f).unwrap();

            for row in 0..self.set_size {
                write!(f, "{:>width$} |", (b'a' + row as u8) as char, width = width).unwrap();

                for column in 0..self.set_size {
                    write!(
                        f,
                        "{:>width$}",
                        (b'a' + self.cayley_table[row * self.set_size + column] as u8) as char,
                        width = width
                    )
                    .unwrap();
                }

                writeln!(f).unwrap();
            }

            Ok(())
        }
    }
}
