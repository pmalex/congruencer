//! Binary operation implementation with dynamic size.

pub mod iterator;

use rayon::prelude::{IntoParallelIterator, IntoParallelRefMutIterator, ParallelIterator};
use std::fmt::Display;

use crate::ElementIndex;

pub struct BinaryOperation {
    cayley_table: Vec<ElementIndex>,
    set_size: usize,
}

impl BinaryOperation {
    pub fn zero(set_size: usize) -> Self {
        Self {
            cayley_table: vec![0; set_size * set_size],
            set_size,
        }
    }

    pub fn from_cayley_table(table: &[ElementIndex]) -> Self {
        let set_size = (table.len() as f32).sqrt() as usize;

        assert_eq!(set_size * set_size, table.len());

        Self {
            cayley_table: table.to_owned(),
            set_size,
        }
    }

    #[inline(always)]
    /// Applies a binary operation to an arguments.
    pub fn call(&self, a: ElementIndex, b: ElementIndex) -> ElementIndex {
        debug_assert!(a < self.set_size);
        debug_assert!(b < self.set_size);

        self.cayley_table[a * self.set_size + b]
    }

    #[inline(always)]
    pub fn get_set_size(&self) -> usize {
        self.set_size
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

    #[inline]
    pub fn iter_mut(&mut self) -> std::slice::IterMut<ElementIndex> {
        self.cayley_table.iter_mut()
    }

    #[inline]
    pub fn par_iter_mut(&mut self) -> rayon::slice::IterMut<ElementIndex> {
        self.cayley_table.par_iter_mut()
    }
}

impl Display for BinaryOperation {
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

#[cfg(test)]
mod test {
    use super::BinaryOperation;
    use itertools::{iproduct, Itertools};

    /// Simple (and slow) implementation.
    fn is_commutative_slow(binary_operation: &BinaryOperation) -> bool {
        (1..binary_operation.get_set_size()).all(|element_a_index| {
            (1..binary_operation.get_set_size()).all(|element_b_index| {
                binary_operation.call(element_a_index, element_b_index)
                    == binary_operation.call(element_b_index, element_a_index)
            })
        })
    }

    /// Simple (and slow) implementation.
    fn is_associative_slow(binary_operation: &BinaryOperation) -> bool {
        iproduct!(
            (0..binary_operation.get_set_size()),
            (0..binary_operation.get_set_size()),
            (0..binary_operation.get_set_size())
        )
        .collect_vec()
        .into_iter()
        .all(|(a, b, c)| {
            binary_operation.call(binary_operation.call(a, b), c)
                == binary_operation.call(a, binary_operation.call(b, c))
        })
    }

    #[test]
    fn is_commutative() {
        // TODO: вбить вместо нулевых конкретные бинарные операции, сгенерированные итератором по ним.
        let binary_operation = BinaryOperation::zero(20);

        assert_eq!(
            binary_operation.is_commutative(),
            is_commutative_slow(&binary_operation)
        );
    }

    #[test]
    fn is_associative() {
        let mut binary_operation = BinaryOperation::zero(20);

        for element in binary_operation.iter_mut() {
            *element = 0;
        }

        assert_eq!(
            binary_operation.is_associative(),
            is_associative_slow(&binary_operation)
        );
    }
}
