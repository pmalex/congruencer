//! Binary operation implementation with statically known size.

pub mod iterator;

use rayon::prelude::{IntoParallelIterator, ParallelIterator};

use crate::ElementIndex;

#[derive(Clone)]
pub struct BinaryOperation<const SIZE: usize>(pub [[ElementIndex; SIZE]; SIZE]);

impl<const SIZE: usize> BinaryOperation<SIZE> {
    pub fn zero() -> Self {
        Self([[0; SIZE]; SIZE])
    }

    #[inline(always)]
    /// Applies a binary operation to an arguments.
    pub fn call(&self, a: ElementIndex, b: ElementIndex) -> ElementIndex {
        self.0[a][b]
    }

    /// Creates a binary operation based on existent cayley table.
    pub fn from_cayley_table(table: &[[ElementIndex; SIZE]; SIZE]) -> Self {
        for row in table.iter() {
            for &x in row.iter() {
                assert!(x < SIZE);
            }
        }

        Self(*table)
    }

    pub fn is_associative(&self) -> bool {
        (0..SIZE * SIZE * SIZE).into_par_iter().all(|index_3d| {
            let c = index_3d / (SIZE * SIZE);

            let index_2d = index_3d % (SIZE * SIZE);

            let a = index_2d / SIZE;
            let b = index_2d % SIZE;

            self.call(self.call(a, b), c) == self.call(a, self.call(b, c))
        })
    }

    pub fn is_commutative(&self) -> bool {
        (1..SIZE).into_par_iter().all(|a| {
            (0..a)
                .into_par_iter()
                .all(|b| self.call(a, b) == self.call(b, a))
        })
    }
}

impl<const SIZE: usize> std::fmt::Display for BinaryOperation<SIZE> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let width = (SIZE as f32).log10().ceil() as usize + 1;

        // Printing the first row.
        write!(f, "{:>width$}", "", width = width + 2).unwrap();
        for column in 0..SIZE {
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
        for _ in 0..SIZE {
            write!(f, "{:->width$}", "", width = width).unwrap();
        }
        writeln!(f).unwrap();

        for row in 0..SIZE {
            write!(f, "{:>width$} |", (b'a' + row as u8) as char, width = width).unwrap();

            for column in 0..SIZE {
                write!(
                    f,
                    "{:>width$}",
                    (b'a' + self.0[row][column] as u8) as char,
                    width = width
                )
                .unwrap();
            }

            writeln!(f).unwrap();
        }

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::BinaryOperation;
    use itertools::{iproduct, Itertools};

    /// Simple (and slow) implementation.
    fn is_commutative_slow<const SIZE: usize>(binary_operation: &BinaryOperation<SIZE>) -> bool {
        (1..SIZE).all(|element_a_index| {
            (1..SIZE).all(|element_b_index| {
                binary_operation.call(element_a_index, element_b_index)
                    == binary_operation.call(element_b_index, element_a_index)
            })
        })
    }

    /// Simple (and slow) implementation.
    fn is_associative_slow<const SIZE: usize>(binary_operation: &BinaryOperation<SIZE>) -> bool {
        iproduct!((0..SIZE), (0..SIZE), (0..SIZE))
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
        let binary_operation = BinaryOperation::<20>::zero();

        assert_eq!(
            binary_operation.is_commutative(),
            is_commutative_slow(&binary_operation)
        );
    }

    #[test]
    fn is_associative() {
        let binary_operation = BinaryOperation::<20>::zero();

        assert_eq!(
            binary_operation.is_associative(),
            is_associative_slow(&binary_operation)
        );
    }
}
