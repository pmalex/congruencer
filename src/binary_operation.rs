use rayon::prelude::{IntoParallelIterator, IntoParallelRefMutIterator, ParallelIterator};
use std::fmt::Display;

/// Baisic element type of a binary operation.
pub type Element = usize;

pub struct BinaryOperation {
    cayley_table: Vec<Element>,
    number_of_elements: usize,
}

impl BinaryOperation {
    pub fn new(number_of_elements: usize) -> Self {
        Self {
            cayley_table: vec![usize::MAX; number_of_elements * number_of_elements],
            number_of_elements,
        }
    }

    pub fn from_cayley_table(table: &[Element], number_of_elements: usize) -> Self {
        assert_eq!(table.len() % number_of_elements, 0);

        Self {
            cayley_table: table.to_owned(),
            number_of_elements,
        }
    }

    #[inline(always)]
    /// Apply a binary operation to an arguments.
    pub fn apply(&self, a: Element, b: Element) -> Element {
        debug_assert!(a < self.number_of_elements);
        debug_assert!(b < self.number_of_elements);

        self.cayley_table[a * self.number_of_elements + b]
    }

    #[inline(always)]
    pub fn get_number_of_elements(&self) -> usize {
        self.number_of_elements
    }

    pub fn is_idempotent(&self) -> bool {
        (0..self.number_of_elements)
            .into_par_iter()
            .all(|a| self.cayley_table[a * self.number_of_elements + a] == a)
    }

    pub fn is_commutative(&self) -> bool {
        (1..self.number_of_elements).into_par_iter().all(|a| {
            (0..a)
                .into_par_iter()
                .all(|b| self.apply(a, b) == self.apply(b, a))
        })
    }

    pub fn is_associative(&self) -> bool {
        let number_of_elements_squared = self.number_of_elements * self.number_of_elements;

        (0..number_of_elements_squared * self.number_of_elements)
            .into_par_iter()
            .all(|index_3d| {
                let c = index_3d / number_of_elements_squared;

                let index_2d = index_3d % number_of_elements_squared;

                let a = index_2d / self.number_of_elements;
                let b = index_2d % self.number_of_elements;

                self.apply(self.apply(a, b), c) == self.apply(a, self.apply(b, c))
            })
    }

    #[inline]
    pub fn iter_mut(&mut self) -> std::slice::IterMut<Element> {
        self.cayley_table.iter_mut()
    }

    #[inline]
    pub fn par_iter_mut(&mut self) -> rayon::slice::IterMut<Element> {
        self.cayley_table.par_iter_mut()
    }
}

impl Display for BinaryOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.cayley_table.len() < self.number_of_elements * self.number_of_elements {
            Err(std::fmt::Error)
        } else {
            let width = (self.number_of_elements as f32).log10().ceil() as usize + 1;

            write!(f, "{:>width$}", "", width = width + 2).unwrap();
            for column in 0..self.number_of_elements {
                write!(f, "{:>width$}", column, width = width).unwrap();
            }
            writeln!(f).unwrap();

            write!(f, "{:->width$}", "", width = width + 2).unwrap();
            for _ in 0..self.number_of_elements {
                write!(f, "{:->width$}", "", width = width).unwrap();
            }
            writeln!(f).unwrap();

            for row in 0..self.number_of_elements {
                write!(f, "{:>width$} |", row, width = width).unwrap();

                for column in 0..self.number_of_elements {
                    write!(
                        f,
                        "{:>width$}",
                        self.cayley_table[row * self.number_of_elements + column],
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
    use crate::binary_operation::BinaryOperation;
    use itertools::{iproduct, Itertools};

    /// Simple (and slow) implementation.
    fn is_commutative_slow(binary_operation: &BinaryOperation) -> bool {
        (1..binary_operation.get_number_of_elements()).all(|element_a_index| {
            (1..binary_operation.get_number_of_elements()).all(|element_b_index| {
                binary_operation.apply(element_a_index, element_b_index)
                    == binary_operation.apply(element_b_index, element_a_index)
            })
        })
    }

    /// Simple (and slow) implementation.
    fn is_associative_slow(binary_operation: &BinaryOperation) -> bool {
        iproduct!(
            (0..binary_operation.get_number_of_elements()),
            (0..binary_operation.get_number_of_elements()),
            (0..binary_operation.get_number_of_elements())
        )
        .collect_vec()
        .into_iter()
        .all(|(a, b, c)| {
            binary_operation.apply(binary_operation.apply(a, b), c)
                == binary_operation.apply(a, binary_operation.apply(b, c))
        })
    }

    #[test]
    fn is_commutative() {
        let binary_operation = BinaryOperation::new(20);

        assert_eq!(
            binary_operation.is_commutative(),
            is_commutative_slow(&binary_operation)
        );
    }

    #[test]
    fn is_associative() {
        let mut binary_operation = BinaryOperation::new(20);

        for element in binary_operation.iter_mut() {
            *element = 0;
        }

        assert_eq!(
            binary_operation.is_associative(),
            is_associative_slow(&binary_operation)
        );
    }
}
