use itertools::{iproduct, Itertools};
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

    #[inline(always)]
    /// Update Cayley table by an absolute index.
    pub fn set(&mut self, absolute_index: usize, c: Element) {
        debug_assert!(absolute_index < self.number_of_elements);
        debug_assert!(c < self.number_of_elements);

        self.cayley_table[absolute_index] = c;
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

    // TODO: можно оптимизировать
    pub fn is_commutative(&self) -> bool {
        (0..self.number_of_elements)
            .cartesian_product(0..self.number_of_elements)
            .collect_vec()
            .into_par_iter()
            .all(|(a, b)| self.apply(a, b) == self.apply(b, a))
    }

    pub fn is_commutative_new(&self) -> bool {
        (1..self.number_of_elements)
            .into_par_iter()
            .all(|row_number| {
                (0..row_number).into_par_iter().all(|column_number| {
                    self.apply(row_number, column_number) == self.apply(column_number, row_number)
                })
            })
    }

    pub fn is_associative(&self) -> bool {
        iproduct!(
            (0..self.number_of_elements),
            (0..self.number_of_elements),
            (0..self.number_of_elements)
        )
        .collect_vec()
        .into_par_iter()
        .all(|(a, b, c)| self.apply(self.apply(a, b), c) == self.apply(a, self.apply(b, c)))
    }

    #[inline]
    pub fn par_iter_mut(&mut self) -> rayon::slice::IterMut<usize> {
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

#[test]
fn trivial() {
    assert_eq!(BinaryOperation::new(20).is_commutative_new(), true);
}
