use itertools::{
    FoldWhile::{Continue, Done},
    Itertools,
};

use crate::partition::BaseElement;

pub struct Act {
    cayley_table: Vec<BaseElement>,
    columns: usize,
}

impl Act {
    pub fn new_from_cayley_table(table: &[BaseElement], columns: usize) -> Self {
        assert!(columns > 0 && columns <= table.len());
        assert!(table.len() > 0 && table.len() % columns == 0);

        Self {
            cayley_table: table.to_owned(),
            columns,
        }
    }

    #[inline]
    pub fn get_semigroup_size(&self) -> usize {
        self.cayley_table.len() / self.columns
    }

    /// Multiply a set of an act's elements by a semigroup element.
    pub fn m(&self, class: BaseElement, semigroup_element_index: usize) -> BaseElement {
        // Check that the number (class) and the table are compatible.
        debug_assert!(
            ((BaseElement::BITS - class.leading_zeros() - 1) as usize) // Position of the most significant unity.
            <
            self.cayley_table.len() / self.columns, // Number of rows.
            "The class({:b}) is too big, the table does not contains enough values",
            class
        );

        debug_assert!(semigroup_element_index < self.columns);

        let (result, _) = (0..BaseElement::BITS as usize)
            .fold_while((0 as BaseElement, class), |(accumulator, class), i| {
                if class == 0 {
                    Done((accumulator, class))
                } else {
                    if class & 1 != 0 {
                        let result_position =
                            1 << self.cayley_table[i * self.columns + semigroup_element_index];

                        Continue((accumulator | result_position, class >> 1))
                    } else {
                        Continue((accumulator, class >> 1))
                    }
                }
            })
            .into_inner();

        result
    }
}

#[test]
fn act_multiplication() {
    #[rustfmt::skip]
    let cayley_table = [
        /* 0 */ 1, 0, 3, 2, 2 as BaseElement,
        /* 1 */ 3, 1, 1, 0, 2,
        /* 2 */ 2, 3, 3, 1, 2,
        /* 3 */ 3, 3, 2, 3, 2,
    ];

    let act = Act::new_from_cayley_table(&cayley_table, 5);

    assert_eq!(act.m(0b01010, 0), 0b01000);
    assert_eq!(act.m(0b01010, 1), 0b01010);
    assert_eq!(act.m(0b01010, 2), 0b00110);
    assert_eq!(act.m(0b01010, 3), 0b01001);
    assert_eq!(act.m(0b01101, 4), 0b00100);
}
