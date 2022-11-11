use rayon::prelude::{IntoParallelIterator, IntoParallelRefIterator, ParallelIterator};
use std::fmt::Display;

use crate::act::Act;

pub type BaseElement = u16;

pub struct Partition {
    raw_partition: Vec<BaseElement>,
    partition_size: usize,
}

impl Partition {
    #[inline]
    pub fn new(raw_partition: &[BaseElement], partition_size: usize) -> Self {
        Self {
            raw_partition: raw_partition.to_vec(),
            partition_size,
        }
    }

    /// Generates all possible partitions of a n-element set.
    pub fn new_partition_set(set_size: usize) -> Vec<Partition> {
        assert!(set_size > 0 && set_size <= BaseElement::BITS as usize);

        (1..set_size).fold(vec![Partition::new(&[1], 1)], |previous_generation, _| {
            previous_generation
                .into_par_iter()
                .flat_map(|partition| partition.expand())
                .collect::<Vec<Partition>>()
        })
    }

    pub fn new_congruence_set(set_size: usize, act: &Act) -> Vec<Partition> {
        Self::new_partition_set(set_size)
            .into_par_iter()
            .filter(|partition| partition.is_congruence(act))
            .collect()
    }

    /// Generate a new set of partitions by adding a new element to the current partition.
    fn expand(&self) -> Vec<Self> {
        let Self {
            raw_partition,
            partition_size,
        } = self;

        // Binary repesentation of a new element that should be added to the partition.
        let new_element = 1 << partition_size;

        (0..raw_partition.len())
            .into_par_iter()
            .map(|class_number| {
                let mut new_raw_partition = raw_partition.clone();

                new_raw_partition[class_number] |= new_element;

                Self {
                    raw_partition: new_raw_partition,
                    partition_size: partition_size + 1,
                }
            })
            .chain(rayon::iter::once({
                let mut new_raw_partition =
                    Vec::<BaseElement>::with_capacity(raw_partition.len() + 1);

                new_raw_partition.clone_from(raw_partition);

                new_raw_partition.push(new_element);

                Self {
                    raw_partition: new_raw_partition,
                    partition_size: partition_size + 1,
                }
            }))
            .collect::<Vec<Partition>>()
    }

    /// Returns true, if a partition is congruence.
    pub fn is_congruence(&self, act: &Act) -> bool {
        self.raw_partition.par_iter().all(|&class| {
            debug_assert!(class > 0);

            (0..act.get_semigroup_size())
                .into_par_iter()
                .all(|semigroup_element_index| {
                    let new_class = act.m(class, semigroup_element_index);

                    debug_assert!(new_class > 0);

                    match self
                        .raw_partition
                        .par_iter()
                        .find_any(|&&old_class| (new_class | old_class) == old_class)
                    {
                        Some(_) => true,
                        None => false,
                    }
                })
        })
    }
}

impl PartialEq for Partition {
    fn eq(&self, _other: &Self) -> bool {
        unimplemented!()
    }
}

impl PartialOrd for Partition {
    fn partial_cmp(&self, _other: &Self) -> Option<std::cmp::Ordering> {
        unimplemented!()
    }

    fn le(&self, other: &Self) -> bool {
        self.raw_partition.par_iter().all(|&class| {
            match other
                .raw_partition
                .iter()
                .find(|&&other_class| (class | other_class) == other_class)
            {
                Some(_) => true,
                None => false,
            }
        })
    }

    #[inline(always)]
    fn ge(&self, other: &Self) -> bool {
        other.le(self)
    }

    fn gt(&self, _other: &Self) -> bool {
        unimplemented!();
    }

    fn lt(&self, _other: &Self) -> bool {
        unimplemented!();
    }
}

impl Display for Partition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.partition_size == self.raw_partition.len() {
            write!(f, "Δ").unwrap();
        } else if self.raw_partition.len() == 1 {
            write!(f, "∇").unwrap();
        } else {
            for class in &self.raw_partition {
                if class.is_power_of_two() {
                    continue; // Do not print the powers of two, because it's a one-element class.
                }

                write!(f, "(").unwrap();

                let mut mask = 1 as BaseElement;

                for i in 0..self.partition_size {
                    if (class & mask) != 0 {
                        write!(f, "{}", (('a' as u8) + i as u8) as char).unwrap();
                    }

                    mask <<= 1;
                }
                write!(f, ")").unwrap();
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use crate::{act::Act, partition::BaseElement};

    use super::Partition;

    const BELL_NUMBERS: [u32; 16] = [
        1, 1, 2, 5, 15, 52, 203, 877, 4140, 21147, 115975, 678570, 4213597, 27644437, 190899322,
        1382958545,
    ];

    #[test]
    fn generate_partitions() {
        for n in 1..12 {
            assert_eq!(
                Partition::new_partition_set(n).len() as u32,
                BELL_NUMBERS[n as usize]
            );
        }
    }

    #[test]
    fn partial_ord_le_1() {
        let partition_1 = Partition::new(&[0b11010, 0b00101], 5);
        let partition_2 = Partition::new(&[0b11000, 0b00001], 5);

        assert_eq!(partition_2 <= partition_1, true);
    }

    #[test]
    fn partial_ord_le_2() {
        let partition_1 = Partition::new(&[0b11010, 0b00101], 5);
        let partition_2 = Partition::new(&[0b11000, 0b00001], 5);

        assert_eq!(partition_1 >= partition_2, true);
    }

    #[test]
    fn partial_ord_le_3() {
        let partition_1 = Partition::new(&[0b11010, 0b00101], 5);
        let partition_2 = Partition::new(&[0b01110, 0b00100], 5);

        assert_eq!(partition_2 <= partition_1, false);
    }

    #[test]
    fn partial_ord_le_4() {
        let ab = Partition::new(&[0b011], 3);
        let ac = Partition::new(&[0b101], 3);

        assert_eq!(ab <= ac, false);
        assert_eq!(ab >= ac, false);
    }

    #[test]
    fn is_congruence_1() {
        #[rustfmt::skip]
        let cayley_table = [
            /* 0 */ 1, 0, 3, 2, 2 as BaseElement,
            /* 1 */ 3, 1, 1, 0, 2,
            /* 2 */ 2, 3, 3, 1, 2,
            /* 3 */ 3, 3, 2, 3, 2,
        ];

        let act = Act::new_from_cayley_table(&cayley_table, 5);

        let partition = Partition::new(&[0b1010, 0b0101], 4);

        assert_eq!(partition.is_congruence(&act), false);
    }

    #[test]
    fn is_congruence_2() {
        #[rustfmt::skip]
        let cayley_table = [
            /* 0 */ 1, 3, 2, 2 as BaseElement,
            /* 1 */ 3, 0, 2, 1,
            /* 2 */ 3, 1, 2, 0,
            /* 3 */ 3, 2, 0, 3,
        ];

        let act = Act::new_from_cayley_table(&cayley_table, 4);

        let partition = Partition::new(&[0b1010, 0b0101], 4);

        assert_eq!(partition.is_congruence(&act), true);
    }
}
