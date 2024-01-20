use rayon::prelude::{IntoParallelIterator, IntoParallelRefIterator, ParallelIterator};
use std::cmp::Ordering;

/// Bounds the maximum amount of elements in a partition.
type BaseElement = u16;

#[derive(Clone)]
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

    #[inline]
    pub fn par_iter(&self) -> rayon::slice::Iter<BaseElement> {
        self.raw_partition.par_iter()
    }

    #[inline(always)]
    pub fn size(&self) -> usize {
        self.partition_size
    }

    /// Generate a new set of partitions by adding a new element to the current partition.
    fn expand(&self) -> Vec<Self> {
        let Self {
            raw_partition,
            partition_size,
        } = self;

        // Binary repesentation of a new element that should be added to the partition.
        let new_element_position = 1 << partition_size;

        (0..raw_partition.len())
            .into_par_iter()
            .map(|class_number| {
                let mut new_raw_partition = raw_partition.clone();

                new_raw_partition[class_number] |= new_element_position;

                Self {
                    raw_partition: new_raw_partition,
                    partition_size: partition_size + 1,
                }
            })
            .chain(rayon::iter::once({
                let mut new_raw_partition =
                    Vec::<BaseElement>::with_capacity(raw_partition.len() + 1);

                new_raw_partition.clone_from(raw_partition);

                new_raw_partition.push(new_element_position);

                Self {
                    raw_partition: new_raw_partition,
                    partition_size: partition_size + 1,
                }
            }))
            .collect::<Vec<Partition>>()
    }

    /// Print a partition in a human-friendly form (using an alphabet).
    pub fn print(&self, alphabet: &str) {
        assert_eq!(self.partition_size, alphabet.chars().count());

        if self.partition_size == self.raw_partition.len() {
            print!("Δ");
        } else if self.raw_partition.len() == 1 {
            print!("∇");
        } else {
            for &class in &self.raw_partition {
                if class.is_power_of_two() {
                    continue; // Do not print the powers of two, because it's a one-element class.
                }

                print!("(");

                let mut mask = 1 as BaseElement;

                for c in alphabet.chars() {
                    if class & mask != 0 {
                        print!("{}", c);
                    }

                    mask <<= 1;
                }
                print!(")");
            }
        }
    }
}

/// Generates all possible partitions of an n-element set.
pub fn new_partitions_set(set_size: usize) -> Vec<Partition> {
    assert!(set_size > 0 && set_size <= BaseElement::BITS as usize);

    (1..set_size).fold(vec![Partition::new(&[1], 1)], |previous_generation, _| {
        previous_generation
            .into_par_iter()
            .flat_map(|partition| partition.expand())
            .collect::<Vec<Partition>>()
    })
}

impl PartialEq for Partition {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.raw_partition
            .iter()
            .all(|partition| other.raw_partition.contains(partition))
            && other
                .raw_partition
                .iter()
                .all(|partition| self.raw_partition.contains(partition))
    }
}

impl PartialOrd for Partition {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self <= other, other <= self) {
            (true, true) => Some(Ordering::Equal),
            (true, false) => Some(Ordering::Less),
            (false, true) => Some(Ordering::Greater),
            (false, false) => None,
        }
    }

    #[inline]
    fn le(&self, other: &Self) -> bool {
        self.raw_partition.par_iter().all(|&class| {
            other
                .raw_partition
                .iter()
                .any(|&other_class| (class | other_class) == other_class)
        })
    }

    #[inline(always)]
    fn ge(&self, other: &Self) -> bool {
        other.le(self)
    }
}

#[cfg(test)]
mod test {
    use crate::partition::{self, Partition};

    const BELL_NUMBERS: [u32; 16] = [
        1, 1, 2, 5, 15, 52, 203, 877, 4140, 21147, 115975, 678570, 4213597, 27644437, 190899322,
        1382958545,
    ];

    #[test]
    fn generate_partitions() {
        for n in 1..12 {
            assert_eq!(
                partition::new_partitions_set(n).len() as u32,
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
}
