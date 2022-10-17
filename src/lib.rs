// TODO: после написания кода векторов посмотреть сколько памяти из выделенной не используется.

use rayon::prelude::{IntoParallelIterator, ParallelIterator};
use std::fmt::Display;

type BaseElement = u16;
type PartitonSize = u8;

#[derive(Debug)]
pub struct Partition {
    raw_partition: Vec<BaseElement>,
    partition_size: PartitonSize,
}

impl Display for Partition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for class in &self.raw_partition {
            write!(f, "(").unwrap();

            let mut mask = 1 as BaseElement;

            for i in 0..self.partition_size {
                if (class & mask) != 0 {
                    write!(f, "{}", (('a' as u8) + i) as char).unwrap();
                }

                mask <<= 1;
            }
            write!(f, ")").unwrap();
        }

        Ok(())
    }
}

impl Partition {
    pub fn new(raw_partition: Vec<BaseElement>, partition_size: PartitonSize) -> Self {
        Self {
            raw_partition,
            partition_size,
        }
    }

    /// Generate a new set of partitions by adding a new element to the current partition.
    pub fn expand(&self) -> Vec<Self> {
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
}

#[cfg(test)]
mod test {
    use super::Partition;

    #[test]
    fn expand_partition() {
        let partition = Partition::new(vec![0b00011, 0b01100, 0b10000], 5);

        println!("{}", partition);

        for partition in partition.expand() {
            println!("{}", partition);
        }
    }
}
