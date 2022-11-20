use crate::{binary_operation::BinaryOperation, long_number::LongNumberIncrementalIterator};

pub struct Semigroup(BinaryOperation);

impl TryFrom<BinaryOperation> for Semigroup {
    type Error = ();

    fn try_from(f: BinaryOperation) -> Result<Self, Self::Error> {
        if f.is_associative() {
            Ok(Self(f))
        } else {
            Err(())
        }
    }
}

// pub struct SemigroupIterator(LongNumberIncrementalIterator);

// impl SemigroupIterator {
//     pub fn new(semigroup_size: usize) -> Self {
//         Self {
//             semigroup_code: Vec::<ElementIndex>::with_capacity(semigroup_size * semigroup_size),
//             semigroup_size,
//         }
//     }
// }

// impl Iterator for SemigroupIterator {
//     type Item = Semigroup;

//     fn next(&mut self) -> Option<Self::Item> {
//         let semigroup_size = self.semigroup_size;
//         let semigroup_code = &self.semigroup_code;

//         for n in 0..semigroup_size * semigroup_size {}

//         todo!()
//     }
// }
