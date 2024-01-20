use super::BinaryOperation;

pub struct BinaryOperationIterator<const SIZE: usize> {
    binary_operation: BinaryOperation<SIZE>,
    end_is_reached: bool,
}

impl<const SIZE: usize> BinaryOperationIterator<SIZE> {
    pub fn new() -> Self {
        Self {
            binary_operation: BinaryOperation::zero(),
            end_is_reached: false,
        }
    }
}

impl<const SIZE: usize> Iterator for BinaryOperationIterator<SIZE> {
    type Item = BinaryOperation<SIZE>;

    fn next(&mut self) -> Option<Self::Item> {
        if !self.end_is_reached {
            let result = self.binary_operation.clone();

            // The `true` means that we need to do the exactly one increment.
            let mut do_shift = true;

            for digit in self.binary_operation.0.iter_mut().flatten() {
                if !do_shift && *digit < SIZE {
                    return Some(result);
                }

                if do_shift {
                    *digit += 1;
                    do_shift = false;

                    if *digit == SIZE {
                        *digit = 0;
                        do_shift = true;
                    }
                }
            }

            if do_shift {
                self.end_is_reached = true;
            }

            Some(result)
        } else {
            None
        }
    }
}
