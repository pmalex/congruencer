use super::LongNaturalNumber;

pub struct LongNaturalNumberIterator<const SIZE: usize, const RADIX: usize> {
    long_number: LongNaturalNumber<SIZE, RADIX>,
    end_is_reached: bool,
}

impl<const SIZE: usize, const RADIX: usize> LongNaturalNumberIterator<SIZE, RADIX> {
    pub fn new() -> Self {
        Self {
            long_number: LongNaturalNumber::<SIZE, RADIX>::zero(),
            end_is_reached: false,
        }
    }
}

impl<const SIZE: usize, const RADIX: usize> Iterator for LongNaturalNumberIterator<SIZE, RADIX> {
    type Item = LongNaturalNumber<SIZE, RADIX>;

    fn next(&mut self) -> Option<Self::Item> {
        if !self.end_is_reached {
            let result = self.long_number.clone();

            // The `true` means that we need to do the exactly one increment.
            let mut do_shift = true;

            for digit in self.long_number.digits.iter_mut() {
                if !do_shift && *digit < RADIX {
                    return Some(result);
                }

                if do_shift {
                    *digit += 1;
                    do_shift = false;

                    if *digit == RADIX {
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

#[test]
fn long_binary() {
    const SIZE: usize = 3;
    const RADIX: usize = 2;

    let mut long_number_it = LongNaturalNumberIterator::<SIZE, RADIX>::new();

    assert_eq!(
        long_number_it.next(),
        Some(LongNaturalNumber::from_raw_slice(&[0, 0, 0]))
    );
    assert_eq!(
        long_number_it.next(),
        Some(LongNaturalNumber::from_raw_slice(&[1, 0, 0]))
    );
    assert_eq!(
        long_number_it.next(),
        Some(LongNaturalNumber::from_raw_slice(&[0, 1, 0]))
    );
    assert_eq!(
        long_number_it.next(),
        Some(LongNaturalNumber::from_raw_slice(&[1, 1, 0]))
    );
    assert_eq!(
        long_number_it.next(),
        Some(LongNaturalNumber::from_raw_slice(&[0, 0, 1]))
    );
    assert_eq!(
        long_number_it.next(),
        Some(LongNaturalNumber::from_raw_slice(&[1, 0, 1]))
    );
    assert_eq!(
        long_number_it.next(),
        Some(LongNaturalNumber::from_raw_slice(&[0, 1, 1]))
    );
    assert_eq!(
        long_number_it.next(),
        Some(LongNaturalNumber::from_raw_slice(&[1, 1, 1]))
    );
    assert_eq!(long_number_it.next(), None);
    assert_eq!(long_number_it.next(), None);
    assert_eq!(long_number_it.next(), None);
}

#[test]
fn long_ternary() {
    const SIZE: usize = 2;
    const RADIX: usize = 3;

    let mut long_number_it = LongNaturalNumberIterator::<SIZE, RADIX>::new();

    assert_eq!(
        long_number_it.next(),
        Some(LongNaturalNumber::from_raw_slice(&[0, 0]))
    );
    assert_eq!(
        long_number_it.next(),
        Some(LongNaturalNumber::from_raw_slice(&[1, 0]))
    );
    assert_eq!(
        long_number_it.next(),
        Some(LongNaturalNumber::from_raw_slice(&[2, 0]))
    );
    assert_eq!(
        long_number_it.next(),
        Some(LongNaturalNumber::from_raw_slice(&[0, 1]))
    );
    assert_eq!(
        long_number_it.next(),
        Some(LongNaturalNumber::from_raw_slice(&[1, 1]))
    );
    assert_eq!(
        long_number_it.next(),
        Some(LongNaturalNumber::from_raw_slice(&[2, 1]))
    );
    assert_eq!(
        long_number_it.next(),
        Some(LongNaturalNumber::from_raw_slice(&[0, 2]))
    );
    assert_eq!(
        long_number_it.next(),
        Some(LongNaturalNumber::from_raw_slice(&[1, 2]))
    );
    assert_eq!(
        long_number_it.next(),
        Some(LongNaturalNumber::from_raw_slice(&[2, 2]))
    );
    assert_eq!(long_number_it.next(), None);
    assert_eq!(long_number_it.next(), None);
    assert_eq!(long_number_it.next(), None);
}
