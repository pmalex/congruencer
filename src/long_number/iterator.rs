use super::LongNaturalNumber;

#[derive(Clone)]
pub struct LongNaturalNumberIterator {
    long_number: LongNaturalNumber,
    end_is_reached: bool,
}

impl LongNaturalNumberIterator {
    pub fn new(digits_number: usize, radix: usize) -> Self {
        Self {
            long_number: LongNaturalNumber::zero(digits_number, radix),
            end_is_reached: false,
        }
    }
}

impl Iterator for LongNaturalNumberIterator {
    type Item = LongNaturalNumber;

    fn next(&mut self) -> Option<Self::Item> {
        if !self.end_is_reached {
            let result = self.long_number.clone();

            // The `true` means that we need to do the exactly one increment.
            let mut do_shift = true;

            for digit in self.long_number.digits.iter_mut() {
                if !do_shift && *digit < self.long_number.radix {
                    return Some(result);
                }

                if do_shift {
                    *digit += 1;
                    do_shift = false;

                    if *digit == self.long_number.radix {
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
    let radix = 2;

    let mut long_number_it = LongNaturalNumberIterator::new(3, radix);

    assert_eq!(
        long_number_it.next(),
        Some(LongNaturalNumber::from_raw_slice(&[0, 0, 0], radix))
    );
    assert_eq!(
        long_number_it.next(),
        Some(LongNaturalNumber::from_raw_slice(&[1, 0, 0], radix))
    );
    assert_eq!(
        long_number_it.next(),
        Some(LongNaturalNumber::from_raw_slice(&[0, 1, 0], radix))
    );
    assert_eq!(
        long_number_it.next(),
        Some(LongNaturalNumber::from_raw_slice(&[1, 1, 0], radix))
    );
    assert_eq!(
        long_number_it.next(),
        Some(LongNaturalNumber::from_raw_slice(&[0, 0, 1], radix))
    );
    assert_eq!(
        long_number_it.next(),
        Some(LongNaturalNumber::from_raw_slice(&[1, 0, 1], radix))
    );
    assert_eq!(
        long_number_it.next(),
        Some(LongNaturalNumber::from_raw_slice(&[0, 1, 1], radix))
    );
    assert_eq!(
        long_number_it.next(),
        Some(LongNaturalNumber::from_raw_slice(&[1, 1, 1], radix))
    );
    assert_eq!(long_number_it.next(), None);
    assert_eq!(long_number_it.next(), None);
    assert_eq!(long_number_it.next(), None);
}

#[test]
fn long_ternary() {
    let radix = 3;

    let mut long_number_it = LongNaturalNumberIterator::new(2, radix);

    assert_eq!(
        long_number_it.next(),
        Some(LongNaturalNumber::from_raw_slice(&[0, 0], radix))
    );
    assert_eq!(
        long_number_it.next(),
        Some(LongNaturalNumber::from_raw_slice(&[1, 0], radix))
    );
    assert_eq!(
        long_number_it.next(),
        Some(LongNaturalNumber::from_raw_slice(&[2, 0], radix))
    );
    assert_eq!(
        long_number_it.next(),
        Some(LongNaturalNumber::from_raw_slice(&[0, 1], radix))
    );
    assert_eq!(
        long_number_it.next(),
        Some(LongNaturalNumber::from_raw_slice(&[1, 1], radix))
    );
    assert_eq!(
        long_number_it.next(),
        Some(LongNaturalNumber::from_raw_slice(&[2, 1], radix))
    );
    assert_eq!(
        long_number_it.next(),
        Some(LongNaturalNumber::from_raw_slice(&[0, 2], radix))
    );
    assert_eq!(
        long_number_it.next(),
        Some(LongNaturalNumber::from_raw_slice(&[1, 2], radix))
    );
    assert_eq!(
        long_number_it.next(),
        Some(LongNaturalNumber::from_raw_slice(&[2, 2], radix))
    );
    assert_eq!(long_number_it.next(), None);
    assert_eq!(long_number_it.next(), None);
    assert_eq!(long_number_it.next(), None);
}