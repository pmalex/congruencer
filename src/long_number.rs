#[derive(Clone, Debug, PartialEq)]
pub struct LongNumber {
    pub digits: Vec<usize>,
    pub radix: usize,
}

impl LongNumber {
    /// Creates a long zero.
    pub fn new(digits_number: usize, radix: usize) -> Self {
        assert!(radix > 0 && digits_number > 0);

        Self {
            digits: vec![0; digits_number],
            radix,
        }
    }

    /// Creates a long number from a raw slice.
    pub fn from_raw_slice(digits: &[usize], radix: usize) -> Self {
        Self {
            digits: digits.to_owned(),
            radix,
        }
    }

    /// Returns incremental iterator.
    pub fn into_inc_iter(self) -> LongNumberIncrementalIterator {
        LongNumberIncrementalIterator {
            long_number: self,
            is_end_reached: false,
        }
    }
}

#[derive(Clone)]
pub struct LongNumberIncrementalIterator {
    long_number: LongNumber,
    is_end_reached: bool,
}

impl Iterator for LongNumberIncrementalIterator {
    type Item = LongNumber;

    fn next(&mut self) -> Option<Self::Item> {
        if !self.is_end_reached {
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
                self.is_end_reached = true;
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

    let mut long_number_it = LongNumber::new(3, radix).into_inc_iter();

    assert_eq!(
        long_number_it.next(),
        Some(LongNumber::from_raw_slice(&[0, 0, 0], radix))
    );
    assert_eq!(
        long_number_it.next(),
        Some(LongNumber::from_raw_slice(&[1, 0, 0], radix))
    );
    assert_eq!(
        long_number_it.next(),
        Some(LongNumber::from_raw_slice(&[0, 1, 0], radix))
    );
    assert_eq!(
        long_number_it.next(),
        Some(LongNumber::from_raw_slice(&[1, 1, 0], radix))
    );
    assert_eq!(
        long_number_it.next(),
        Some(LongNumber::from_raw_slice(&[0, 0, 1], radix))
    );
    assert_eq!(
        long_number_it.next(),
        Some(LongNumber::from_raw_slice(&[1, 0, 1], radix))
    );
    assert_eq!(
        long_number_it.next(),
        Some(LongNumber::from_raw_slice(&[0, 1, 1], radix))
    );
    assert_eq!(
        long_number_it.next(),
        Some(LongNumber::from_raw_slice(&[1, 1, 1], radix))
    );
    assert_eq!(long_number_it.next(), None);
    assert_eq!(long_number_it.next(), None);
    assert_eq!(long_number_it.next(), None);
}

#[test]
fn long_ternary() {
    let radix = 3;

    let mut long_number_it = LongNumber::new(2, radix).into_inc_iter();

    assert_eq!(
        long_number_it.next(),
        Some(LongNumber::from_raw_slice(&[0, 0], radix))
    );
    assert_eq!(
        long_number_it.next(),
        Some(LongNumber::from_raw_slice(&[1, 0], radix))
    );
    assert_eq!(
        long_number_it.next(),
        Some(LongNumber::from_raw_slice(&[2, 0], radix))
    );
    assert_eq!(
        long_number_it.next(),
        Some(LongNumber::from_raw_slice(&[0, 1], radix))
    );
    assert_eq!(
        long_number_it.next(),
        Some(LongNumber::from_raw_slice(&[1, 1], radix))
    );
    assert_eq!(
        long_number_it.next(),
        Some(LongNumber::from_raw_slice(&[2, 1], radix))
    );
    assert_eq!(
        long_number_it.next(),
        Some(LongNumber::from_raw_slice(&[0, 2], radix))
    );
    assert_eq!(
        long_number_it.next(),
        Some(LongNumber::from_raw_slice(&[1, 2], radix))
    );
    assert_eq!(
        long_number_it.next(),
        Some(LongNumber::from_raw_slice(&[2, 2], radix))
    );
    assert_eq!(long_number_it.next(), None);
    assert_eq!(long_number_it.next(), None);
    assert_eq!(long_number_it.next(), None);
}
