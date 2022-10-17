type BaseType = u16;

/// Inserting zero into the bit representation of a number, starting from the specified position.
///
/// The position number is counted from the least significant bit of a number.
fn _insert_zero(mut number: BaseType, position: u32) -> BaseType {
    if number == 0 {
        return number;
    }

    // Make sure that we can actually insert the specified amount of bits.
    debug_assert!(number.leading_zeros() > 0);

    debug_assert!(position <= BaseType::BITS);

    // Trivial case.
    if position == 0 {
        number << 1
    }
    // Main case.
    else if position < BaseType::BITS - number.leading_zeros() {
        let mask: BaseType = (1 << position) - 1; // The same as 2^position - 1.

        // Saving all the bits before the `position`.
        let low_bits = number & mask;

        // Reset to zero all the number's bits before the `position`.
        number &= !mask;

        // Shift the result to the left.
        number <<= 1;

        // Restore back the low bits.
        number |= low_bits;

        number
    }
    // Nothing to do.
    else {
        number
    }
}

// TODO: написать функцию прореживания битовой записи числа нулями.

#[inline(always)]
pub fn is_power_of_two(number: BaseType) -> bool {
    debug_assert!(number > 0);

    (number & (number - 1)) == 0
}

pub fn main(set_size: BaseType) {
    // A vector of consecutive numbers starting with 3, with the exception of powers of two.
    let lookup_table = (3..=(1 << set_size))
        .filter(|&n| !is_power_of_two(n))
        .collect::<Vec<BaseType>>();

    for it in lookup_table.iter() {
        println!("{:b}", it);
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn insert_zeros() {
        assert_eq!(super::_insert_zero(0b0010011, 0), 0b0100110); // Trivial case.
        assert_eq!(super::_insert_zero(0b0010011, 5), 0b0010011);
        assert_eq!(super::_insert_zero(0b0010011, 3), 0b0100011);
        assert_eq!(super::_insert_zero(0b0010011, 4), 0b0100011);
        assert_eq!(super::_insert_zero(0b111, 1), 0b1101);

        assert_eq!(
            super::_insert_zero(0b0011111111111111, 13),
            0b101111111111111
        );
    }

    #[test]
    fn is_power_of_two() {
        assert_eq!(super::is_power_of_two(1), true);
        assert_eq!(super::is_power_of_two(2), true);
        assert_eq!(super::is_power_of_two(3), false);
        assert_eq!(super::is_power_of_two(4), true);
        assert_eq!(super::is_power_of_two(5), false);
        assert_eq!(super::is_power_of_two(6), false);
        assert_eq!(super::is_power_of_two(7), false);
        assert_eq!(super::is_power_of_two(8), true);
        assert_eq!(super::is_power_of_two(9), false);
        assert_eq!(super::is_power_of_two(10), false);

        assert_eq!(super::is_power_of_two(16), true);
        assert_eq!(super::is_power_of_two(32), true);
        assert_eq!(super::is_power_of_two(64), true);
        assert_eq!(super::is_power_of_two(128), true);

        assert_eq!(super::is_power_of_two(428), false);
    }
}
