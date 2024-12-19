pub mod iterator;

/// Длинное натуральное число с фиксированным основанием.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LongNumber {
    /// Цифры длинного числа.
    pub digits: Vec<usize>,

    /// Основание системы счисления.
    pub radix: usize,
}

impl LongNumber {
    /// Creates a long zero.
    pub fn zero(digits_number: usize, radix: usize) -> Self {
        assert!(radix > 0 && digits_number > 0);

        Self {
            digits: vec![0; digits_number],
            radix,
        }
    }

    /// Creates a long number from a raw slice.
    pub fn from_slice(digits: &[usize], radix: usize) -> Self {
        Self {
            digits: digits.to_owned(),
            radix,
        }
    }

    /// Создаёт случайное длинное число.
    pub fn random(digits: usize, radix: usize) -> Self {
        assert!(radix > 0);
        assert!(digits > 0);

        let mut rng = rand::thread_rng();

        let digits = (0..digits)
            .map(|_| rand::Rng::gen_range(&mut rng, 0..radix))
            .collect::<Vec<usize>>();

        Self { digits, radix }
    }
}

impl std::fmt::Display for LongNumber {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "< ")?;

        for &d in &self.digits {
            write!(f, "{} ", d)?;
        }

        write!(f, ">")?;

        Ok(())
    }
}
