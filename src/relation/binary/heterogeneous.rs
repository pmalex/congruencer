use bit_matrix::BitMatrix;

/// Heterogeneous binary relation.
pub struct Relation<X, Y> {
    _domain: Vec<X>,
    _range: Vec<Y>,

    _matrix: BitMatrix,
}

impl<X, Y> Relation<X, Y> {
    pub fn new(domain: &[X], range: &[Y]) -> Self
    where
        X: Clone,
        Y: Clone,
    {
        Self {
            _domain: domain.to_vec(),
            _range: range.to_vec(),

            _matrix: BitMatrix::new(domain.len(), range.len()),
        }
    }

    pub fn apply(&self, _argument: &X) -> &[Y] {
        // let row_index = self.domain.iter().position(argument).unwrap();

        // let r = self.matrix.iter_row(row_index).scan(initial_state, f)

        todo!()
    }

    pub fn is_left_total(&self) -> bool {
        todo!()
    }

    pub fn is_right_total(&self) -> bool {
        todo!()
    }

    pub fn is_left_unique(&self) -> bool {
        todo!()
    }

    pub fn is_right_unique(&self) -> bool {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
