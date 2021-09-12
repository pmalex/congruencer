use super::heterogeneous;

/// Homogeneous binary relation.
pub struct Relation<X>(heterogeneous::Relation<X, X>);

impl<X> Relation<X> {
    pub fn new(set: &[X]) -> Self
    where
        X: Clone,
    {
        Self(heterogeneous::Relation::new(set, set))
    }

    pub fn is_reflexive(&self) -> bool {
        todo!()
    }

    pub fn is_symmetric(&self) -> bool {
        todo!()
    }

    pub fn is_antisymmetric(&self) -> bool {
        todo!()
    }

    pub fn is_transitive(&self) -> bool {
        todo!()
    }

    #[inline]
    pub fn is_left_total(&self) -> bool {
        self.0.is_left_total()
    }

    #[inline]
    pub fn is_right_total(&self) -> bool {
        self.0.is_right_total()
    }

    #[inline]
    pub fn is_left_unique(&self) -> bool {
        self.0.is_left_unique()
    }
}
