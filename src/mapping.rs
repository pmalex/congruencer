use std::convert::TryFrom;

use crate::relation::binary::heterogeneous::Relation;

/// The mapping from X to Y.
pub struct Mapping<X, Y>(Relation<X, Y>);

impl<X, Y> Mapping<X, Y> {
    pub fn apply(&self, argument: &X) -> &Y {
        if let [image] = self.0.apply(argument) {
            image
        } else {
            panic!()
        }
    }
}

impl<X, Y> TryFrom<Relation<X, Y>> for Mapping<X, Y> {
    type Error = &'static str;

    fn try_from(relation: Relation<X, Y>) -> Result<Self, Self::Error> {
        if relation.is_right_unique() && relation.is_left_total() {
            Ok(Mapping(relation))
        } else {
            Err("Cannot construct mapping from relation. Relation should be right unique and left total.")
        }
    }
}
