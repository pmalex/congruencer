use std::convert::TryFrom;

use super::homogeneous;

/// Equivalence relation.
pub struct Equivalence<X>(homogeneous::Relation<X>);

impl<X> TryFrom<homogeneous::Relation<X>> for Equivalence<X> {
    type Error = &'static str;

    fn try_from(relation: homogeneous::Relation<X>) -> Result<Self, Self::Error> {
        if relation.is_reflexive() && relation.is_symmetric() && relation.is_transitive() {
            Ok(Self(relation))
        } else {
            Err("Cannot construct equivalence relation. It should be a reflexive, symmetric and transitive binary relation.")
        }
    }
}
