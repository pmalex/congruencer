use std::cmp::Ordering;

use crate::ElementIndex;

/// Partially ordered set.
pub struct Poset {
    table: Vec<Option<Ordering>>,
    set_size: usize,
}

impl Poset {
    #[inline(always)]
    /// Returns relation between two elements.
    pub fn cmp(&self, a: ElementIndex, b: ElementIndex) -> Option<Ordering> {
        debug_assert!(a < self.set_size && b < self.set_size);

        self.table[a * self.set_size + b]
    }

    #[inline]
    fn is_reflexive(&self) -> bool {
        (0..self.set_size).all(|i| self.table[i * self.set_size + i] == Some(Ordering::Equal))
    }

    fn is_symmetric(&self) -> bool {
        for i in 0..self.set_size {
            for j in i + 1..self.set_size {
                if !matches!(
                    (
                        self.table[i * self.set_size + j],
                        self.table[j * self.set_size + i],
                    ),
                    (Some(Ordering::Less), Some(Ordering::Greater))
                        | (Some(Ordering::Greater), Some(Ordering::Less))
                        | (Some(Ordering::Equal), Some(Ordering::Equal))
                        | (None, None)
                ) {
                    return false;
                }
            }
        }

        true
    }

    fn is_transitive(&self) -> bool {
        for a in &self.table {
            for b in &self.table {
                for c in &self.table {
                    if let (Some(Ordering::Less), Some(Ordering::Less)) =
                        (a.partial_cmp(b), b.partial_cmp(c))
                    {
                        if let Some(Ordering::Less) = a.partial_cmp(c) {
                            continue;
                        } else {
                            return false;
                        }
                    }
                }
            }
        }

        true
    }
}

impl<P> From<&[P]> for Poset
where
    P: PartialOrd,
{
    fn from(elements_set: &[P]) -> Self {
        assert!(!elements_set.is_empty());

        let set_size = elements_set.len();

        let mut table = Vec::<Option<Ordering>>::with_capacity(set_size * set_size);

        for (i, a) in elements_set.iter().enumerate() {
            for (j, b) in elements_set.iter().enumerate() {
                table[i * set_size + j] = a.partial_cmp(b);
            }
        }

        let poset = Self { table, set_size };

        // TODO: перенести это повыше
        assert!(poset.is_reflexive());
        assert!(poset.is_symmetric());
        assert!(poset.is_transitive());

        poset
    }
}

/// Returns a vector of the nearest (maximal) incomparable lower bounds for some element.
pub fn nearest_incomparable_lower_bounds<'a, P: PartialOrd>(
    poset: &'a [P],
    element: &'a P,
) -> Vec<&'a P> {
    if poset.is_empty() {
        return vec![];
    }

    // In the exreme case in the next iteration we keep all the elements and add a new one.
    let mut result = Vec::<&P>::with_capacity(poset.len() + 1);

    // The strategy: "diving" the poset elements into the result vector.
    'outer: for p in poset {
        // Filter out nedless elements.
        if let Some(Ordering::Less) = p.partial_cmp(element) {
            if result.is_empty() {
                result.push(p);
                continue;
            }

            let mut result_tmp = Vec::<&P>::with_capacity(result.len() + 1);

            for &r in &result {
                if p <= r {
                    continue 'outer; // Breaking the loop. The element `p` does not suit us.
                } else if r <= p {
                    continue; // In that case we don't need to add `r` in the new result, just continue the cycle.
                } else {
                    result_tmp.push(r); // The elements `r` and `p` are incomparable => we add the `r`.
                }
            }

            result_tmp.push(p);

            result = result_tmp;
        }
    }

    result.shrink_to_fit();

    if cfg!(debug_assertions) {
        // Checking that all the result elements are incomparable.
        assert!(result.iter().all(|&r1| result
            .iter()
            .all(|&r2| matches!(r1.partial_cmp(r2), Some(Ordering::Equal) | None))));

        // Checking that all result elements < `element`.
        assert!(result.iter().all(|&r| r < element));

        // Checking that there are no poset element `p` such that `element` > p > result elements
        // (i.e. that the result elements are indeed a nearest lower bounds).
        assert!(!poset
            .iter()
            .any(|p| p < element && result.iter().all(|&r| r < p)));
    }

    result
}

/// Returns a vector of the nearest (minimal) incomparable upper bounds for some element.
pub fn nearest_incomparable_upper_bounds<'a, P: PartialOrd>(
    poset: &'a [P],
    element: &'a P,
) -> Vec<&'a P> {
    if poset.is_empty() {
        return vec![];
    }

    // In the exreme case in the next iteration we keep all the elements and add a new one.
    let mut result = Vec::<&P>::with_capacity(poset.len() + 1);

    // The strategy: "diving" the poset elements into the result vector.
    'outer: for p in poset {
        // Filter out nedless elements.
        if let Some(Ordering::Greater) = p.partial_cmp(element) {
            if result.is_empty() {
                result.push(p);
                continue;
            }

            let mut result_tmp = Vec::<&P>::with_capacity(result.len() + 1);

            for &r in &result {
                if r <= p {
                    continue 'outer; // Breaking the loop. The element `p` does not suit us.
                } else if p <= r {
                    continue; // In that case we don't need to add `r` in the new result, just continue the cycle.
                } else {
                    result_tmp.push(r); // The elements `r` and `p` are incomparable => we add the `r`.
                }
            }

            result_tmp.push(p);

            result = result_tmp;
        }
    }

    result.shrink_to_fit();

    if cfg!(debug_assertions) {
        // Checking that all the result elements are incomparable.
        assert!(result.iter().all(|&r1| result
            .iter()
            .all(|&r2| matches!(r1.partial_cmp(r2), Some(Ordering::Equal) | None))));

        // Checking that all result elements > `element`.
        assert!(result.iter().all(|&r| element < r));

        // Checking that there are no poset element `p` such that `element` < p < result elements
        // (i.e. that the result elements are indeed a nearest upper bounds).
        assert!(!poset
            .iter()
            .any(|p| element < p && result.iter().all(|&r| p < r)));
    }

    result
}
