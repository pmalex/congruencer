use std::cmp::Ordering;

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
