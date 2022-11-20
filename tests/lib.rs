use congruencer::{act::Act, congruence::Congruence, lattice::Lattice, poset};

/// Print a poset in the Graphiz Dot format.
fn poset_print_dot<P>(poset: &[P])
where
    P: PartialOrd + std::fmt::Display,
{
    println!("graph lattice {{");
    println!("\trankdir = TB;");
    println!("\tratio = 0.75;");
    println!("\tnode[shape = none];");
    println!();

    for p in poset {
        for nearest_upper_bound in poset::nearest_incomparable_lower_bounds(poset, p) {
            println!("\t\"{}\" -- \"{}\"", p, nearest_upper_bound);
        }
    }

    println!("}}");
}

#[test]
fn lattice_of_congruences_1() {
    #[rustfmt::skip]
    let cayley_table = [
        //       s1
        /* a */  1,
        /* 1 */  1,
        /* 2 */  2,
        /* 3 */  3,
    ];

    let act = Act::new_from_cayley_table(&cayley_table, 1);

    let congruence_set = act.new_congruence_set();

    for p in &congruence_set {
        print!("{}, ", p);
    }
    println!();

    // { ∇, (abc), (abd), (ab)(cd), (ab), (bcd), (bc), (bd), (cd), Δ }

    let lattice = Lattice::from(congruence_set.as_slice());

    assert!(lattice.is_lattice());

    poset_print_dot(congruence_set.as_slice());

    assert!(lattice.is_modular());
    // assert!(lattice.is_distributive());
}
