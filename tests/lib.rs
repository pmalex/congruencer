/// The examples here are from the work `https://www.mathnet.ru/links/72a0f5ea28e6085fc3d2cfe72ca523a6/dvmg303.pdf` [1].
use congruencer::{
    act::Act, congruence::Congruence, lattice::Lattice, partition::Partition, poset,
};

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

#[inline]
/// Returns true, if a slices are equal as a sets.
fn is_slices_equal<T: PartialEq>(slice_1: &[T], slice_2: &[T]) -> bool {
    slice_1.iter().all(|s| slice_2.contains(s)) && slice_2.iter().all(|s| slice_1.contains(s))
}

/// [1], p.108, the top one.
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

    let act_size = act.size();

    let congruence_set = act.new_congruence_set();

    print!("Congruence set: {{");
    for p in &congruence_set {
        print!("{}, ", p);
    }
    println!("}}");

    let control_set = [
        Partition::new(&[0b1111], act_size),                         // ∇
        Partition::new(&[0b0111, 0b1000], act_size),                 // (abc)
        Partition::new(&[0b1011, 0b0100], act_size),                 // (abd)
        Partition::new(&[0b0011, 0b1100], act_size),                 // (ab)(cd)
        Partition::new(&[0b0011, 0b1000, 0b0100], act_size),         // (ab)
        Partition::new(&[0b1110, 0b0001], act_size),                 // (bcd)
        Partition::new(&[0b0110, 0b1000, 0b0001], act_size),         // (bc)
        Partition::new(&[0b1010, 0b0100, 0b0001], act_size),         // (bd)
        Partition::new(&[0b1100, 0b0010, 0b0001], act_size),         // (cd)
        Partition::new(&[0b1000, 0b0100, 0b0010, 0b0001], act_size), // Δ
    ];

    print!("Control set: {{");
    for p in &control_set {
        print!("{}, ", p);
    }
    println!("}}");

    assert!(is_slices_equal(&control_set, &congruence_set));

    let lattice = Lattice::from(congruence_set.as_slice());

    poset_print_dot(congruence_set.as_slice());

    assert!(lattice.is_lattice());
    assert!(lattice.is_modular());
}

/// [1], lemma 3, case (i), p.108, the bottom one.
#[test]
fn lattice_of_congruences_2() {
    #[rustfmt::skip]
    let cayley_table = [
        //       s1  s2
        /* a */  1,  2,
        /* 1 */  1,  1,
        /* 2 */  2,  2,
        /* 3 */  3,  3,
    ];

    let act = Act::new_from_cayley_table(&cayley_table, 2);

    let act_size = act.size();

    let congruence_set = act.new_congruence_set();

    print!("Congruence set: {{");
    for p in &congruence_set {
        print!("{}, ", p);
    }
    println!("}}");

    let control_set = [
        Partition::new(&[0b1111], act_size),                         // ∇
        Partition::new(&[0b0111, 0b1000], act_size),                 // (abc)
        Partition::new(&[0b1110, 0b0001], act_size),                 // (bcd)
        Partition::new(&[0b0110, 0b1000, 0b0001], act_size),         // (bc)
        Partition::new(&[0b1010, 0b0100, 0b0001], act_size),         // (bd)
        Partition::new(&[0b1100, 0b0010, 0b0001], act_size),         // (cd)
        Partition::new(&[0b1000, 0b0100, 0b0010, 0b0001], act_size), // Δ
    ];

    print!("Control set: {{");
    for p in &control_set {
        print!("{}, ", p);
    }
    println!("}}");

    assert!(is_slices_equal(&control_set, &congruence_set));

    let lattice = Lattice::from(congruence_set.as_slice());

    poset_print_dot(congruence_set.as_slice());

    assert!(lattice.is_lattice());
    assert!(lattice.is_modular());
}
