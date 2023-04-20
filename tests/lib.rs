//! The examples here are from paper `https://www.mathnet.ru/links/72a0f5ea28e6085fc3d2cfe72ca523a6/dvmg303.pdf` [1].

use congruencer::{
    act::Act, congruence::Congruence, lattice::Lattice, partition::Partition, poset,
};

mod left_zero {
    pub static P_NABLA: [u16; 1] = [0b1111];
    pub static P_A1: [u16; 3] = [0b0011, 0b1000, 0b0100];
    pub static P_A12: [u16; 2] = [0b0111, 0b1000];
    pub static P_A13: [u16; 2] = [0b1011, 0b1000];
    pub static P_A1_23: [u16; 2] = [0b0011, 0b1100];
    pub static P_123: [u16; 2] = [0b1110, 0b0001];
    pub static P_12: [u16; 3] = [0b0110, 0b1000, 0b0001];
    pub static P_13: [u16; 3] = [0b1010, 0b0100, 0b0001];
    pub static P_23: [u16; 3] = [0b1100, 0b0010, 0b0001];
    pub static P_DELTA: [u16; 4] = [0b1000, 0b0100, 0b0010, 0b0001];

    #[rustfmt::skip]
    pub static ACT_1: [[usize; 1]; 4] = [
        //       s1
        /* a */  [1],
        /* 1 */  [1],
        /* 2 */  [2],
        /* 3 */  [3],
    ];

    #[rustfmt::skip]
    pub static ACT_2: [[usize; 2]; 4] = [
        //       s1  s2
        /* a */  [1,  2],
        /* 1 */  [1,  1],
        /* 2 */  [2,  2],
        /* 3 */  [3,  3],
    ];
}

#[inline]
/// Returns true, if a slices are equal as a sets.
fn is_slices_equal<T: PartialEq>(slice_1: &[T], slice_2: &[T]) -> bool {
    slice_1.iter().all(|s| slice_2.contains(s)) && slice_2.iter().all(|s| slice_1.contains(s))
}

fn lattice_of_congruences<const ACT_SIZE: usize, const SEMIGROUP_SIZE: usize>(
    act: Act<ACT_SIZE, SEMIGROUP_SIZE>,
    congruences: &[&[u16]],
) {
    todo!()
}

/// [1], lemma 3, case (i), p.108, the top one.
#[test]
fn lattice_of_congruences_1() {
    let act = Act::new(left_zero::ACT_1);

    let act_size = act.size();

    let congruence_set = act.new_congruence_set();

    let control_set = [
        Partition::new(&[0b1111], act_size),                         // ∇
        Partition::new(&[0b0111, 0b1000], act_size),                 // (a12)
        Partition::new(&[0b1011, 0b0100], act_size),                 // (a13)
        Partition::new(&[0b0011, 0b1100], act_size),                 // (a1)(23)
        Partition::new(&[0b0011, 0b1000, 0b0100], act_size),         // (a1)
        Partition::new(&[0b1110, 0b0001], act_size),                 // (123)
        Partition::new(&[0b0110, 0b1000, 0b0001], act_size),         // (12)
        Partition::new(&[0b1010, 0b0100, 0b0001], act_size),         // (13)
        Partition::new(&[0b1100, 0b0010, 0b0001], act_size),         // (23)
        Partition::new(&[0b1000, 0b0100, 0b0010, 0b0001], act_size), // Δ
    ];

    assert!(is_slices_equal(&control_set, &congruence_set));

    let lattice = Lattice::from(congruence_set.as_slice());

    assert!(lattice.is_lattice());
    assert!(lattice.is_modular());
}

// [1], lemma 3, case (i), p.108, the bottom one.
// #[test]
// fn lattice_of_congruences_2() {
//     #[rustfmt::skip]
//     let cayley_table = [
//         //       s1  s2
//         /* a */  1,  2,
//         /* 1 */  1,  1,
//         /* 2 */  2,  2,
//         /* 3 */  3,  3,
//     ];

//     let act = Act::new_from_cayley_table(&cayley_table, 2);

//     let act_size = act.size();

//     let congruence_set = act.new_congruence_set();

//     let control_set = [
//         Partition::new(&[0b1111], act_size),                         // ∇
//         Partition::new(&[0b0111, 0b1000], act_size),                 // (a12)
//         Partition::new(&[0b1110, 0b0001], act_size),                 // (123)
//         Partition::new(&[0b0110, 0b1000, 0b0001], act_size),         // (12)
//         Partition::new(&[0b1010, 0b0100, 0b0001], act_size),         // (13)
//         Partition::new(&[0b1100, 0b0010, 0b0001], act_size),         // (23)
//         Partition::new(&[0b1000, 0b0100, 0b0010, 0b0001], act_size), // Δ
//     ];

//     assert!(is_slices_equal(&control_set, &congruence_set));

//     let lattice = Lattice::from(congruence_set.as_slice());

//     assert!(lattice.is_lattice());
//     assert!(lattice.is_modular());
// }
