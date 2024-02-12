//! Примеры, приведённые здесь взяты из этой статьи: https://www.mathnet.ru/links/72a0f5ea28e6085fc3d2cfe72ca523a6/dvmg303.pdf,
//! будем обозначать её далее как [1].

use congruencer::{act::Act, lattice::Lattice, partition::Partition};

mod acts_over_left_zero_semigroup {
    //! Модуль, содержащий таблицы простейших полигонов над полугруппой левых нулей.

    use congruencer::partition::BaseDataType;

    #[rustfmt::skip]
    /// Имя переменной - сокращение от: 'Partition Nabla'.
    // Далее буква P в начале имён обозначает то же самое.
    pub static P_NABLA: [BaseDataType; 1] = [0b1111]; // ∇
    pub static P_A1: [BaseDataType; 3] = [0b0011, 0b1000, 0b0100]; // (a1)
    pub static P_A12: [BaseDataType; 2] = [0b0111, 0b1000]; // (a12)
    pub static P_A13: [BaseDataType; 2] = [0b1011, 0b0100]; // (a13)
    pub static P_A1_23: [BaseDataType; 2] = [0b0011, 0b1100]; // (a1)(23)
    pub static P_123: [BaseDataType; 2] = [0b1110, 0b0001]; // (123)
    pub static P_12: [BaseDataType; 3] = [0b0110, 0b1000, 0b0001]; // (12)
    pub static P_13: [BaseDataType; 3] = [0b1010, 0b0100, 0b0001]; // (13)
    pub static P_23: [BaseDataType; 3] = [0b1100, 0b0010, 0b0001]; // (23)
    pub static P_DELTA: [BaseDataType; 4] = [0b1000, 0b0100, 0b0010, 0b0001]; // Δ

    #[rustfmt::skip]
    pub static ACT_1: [usize; 4] = [
        //       s1
        /* a */  1,
        /* 1 */  1,
        /* 2 */  2,
        /* 3 */  3,
    ];

    #[rustfmt::skip]
    pub static ACT_2: [usize; 8] = [
        //       s1  s2
        /* a */  1,  2,
        /* 1 */  1,  1,
        /* 2 */  2,  2,
        /* 3 */  3,  3,
    ];
}

#[inline]
/// Returns true, if a slices are equal as a sets.
fn is_slices_equal<T: PartialEq>(slice_1: &[T], slice_2: &[T]) -> bool {
    slice_1.iter().all(|s| slice_2.contains(s)) && slice_2.iter().all(|s| slice_1.contains(s))
}

/// 1], lmma 3, case (i), p.108, the top one.
#[test]
fn lattice_of_congruences_1() {
    use acts_over_left_zero_semigroup::*;

    let act = Act::new(acts_over_left_zero_semigroup::ACT_1.to_vec(), 4);

    let act_size = act.size();

    let congruence_set = act.new_congruence_set();

    let control_set = [
        Partition::new(&P_NABLA, act_size), // ∇
        Partition::new(&P_A12, act_size),   // (a12)
        Partition::new(&P_A13, act_size),   // (a13)
        Partition::new(&P_A1_23, act_size), // (a1)(23)
        Partition::new(&P_A1, act_size),    // (a1)
        Partition::new(&P_123, act_size),   // (123)
        Partition::new(&P_12, act_size),    // (12)
        Partition::new(&P_13, act_size),    // (13)
        Partition::new(&P_23, act_size),    // (23)
        Partition::new(&P_DELTA, act_size), // Δ
    ];

    assert!(is_slices_equal(&control_set, &congruence_set));

    let lattice = Lattice::from(congruence_set.as_slice());

    assert!(lattice.is_lattice());
    assert!(lattice.is_modular());
}

// [1], lemma 3, case (i), p.108, the bottom one.
#[test]
fn lattice_of_congruences_2() {
    use acts_over_left_zero_semigroup::*;

    let act = Act::new(acts_over_left_zero_semigroup::ACT_2.to_vec(), 4);

    let act_size = act.size();

    let congruence_set = act.new_congruence_set();

    let control_set = [
        Partition::new(&P_NABLA, act_size), // ∇
        Partition::new(&P_A12, act_size),   // (a12)
        Partition::new(&P_123, act_size),   // (123)
        Partition::new(&P_12, act_size),    // (12)
        Partition::new(&P_13, act_size),    // (13)
        Partition::new(&P_23, act_size),    // (23)
        Partition::new(&P_DELTA, act_size), // Δ
    ];

    assert!(is_slices_equal(&control_set, &congruence_set));

    let lattice = Lattice::from(congruence_set.as_slice());

    assert!(lattice.is_lattice());
    assert!(lattice.is_modular());
}
