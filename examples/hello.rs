fn _f(n: usize) {
    println!("n = {} = {:b}", n, n);
    
    for m in 1..=n/2 {
        println!("{:b} + {:b}", m, n - m);
    }
}

fn _print_classes(m: usize) {
    let max_number_of_classes = (m as f32).log2().ceil() as usize;

    dbg!(&max_number_of_classes);

    let vec = (1..=m).map(
        |n| {
            (n, n.count_ones() as usize)
        }
    ).collect::<Vec<(usize, usize)>>();

    for class_ref in 1..max_number_of_classes {
        print!("[{}]: ", class_ref);

        for &(number, class) in &vec {
            if class == class_ref {
                print!("{:b}, ", number);
            }
        }

        println!();
    }
}

fn main() {
    // congruencer::main(4);
    _print_classes(100);
}
