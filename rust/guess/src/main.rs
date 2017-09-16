extern crate rand;


// use std::io;
// use std::cmp::Ordering;
// use rand::Rng;

// fn take_str(s : & String) -> String {
//     let mut ret = s.clone();
//     ret.push_str("much hello");
//     ret
// }

fn main() {
    let s = String::from("жопа");
    let t : String = s.chars().skip(3).collect();
    println!("{}", s);
    println!("{}", t);
}

// thread 'main' panicked at 'byte index 3 is not a char boundary; it is inside 'о' (bytes 2..4) of `жопа`', src/libcore/str/mod.rs:1771
// note: Run with `RUST_BACKTRACE=1` for a backtrace.
