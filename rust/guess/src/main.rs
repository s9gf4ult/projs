extern crate rand;

use std::iter;
use std::sync::mpsc;
use std::thread;
use std::io;
// use std::cmp::Ordering;
// use rand::Rng;

macro_rules! zip {
    ($x: expr) => ($x);
    ($x: expr, $($y: expr), +) => (
        $x.iter().zip(
            zip!($($y), +))
    )
}

fn main() {
    let v = vec!["yoba","yoba","boba", "boba","hueba", "hueba"];
    let res = std::iter::repeat(v.iter())
      //  .take(4)
        .flatten() ;
        // .take(11)
    let v2 = vec![1,2] ;
    let res2 = std::iter::repeat(v2.iter())
        .flatten()
        .take(11)
        .collect::<Vec<_>>();

    let z = zip!(res2, res);
       // .collect::<Vec<_>>();
    dbg!(z.last());
    // println!("{:?}", z);
}
