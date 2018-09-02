extern crate rand;

// use std::iter;
use std::sync::mpsc;
use std::thread;
// use std::io;
// use std::cmp::Ordering;
// use rand::Rng;

fn main() {
    let (tx_plus, rx) = mpsc::channel();
    let tx_minus = tx_plus.clone();

    let h = thread::spawn(move || {
        for x in 1..10_000_000 {
            tx_plus.send(x).unwrap();
        }
    });

    let h2 = thread::spawn(move || {
        for x in (-10_000_000..-1).rev() {
            tx_minus.send(x).unwrap();
        }
    });

    let mut res : i64 = 0;
    loop {
        match rx.recv() {
            Ok(x) => res += x,
            Err(_) => break
            // All senders are dropped;
        }
    }

    println!("{:?}", res);
    h.join().unwrap();
    h2.join().unwrap();
}
