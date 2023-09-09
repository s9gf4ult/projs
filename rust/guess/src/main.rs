use std::{
    cell::RefCell,
    ops::Deref,
    rc::Rc,
    sync::{mpsc, Arc, Mutex},
    thread,
    time::Duration,
};

#[derive(Debug)]
struct T {
    x: i32,
    y: i32,
}

fn f() -> i32 {
    117
}

fn main() {
    let a = T { x: 32, y: 64 };
    match a {
        T { x: x @ 1..=5, .. } => (),
        T {
            x: x @ 1..=10,
            y: y @ (3 | 7 | 17),
        } if y > x => (),
        T {
            x: x @ (11 | 13 | 17),
            ..
        } => println!("{}", x),
        // T { x: if x > 20, .. } => println!("hehe"),
        _ => println!("other"),
    }
    match f() {
        1 => println!("odin"),
        2 => println!("dva"),
        3..=10 => println!("mnoga"),
        a @ 11..=100 => println!("oche mnoga {}", a),
        a @ (111 | 113 | 117) => println!("krasivo {}", a),
        _ => println!("ohuel?"),
    }
}
