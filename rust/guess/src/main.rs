extern crate serde;
extern crate serde_json;

use std::{
    cell::RefCell,
    ops::Deref,
    rc::Rc,
    sync::{mpsc, Arc, Mutex},
    thread,
    time::Duration,
};

mod ser;

trait Pilot {
    fn fly(&self);
}

trait Wizard {
    fn fly(&self);
}

trait X: Pilot + Wizard {}

#[derive(Debug, Clone, Copy)]
struct Human;

impl Pilot for Human {
    fn fly(&self) {
        println!("This is your captain speaking.");
    }
}

impl Wizard for Human {
    fn fly(&self) {
        println!("Up!");
    }
}

impl Human {
    fn fly(&self, s: &str) {
        println!("*waving arms furiously {}*", s);
    }
}

fn main() {
    let p = Human;
    // p.fly();
    p.fly("hzhuh");
}
