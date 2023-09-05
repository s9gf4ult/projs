use std::{
    cell::RefCell,
    rc::Rc
};


#[derive(Debug)]
enum List {
    Cons(i32, RefCell<Rc<List>>),
    Nil,
}

use List::*;

impl List {
    fn tail(&self) -> Option<&RefCell<Rc<List>>> {
        match self {
            Cons(_, item) => Some(item),
            Nil => None,
        }
    }
}

fn main() {
    let r:RefCell<i32> = RefCell::new(10);
    println!("{}", r.borrow());
    println!("{}", r.borrow_mut());
}
