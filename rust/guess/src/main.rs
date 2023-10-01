use std::fmt::Debug;

trait Tr {
    fn tr(&self) {
        println!("TR!");
    }
}

#[derive(Debug)]
struct I;

impl Tr for I {}

#[derive(Debug)]
struct DST<T: ?Sized> {
    a: usize,
    b: T,
}

fn main() {
    let d = DST { a: 10, b: I };
    let r: &DST<dyn Tr> = &d;
    r.b.tr();
    d.b.tr();
}
