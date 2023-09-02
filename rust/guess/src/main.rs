extern crate rand;
// extern crate thread;

use std::{
    fs::File,
    io,
    cmp::PartialOrd
};

struct T;

impl T { fn x<'a, 'b, 'c>(&'a self, x: &'b i32, y: &'c i32) -> &'a i32 { panic!("FIXME: not implemented") } }

#[derive(Debug)]
struct Point<X1, Y1> {
    x: X1,
    y: Y1,
}

impl<'x1, X1, Y1> Point<&'x1 X1, Y1> {
    #[inline]
    fn mixup_ref<'y2, X2, Y2>(&self, other: &Point<X2, &'y2 Y2>) -> Point<&'x1 X1, &'y2 Y2> {
        Point {
            x: self.x,
            y: other.y,
        }
    }
}

impl<X1: Clone, Y1> Point<X1, Y1> {
    #[inline]
    fn mixup_clone<X2, Y2: Clone>(&self, other: &Point<X2, Y2>) -> Point<X1, Y2> {
        Point {
            x: self.x.clone(),
            y: other.y.clone(),
        }
    }
}

#[derive(Debug)]
enum Either<L, R> {
    Left(L),
    Right(R)
}

fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
    if x.len() > y.len() {
        x
    } else {
        y
    }
}

fn longest2<'a, 'b, 'c>(x: &'a str, y: &'b str) -> &'c str
where
    'a: 'c, 'b: 'c
{
    if x.len() > y.len() {
        x
    } else {
        y
    }
}

impl <'a, 'b> Either<&'a str, &'b str> {
    fn longest(x: &'a str, y: &'b str) -> Self {
        if x.len() > y.len() {
            Either::Left(x)
        } else {
            Either::Right(y)
        }
    }
}


#[derive(Debug)]
struct Rectangle {
    width: u32,
    height: u32,
}

fn main() {
    let mut list = [
        Rectangle { width: 10, height: 1 },
        Rectangle { width: 3, height: 5 },
        Rectangle { width: 7, height: 12 },
    ];

    let mut num_sort_operations = 0;
    list.sort_by_cached_key(|r| {
        num_sort_operations += 1;
        r.width
    });
    println!("{:#?}, sorted in {num_sort_operations} operations", list);
}
