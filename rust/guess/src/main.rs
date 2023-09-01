extern crate rand;

use std::{
    fs::File,
    io,
    cmp::PartialOrd
};

#[derive(Debug)]
struct Point<X1, Y1> {
    x: X1,
    y: Y1,
}

impl<X1: Copy, Y1> Point<X1, Y1> {
    fn mixup<X2, Y2: Copy>(&self, other: &Point<X2, Y2>) -> Point<X1, Y2> {
        Point {
            x: self.x,
            y: other.y,
        }
    }
}

impl<X1: Clone, Y1> Point<X1, Y1> {
    fn mixup_clone<X2, Y2: Clone>(&self, other: &Point<X2, Y2>) -> Point<X1, Y2> {
        Point {
            x: self.x.clone(),
            y: other.y.clone(),
        }
    }
}

#[derive(Debug)]
enum EitherStr<'t1, 't2> {
    Left(&'t1 str),
    Right(&'t2 str)
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

fn longest3<'a, 'b>(x: &'a str, y: &'b str) -> EitherStr<'a, 'b>{
    if x.len() > y.len() {
        EitherStr::Left(x)
    } else {
        EitherStr::Right(y)
    }

}


fn main() {
    let string1 = String::from("long string is long");
    let result;
    {
        let string2 = String::from("xyz");
        result = match longest3(string1.as_str(), string2.as_str()) {
            EitherStr::Left(s1) => s1,
            EitherStr::Right(s2) => panic!("Nope")
        }
    }
    println!("The longest string is:{}", result);
}
