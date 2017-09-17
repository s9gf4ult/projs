extern crate rand;


// use std::io;
// use std::cmp::Ordering;
// use rand::Rng;

struct Name<'a> {
    val : &'a str
}

struct User<'a> {
    name : &'a Name<'a>,
    soname : &'a Name<'a>,
}

impl<'a> User<'a> {
    fn name_str(self) -> &'a str {
        self.name.val
    }

}

fn main() {
    let name;
    {
        let u = User {
            name : Name { val: "ivan" },
            soname : Name { val : "ivanich" },
        };
        // name = u.name_str;
    }
}
