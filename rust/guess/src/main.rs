extern crate rand;


// use std::io;
// use std::cmp::Ordering;
// use rand::Rng;

struct Name<'s> {
    val : &'s str
}

struct User<'a, 's : 'a> {
    name : &'a Name<'s>,
    soname : &'a Name<'s>,
}

impl<'a, 's> User<'a, 's> {
    fn name_str(self) -> &'s str {
        self.name.val
    }

}

fn main() {
    let s;
    let name = Name {val: "ivan"};
    let soname = "ivanich";
    {
        let u = User {
            name : & name,
            soname : & Name { val : soname },
        };
        s = u.name_str();
    }
}
