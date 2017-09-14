extern crate rand;


use std::io;
use std::cmp::Ordering;
use rand::Rng;

fn main() {
    println!("Enter some shit");
    let r : u32 = rand::thread_rng().gen_range(1,101);
    loop {
        let mut s = String::new();
        io::stdin().read_line(&mut s).expect("fuck");
        let num : u32 = match s.trim().parse() {
            Ok(num) => num,
            Err(_) => {
                println!("Wrong numger, reenter");
                continue;
            }
        };
        match num.cmp(& r) {
            Ordering::Less =>
                println!("too small"),
            Ordering::Greater =>
                println!("too big"),
            Ordering::Equal => {
                println!("guessed!");
                break;
            },
        }
    }
}
