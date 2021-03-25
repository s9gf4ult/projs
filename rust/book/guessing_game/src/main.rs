use rand::Rng;
use std::cmp::Ordering;
use std::io;

fn main() {
    println!("Guess");
    let secret: u32 = rand::thread_rng().gen_range(1, 101);

    println!("Number is: {}", secret);


    loop {
        let mut guess = String::new();

        println!("Input guess");
        io::stdin()
            .read_line(&mut guess)
            .expect("Failed to read line");
        println!("You typed {}", guess);

        let guess: u32 = match guess.trim().parse() {
            Ok(a) => a,
            Err(_) => {
                println!("Not a number");
                continue;
            }
        } ;

        match guess.cmp(&secret) {
            Ordering::Less => println!("Small"),
            Ordering::Equal => {
                println!("Guessed");
                break;
            }
            Ordering::Greater => println!("Too big"),
        }
    }
}
