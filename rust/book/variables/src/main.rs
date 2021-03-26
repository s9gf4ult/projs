fn main() {
}

fn intmove() {
    let a : u32 = 10;
    let b = a;
    println!("{}", a);
}

// fn strmove() {
//     let a = String::from("hello") ;
//     {
//         let b = a;
//         println!("{}", b);
//     }
//     println!("{}", a);
// }

fn mutref() {
    let mut a : u32 = 10;
    let r3 = &a;
    let r2 = &a;
    println!("{}", *r3);
}
