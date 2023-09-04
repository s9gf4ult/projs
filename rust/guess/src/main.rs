#![feature(iter_next_chunk)]

#[derive(Debug)]
struct Rectangle {
    width: u32,
    height: u32,
}

fn main() {
    let v = vec![1,2,3,4];
    match v.iter().next_chunk() {
        Ok([a,b,c]) => println!("{a}, {b}, {c}"),
        Err(_) => println!("nope")
    }

}
