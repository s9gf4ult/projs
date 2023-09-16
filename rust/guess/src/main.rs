struct T {
    a: usize,
    b: usize,
    x: i32,
    y: i32,
    z: i32,
}

fn main() {
    let t = T {
        a: 10,
        b: 20,
        x: 0,
        y: 1,
        z: 2,
    };
    let t2 = T { y: 5, ..t };
    println!(r"abc\nabc")
}
