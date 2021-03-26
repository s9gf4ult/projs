fn main() {
    let a : [u32; 3] = [1, 2, 3];
    let mut i :usize = 0 ;
    while i < 2 {
        i += 1;
    } ;


    println!("wow: {}", a[i]);

    for el in a.iter() {
        println!("{}", el);
    }
}

fn ups(a : &mut usize) {
    *a += 1;
}
