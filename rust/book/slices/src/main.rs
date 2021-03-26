fn main() {
    let s = String::from("tntntn wfwfwf");
    let slice = getslice(&s) ;
    println!("{}", s);
    // s.clear();
    println!("{}", slice);
}

fn getslice(s : &str) -> &str {
    return &s[0..5];
}
