struct StrSlice<'a> {
    string: String,
    slice: Option<&'a str>,
}

fn mkSlice<'a>() -> StrSlice<'a> {
    let s = "dummy".to_string() ;
    StrSlice {
        string: s,
        slice: None,
    }
}

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
