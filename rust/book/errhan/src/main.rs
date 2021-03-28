use std::{fs::File, io, io::Read} ;
use std::convert::From ;

fn main() {
    let name = "hello.txt".to_string() ;
    let ret = readf(&name) ;
    println!("{:?}", ret);
    let ret = readf2(&name) ;
    println!("{:?}", ret);
    let ret = readf3(&name) ;
    println!("{:?}", ret);
}


fn readf (s : &String) -> Result<String, io::Error> {
    let mut f = match File::open(s) {
        Ok(f) => f,
        Err(e) => return Err(e)
    } ;
    let mut ret = String::new() ;
    match f.read_to_string(&mut ret) {
        Ok(_) => return Ok(ret),
        Err(e) => return Err(e)
    }
}


fn readf2 (s : &String) -> Result<String, io::Error> {
    let mut f = File::open(s)? ;
    let mut ret = String::new() ;
    f.read_to_string(&mut ret)? ;
    Ok(ret)
}



fn readf3 (s : &String) -> Result<String, io::Error> {
    let mut f = match File::open(s) {
        Ok(f) => f,
        Err(e) => return Err(io::Error::from(e))
    } ;
    let mut ret = String::new() ;
    match f.read_to_string(&mut ret) {
        Ok(_) => return Ok(ret),
        Err(e) => return Err(io::Error::from(e))
    }
}
