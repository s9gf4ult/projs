#[derive(Debug)]
struct User {
    name: String,
    email: String,
    active: bool,
}

fn main() {
    let mut user = User {
        name: String::from("tntntn"),
        email: String::from("ntnt@snsn.com"),
        active: true,
    } ;

    println!("{}", &user.name);
    user.name = String::from("tnaisht");
    println!("{}", &user.name);

    println!("{}", user.email);
    println!("{:?}", user);
    let tup : (u32, u32) = (1, 20) ;
    println!("{:?}", tup);
}
