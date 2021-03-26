struct User {
    name: String,
    email: String,
    active: bool,
}

fn main() {
    let mut  user = User {
        name: String::from("tntntn"),
        email: String::from("ntnt@snsn.com"),
        active: true,
    } ;

    println!("{}", &user.name);
    user.name = String::from("tnaisht");
    println!("{}", &user.name);

    let user2 = User {
        name: String::from("ururur"),
        ..user
    };

    println!("{}", user.email);
}
