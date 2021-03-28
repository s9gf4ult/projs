#[derive(Debug)]
struct User {
    name: String,
    email: String,
    active: bool,
}

impl User {
    fn desc(&self) -> String {
        let mut ret = String::from(&self.name) ;
        ret.push_str(" <") ;
        ret.push_str(&self.email) ;
        ret.push_str(">");
        return ret ;
    }

    fn gennname(&mut self) {
        if (self.name.is_empty()) {
            let opname = self.email.split("@").next() ;
            match opname {
                None => return ,
                Some(name) => {
                    self.name = String::from(name) ;
                }
            }
        }
    }
}


fn main() {
    let mut user = User {
        name: String::from("wfwfw"),
        email: String::from("YOGOGOG@snsn.com"),
        active: true,
    } ;

    println!("{}", &user.name);
    user.name = String::from("tnaisht");
    println!("{}", &user.name);

    println!("{}", user.email);
    println!("{:?}", user);
    let tup : (u32, u32) = (1, 20) ;
    println!("{:?}", tup);
    println!("{}", user.desc());
    let mut user = User {
        name: String::new(),
        ..user
    } ;
    user.gennname();
    println!("{:?}", user);
}
