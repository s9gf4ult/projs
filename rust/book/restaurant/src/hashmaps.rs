use std::collections::HashMap ;

pub fn coll() {
    let mut m : HashMap<String, u32> = HashMap::new() ;
    m.insert("azaza".to_string(), 10) ;
    m.insert("ololo".to_string(), 20) ;
    m.insert(String::from("kek"), 30) ;
    println!("{:?}", m);
    for (k, v) in &mut m {
        println!("{}: {}", k, v);
        *v += 1 ;
    }
    println!("{:?}", m);

    let v = vec![("one".to_string(), 1), ("two".to_string(), 2), ("three".to_string(), 3)] ;
    let m2 : HashMap<_, _> = v.into_iter().collect() ;
    println!("{:?}", m2);
}
