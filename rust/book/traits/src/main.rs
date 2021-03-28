fn main() {
    let v1 = vec![1, 2, 50, 10] ;
    let v2 = vec![1., 22.2, 55.2, 29292.] ;
    let l1 = largest(&v1) ;
    println!("{:?}", l1);
    let l2 = largest(&v2) ;
    println!("{:?}", l2);

    let s1 = "stnt1".to_string() ;
    let s2 ;
    let lon ;
    {
        s2 = "snasht2".to_string() ;
        lon = longest(&s1, &s2) ;
        println!("{}", lon);
    }

}

fn largest<T : PartialOrd> (l : &[T]) -> Option<&T> {
    let mut iter = l.into_iter() ;
    let mut lrg = iter.next()? ;
    for item in iter {
        if item > lrg {
            lrg = item
        }
    } ;
    Some(lrg)
}

fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
    if x.len() > y.len() {
        x
    } else {
        y
    }
}


fn longest2<'a, 'b, 'c> (x: &'a str, y: &'b str) -> &'c str
where 'a : 'c
    , 'b : 'c
{
    if x.len() > y.len() {
        x
    } else {
        y
    }
}
