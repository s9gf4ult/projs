fn main() {
    let v1 = vec![1, 2, 50, 10] ;
    let v2 = vec![1., 22.2, 55.2, 29292.] ;
    let l1 = largest(&v1) ;
    println!("{:?}", l1);
    let l2 = largest(&v2) ;
    println!("{:?}", l2);
}

fn largest<T> (l : &[T]) -> Option<&T>
where T : PartialOrd
{
    let mut iter = l.into_iter() ;
    let mut lrg = iter.next()? ;
    for item in iter {
        if item > lrg {
            lrg = item
        }
    } ;
    Some(lrg)
}
