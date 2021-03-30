use std::collections::HashMap ;
use std::hash::Hash ;

// fn calcFib<F> (c: &mut MapCacher<F, u32, u64>, x : u32) -> u64
//     where F : Fn(&mut MapCacher<F, u32, u64>, u32) -> u64
// {
//     match x {
//         0 => 1,
//         1 => 1,
//         _ => c.execFixCopy(x - 1) + c.execFixCopy(x - 2),
//     }
// }

struct RecFib<'s> {
    f : fn (&'s RecFib, u32) -> u64
}

fn main() {
    let fibs = Fibs::new() ;
    let fv : Vec<u64> = fibs.take_while(|x| x < &1000000000000).collect() ;
    println!("{:?}", fv);
}

struct Cacher<F, V> {
    calc : F,
    value : Option<V>
}

impl <F, V> Cacher<F, V> {
    fn new (calc : F) -> Cacher<F, V> {
        Cacher {
            calc,
            value: None
        }
    }
}

impl <F, V> Cacher<F, V>
where
    F : Fn() -> V, V : Copy
{
    fn exec(&mut self, value : V) -> V {
        match self.value {
            Some(res) => res,
            None => {
                let res = (self.calc)() ;
                self.value = Some(res) ;
                res
            }
        }
    }
}

struct MapCacher<F, K, V>
{
    calc : F,
    values : HashMap<K, V>
}

impl <F, K, V> MapCacher<F, K, V>
{
    pub fn new(calc: F) -> MapCacher<F, K, V> {
        MapCacher {
            calc,
            values : HashMap::new()
        }
    }
}

impl <F, K, V> MapCacher<F, K, V>
where
    F : Fn(K) -> V, K : Eq + Hash + Copy, V : Copy
{
    pub fn execCopy(&mut self, key : K) -> V {
        match self.values.get(&key) {
            Some(res) => *res,
            None => {
                let res = (self.calc)(key) ;
                self.values.insert(key, res) ;
                res
            }
        }
    }
}


// impl <F, K, V> MapCacher<F, K, V>
// where
//     F : Fn(&mut MapCacher<F, K, V>, K) -> V, K : Eq + Hash + Copy, V : Copy
// {
//     pub fn execFixCopy(&mut self, key : K) -> V {
//         match self.values.get(&key) {
//             Some(res) => *res,
//             None => {
//                 let res = (self.calc)(self, key) ;
//                 self.values.insert(key, res) ;
//                 res
//             }
//         }
//     }
// }

pub struct Fibs {
    pub fib : u32,
    a : u64, // fib - 1
    b : u64, // fib - 2
}

impl Fibs {
    fn new() -> Fibs {
        Fibs {
            fib : 0,
            a : 1,
            b : 1
        }
    }
}

impl Iterator for Fibs {
    type Item = u64 ;
    fn next(&mut self) -> Option<Self::Item> {
        let res = match self.fib {
            0 => 1,
            1 => 1,
            _ => self.a + self.b
        } ;
        self.b = self.a ;
        self.a = res ;
        self.fib += 1 ;
        Some(res)
    }
}
