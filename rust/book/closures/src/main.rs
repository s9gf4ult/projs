use std::collections::HashMap ;
use std::hash::Hash ;

fn calcFib(x : u32) -> u64 {
    match x {
        0 => 1,
        1 => 1,
        _ => calcFib(x - 1) + calcFib(x - 2),
    }
}

struct RecFib<'s> {
    f : fn (&'s RecFib, u32) -> u64
}

fn main() {
    let mut fibCache : MapCacher<_, u32, u64> = MapCacher::new(calcFib) ;
    let fib = fibCache.execCopy(3) ;
    println!("{}", fib);
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
//     F : Fn(Fn(K) -> V, K) -> V, K : Eq + Hash + Copy + Sized, V : Copy + Sized
// {
//     pub fn execFixCopy(&mut self, key : K) -> V {
//         match self.values.get(&key) {
//             Some(res) => *res,
//             None => {
//                 let res = (self.calc)(|x| self.execFixCopy(x), key) ;
//                 self.values.insert(key, res) ;
//                 res
//             }
//         }
//     }
// }


// impl <'a, F, K, V> MapCacher<F, &'a K, &'a V>
// where
//     F : Fn(&'a K) -> &'a V, K : Eq + Hash
// {
//     pub fn execRef (&mut self, key : &'a K) -> &'a V {
//         match self.values.get(&key) {
//             Some(res) => *res,
//             None => {
//                 let res = (self.calc)(key) ;
//                 self.values.insert(key, res) ;
//                 res
//             }
//         }
//     }
// }
