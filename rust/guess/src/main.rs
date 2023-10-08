use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;
use std::mem;

struct T1;

impl Drop for T1 {
    fn drop(&mut self) {
        println!("Dropped T1");
    }
}

struct T2;

impl Drop for T2 {
    fn drop(&mut self) {
        println!("Dropped T2");
    }
}

struct Custom {
    t1: T1,
    t2: T2,
}

impl Drop for Custom {
    fn drop(&mut self) {
        println!("Dropped Custom");
    }
}

fn main() {
    let c = Custom { t1: T1, t2: T2 };
}

// fn get_default<'m, 'v, K, V>(map: &'m mut HashMap<K, V>, key: K) -> &'v mut V
// where
//     K: Clone + Eq + Hash,
//     V: Default,
//     'm: 'v,
// {
//     match map.get_mut(&key) {
//         Some(value) => value,
//         None => {
//             map.insert(key.clone(), V::default());
//             map.get_mut(&key).unwrap()
//         }
//     }
// }

// fn main() {}
