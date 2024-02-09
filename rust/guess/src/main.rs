// use std::collections::HashMap;
// use std::fmt::Debug;
// use std::hash::Hash;
// use std::mem;
// use std::ops::{Deref, DerefMut};

fn main() {
    let v: Vec<i32> = vec![1, 2, 3, 4];
    let mut t = v.iter();
    let s: i32 = t.by_ref().take(3).cloned().sum();
    println!("{s}");
    for i in t {
        println!("{}", i);
    }
}

// fn get_default<'m, 'v, K, V>(map: &'m mut HashMap<K, V>, key: K) -> &'v mut V
// where
//     K: Clone + Eq + Hash,
//     V: Default,
//     'm: 'v,
// {
//     if map.contains_key(&key) {
//         map.get_mut(&key).unwrap()
//     } else {
//         map.insert(key.clone(), V::default());
//         map.get_mut(&key).unwrap()
//     }
// }
