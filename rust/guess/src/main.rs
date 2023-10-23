// use std::collections::HashMap;
// use std::fmt::Debug;
// use std::hash::Hash;
// use std::mem;
// use std::ops::{Deref, DerefMut};

#[derive(Debug)]
struct T;

trait Foo {}

impl Foo for T {}

fn main() {
    let x = &T;
    dbg!(x);
    let t: &dyn Foo = &T;
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
