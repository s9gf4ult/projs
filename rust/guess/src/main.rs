use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;
use std::mem;

struct Inspector<'a>(&'a u8);

impl<'a> Drop for Inspector<'a> {
    fn drop(&mut self) {}
}

fn main() {
    let days = Box::new(1);
    {
        let days = Box::new(1);
        let inspector = Some(Inspector(&days));
        println!("Findol");
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
