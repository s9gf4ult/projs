// use std::collections::HashMap;
// use std::fmt::Debug;
// use std::hash::Hash;
// use std::mem;
use std::ops::{Deref, DerefMut};

struct T<Ref>(Ref);

struct U;

impl<Ref: Deref> Deref for T<Ref> {
    type Target = <Ref as Deref>::Target;
    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}

impl<Ref: DerefMut> DerefMut for T<Ref> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0.deref_mut()
    }
}

trait Foo {
    fn foo(self) -> &'static str
    where
        Self: Sized,
    {
        std::any::type_name::<Self>()
    }
}

// impl<Ref> Foo for T<Ref> {}

// impl<Ref> Foo for &T<Ref> {}

// impl<Ref> Foo for &mut T<Ref> {}

// impl Foo for U {}

// impl Foo for &U {}

impl Foo for &mut U {}

fn main() {
    let mut u = U;
    let mut t = T(&mut u);
    // println!("{}", t.foo());
    // println!("{}", <T<&mut U> as Deref>::Target::foo(*t.deref()));
    // println!("{}", <&<T<&mut U> as Deref>::Target>::foo(t.deref()));
    println!(
        "{}",
        <&mut <T<&mut U> as Deref>::Target>::foo(t.deref_mut())
    );
    // println!("{}", T::foo(t));
    // println!("{}", <&mut T>::foo(&mut t));
    // println!("{}", T::foo(t));

    // T::foo(t);
    // <&T>::foo(&t);
    // <&mut T>::foo(&mut t);
    // let u: <T as Deref>::Target = U;
    // <<T as Deref>::Target>::foo(*t);
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
