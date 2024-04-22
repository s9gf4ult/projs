use std::fmt;
use std::ops::{Add, Mul};

#[derive(Debug, Clone)]
struct Value {
    value: f64,
    grad: f64,
    tree: Tree,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "value: {}, grad: {}", self.value, self.grad)
    }
}

#[derive(Debug, Clone)]
enum Tree {
    Leaf,
    Sum(Box<Value>, Box<Value>),
    Prod(Box<Value>, Box<Value>),
}

impl Value {
    pub fn new(init: f64) -> Self {
        Value {
            value: init,
            grad: 0.0,
            tree: Tree::Leaf,
        }
    }

    pub fn grad(&mut self) {
        self.gradSub(1.0)
    }

    pub fn gradSub(&mut self, root: f64) {
        self.grad = root;
        match &mut self.tree {
            Tree::Leaf => (),
            Tree::Sum(a, b) => {
                a.gradSub(root); // grad of sum is 1, so 1 by root = root
                b.gradSub(root);
            }
            Tree::Prod(a, b) => {
                a.gradSub(root * b.value); // d(a*b) / da = b and vice versa
                b.gradSub(root * a.value);
            }
        }
    }
}

impl Add<Value> for Value {
    type Output = Value;
    fn add(self, rhs: Value) -> Value {
        Value {
            value: self.value + rhs.value,
            grad: 0.0,
            tree: Tree::Sum(Box::new(self), Box::new(rhs)),
        }
    }
}

impl Mul<Value> for Value {
    type Output = Value;
    fn mul(self, rhs: Value) -> Value {
        Value {
            value: self.value * rhs.value,
            grad: 0.0,
            tree: Tree::Prod(Box::new(self), Box::new(rhs)),
        }
    }
}

fn main() {
    let mut a = Value::new(10.0) + Value::new(20.0) * Value::new(3.0);
    println!("{}", a);
    a.grad();
    println!("{:?}", a);
}
