use std::fmt;
use crate::vm::ExeState;

// ANCHOR: value
#[derive(Clone)]
pub enum Value {
    Nil,
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(String),
    Function(fn (&mut ExeState) -> i32),
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Boolean(b) => write!(f, "{b}"),
            Value::Integer(i) => write!(f, "{i}"),
            Value::Float(n) => write!(f, "{n:?}"),
            Value::String(s) => write!(f, "{s}"),
            Value::Function(_) => write!(f, "function"),
        }
    }
}
// ANCHOR_END: value

// ANCHOR: peq
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        // TODO compare Integer vs Float
        match (self, other) {
            (Value::Nil, Value::Nil) => true,
            (Value::Boolean(b1), Value::Boolean(b2)) => *b1 == *b2,
            (Value::Integer(i1), Value::Integer(i2)) => *i1 == *i2,
            (Value::Float(f1), Value::Float(f2)) => *f1 == *f2,
            (Value::String(s1), Value::String(s2)) => *s1 == *s2,
            (Value::Function(f1), Value::Function(f2)) => std::ptr::eq(f1, f2),
            (_, _) => false,
        }
    }
}
// ANCHOR_END: peq
