use std::fmt;
use crate::vm::ExeState;

#[derive(Clone)]
pub enum Value {
    Nil,
    String(String),
    Function(fn (&mut ExeState) -> i32),
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::String(s) => write!(f, "{s}"),
            Value::Function(_) => write!(f, "function"),
        }
    }
}
