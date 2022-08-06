use std::fmt;
use std::mem;
use std::rc::Rc;
use std::hash::{Hash, Hasher};
use crate::vm::ExeState;

// sizeof(Value) - 1(tag) - 1(len)
const INLSTR_MAX: usize = 14;

#[derive(Clone)]
pub enum Value {
    Nil,
    Boolean(bool),
    Integer(i64),
    Float(f64),
    ShortStr(u8, [u8; INLSTR_MAX]),
    LongStr(Rc<String>),
    Function(fn (&mut ExeState) -> i32),
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Boolean(b) => write!(f, "{b}"),
            Value::Integer(i) => write!(f, "{i}"),
            Value::Float(n) => write!(f, "{n:?}"),
            Value::ShortStr(len, buf) => write!(f, "{}", String::from_utf8_lossy(&buf[..*len as usize])),
            Value::LongStr(s) => write!(f, "{s}"),
            Value::Function(_) => write!(f, "function"),
        }
    }
}

// ANCHOR: peq
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        // TODO compare Integer vs Float
        match (self, other) {
            (Value::Nil, Value::Nil) => true,
            (Value::Boolean(b1), Value::Boolean(b2)) => *b1 == *b2,
            (Value::Integer(i1), Value::Integer(i2)) => *i1 == *i2,
            (Value::Float(f1), Value::Float(f2)) => *f1 == *f2,
            (Value::ShortStr(len1, s1), Value::ShortStr(len2, s2)) => s1[..*len1 as usize] == s2[..*len2 as usize],
            (Value::LongStr(s1), Value::LongStr(s2)) => *s1 == *s2,
            (Value::Function(f1), Value::Function(f2)) => std::ptr::eq(f1, f2),
            (_, _) => false,
        }
    }
}
// ANCHOR_END: peq

impl Eq for Value {}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Value::Nil => (),
            Value::Boolean(b) => b.hash(state),
            Value::Integer(i) => i.hash(state),
            Value::Float(f) => // TODO try to convert to integer
                unsafe {
                    mem::transmute::<f64, i64>(*f).hash(state)
                }
            Value::LongStr(s) => s.hash(state),
            Value::ShortStr(len, buf) => buf[..*len as usize].hash(state),
            Value::Function(f) => (*f as *const usize).hash(state),
        }
    }
}

impl From<()> for Value {
    fn from(_: ()) -> Self {
        Value::Nil
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Value::Boolean(b)
    }
}

impl From<f64> for Value {
    fn from(n: f64) -> Self {
        Value::Float(n)
    }
}

impl From<i64> for Value {
    fn from(n: i64) -> Self {
        Value::Integer(n)
    }
}

impl From<String> for Value {
    fn from(s: String) -> Self {
        let len = s.len();
        if len > INLSTR_MAX {
            Value::LongStr(Rc::new(s))
        } else {
            let mut buf = [0; INLSTR_MAX];
            buf[..len].copy_from_slice(s.as_bytes());
            Value::ShortStr(len as u8, buf)
        }
    }
}

impl From<&str> for Value {
    fn from(s: &str) -> Self {
        s.to_string().into()
    }
}

impl<'a> From<&'a Value> for &'a str {
    fn from(v: &'a Value) -> Self {
        match v {
            Value::LongStr(s) => s,
            Value::ShortStr(len, buf) => std::str::from_utf8(&buf[..*len as usize]).unwrap(),
            _ => panic!("invalid string Value"),
        }
    }
}

impl From<&Value> for String {
    fn from(v: &Value) -> Self {
        match v {
            Value::LongStr(s) => s.as_ref().clone(),
            Value::ShortStr(len, buf) => String::from_utf8_lossy(&buf[..*len as usize]).to_string(),
            _ => panic!("invalid string Value"),
        }
    }
}
