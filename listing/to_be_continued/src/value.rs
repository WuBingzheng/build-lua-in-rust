use std::fmt;
use std::mem;
use std::rc::Rc;
use std::cell::RefCell;
use std::hash::{Hash, Hasher};
use std::collections::HashMap;
use crate::parse::FuncProto;
use crate::vm::{ExeState, LuaClosure};
use crate::utils::{ftoi, set_vec};

const SHORT_STR_MAX: usize = 14; // sizeof(Value) - 1(tag) - 1(len)
const MID_STR_MAX: usize = 48 - 1;

#[derive(Clone)]
pub enum Value {
    Nil,
    Boolean(bool),
    Integer(i64),
    Float(f64),
    ShortStr(u8, [u8; SHORT_STR_MAX]),
    MidStr(Rc<(u8, [u8; MID_STR_MAX])>),
    LongStr(Rc<Vec<u8>>),
    Table(Rc<RefCell<Table>>),
    RustFunction(fn (&mut ExeState) -> i32),
    RustClosure(Rc<RefCell<Box<dyn FnMut (&mut ExeState) -> i32>>>),
    LuaFunction(Rc<FuncProto>),
    LuaClosure(Rc<LuaClosure>),
}

// ANCHOR: table
pub struct Table {
    pub array: Vec<Value>,
    pub map: HashMap<Value, Value>,
}
// ANCHOR_END: table

impl Table {
    pub fn new(narray: usize, nmap: usize) -> Self {
        Table {
            array: Vec::with_capacity(narray),
            map: HashMap::with_capacity(nmap),
        }
    }

    pub fn index(&self, key: &Value) -> &Value {
        match key {
            // TODO float
            &Value::Integer(i) => self.index_array(i),
            _ => self.map.get(key).unwrap_or(&Value::Nil),
        }
    }
    pub fn index_array(&self, i: i64) -> &Value {
        self.array.get(i as usize - 1)
            .unwrap_or_else(|| self.map.get(&Value::Integer(i as i64))
                .unwrap_or(&Value::Nil))
    }

    pub fn new_index(&mut self, key: Value, value: Value) {
        match key {
            // TODO float
            Value::Integer(i) => self.new_index_array(i, value),
            _ => {
                self.map.insert(key, value);
            }
        }
    }
    pub fn new_index_array(&mut self, i: i64, value: Value) {
        // this is not same with Lua's official implement
        if i > 0 && (i < 4 || i < self.array.capacity() as i64 * 2) {
            set_vec(&mut self.array, i as usize - 1, value);
        } else {
            self.map.insert(Value::Integer(i), value);
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Boolean(b) => write!(f, "{b}"),
            Value::Integer(i) => write!(f, "{i}"),
            Value::Float(n) => write!(f, "{n:?}"),
            Value::ShortStr(len, buf) => write!(f, "{}", String::from_utf8_lossy(&buf[..*len as usize])),
            Value::MidStr(s) => write!(f, "{}", String::from_utf8_lossy(&s.1[..s.0 as usize])),
            Value::LongStr(s) => write!(f, "{}", String::from_utf8_lossy(s)),
            Value::Table(t) => write!(f, "table: {:?}", Rc::as_ptr(t)),
            Value::RustFunction(_) => write!(f, "function"),
            Value::RustClosure(_) => write!(f, "function"),
            Value::LuaFunction(l) => write!(f, "function: {:?}", Rc::as_ptr(l)),
            Value::LuaClosure(l) => write!(f, "function: {:?}", Rc::as_ptr(l)),
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Boolean(b) => write!(f, "{b}"),
            Value::Integer(i) => write!(f, "{i}"),
            Value::Float(n) => write!(f, "{n:?}"),
            Value::ShortStr(len, buf) => write!(f, "'{}'", String::from_utf8_lossy(&buf[..*len as usize])),
            Value::MidStr(s) => write!(f, "\"{}\"", String::from_utf8_lossy(&s.1[..s.0 as usize])),
            Value::LongStr(s) => write!(f, "'''{}'''", String::from_utf8_lossy(s)),
            Value::Table(t) => {
                let t = t.borrow();
                write!(f, "table:{}:{}", t.array.len(), t.map.len())
            }
            Value::RustFunction(_) => write!(f, "rust function"),
            Value::RustClosure(_) => write!(f, "rust closure"),
            Value::LuaFunction(_) => write!(f, "Lua function"),
            Value::LuaClosure(_) => write!(f, "Lua closure"),
        }
    }
}

// ANCHOR: peq
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Nil, Value::Nil) => true,
            (&Value::Boolean(b1), &Value::Boolean(b2)) => b1 == b2,
            (&Value::Integer(i1), &Value::Integer(i2)) => i1 == i2,
            (&Value::Integer(i), &Value::Float(f)) |
            (&Value::Float(f), &Value::Integer(i)) => i as f64 == f && i == f as i64,
            (&Value::Float(f1), &Value::Float(f2)) => f1 == f2,
            (Value::ShortStr(len1, s1), Value::ShortStr(len2, s2)) => s1[..*len1 as usize] == s2[..*len2 as usize],
            (Value::MidStr(s1), Value::MidStr(s2)) => s1.1[..s1.0 as usize] == s2.1[..s2.0 as usize],
            (Value::LongStr(s1), Value::LongStr(s2)) => s1 == s2,
            (Value::Table(t1), Value::Table(t2)) => Rc::as_ptr(t1) == Rc::as_ptr(t2),
            (Value::RustFunction(f1), Value::RustFunction(f2)) => std::ptr::eq(f1, f2),
            (Value::RustClosure(f1), Value::RustClosure(f2)) => Rc::as_ptr(f1) == Rc::as_ptr(f2),
            (Value::LuaFunction(f1), Value::LuaFunction(f2)) => Rc::as_ptr(f1) == Rc::as_ptr(f2),
            (Value::LuaClosure(f1), Value::LuaClosure(f2)) => Rc::as_ptr(f1) == Rc::as_ptr(f2),
            (_, _) => false,
        }
    }
}
// ANCHOR_END: peq

// ANCHOR: eq
impl Eq for Value {}
// ANCHOR_END: eq

// ANCHOR: partialord
impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            // numbers
            (Value::Integer(i1), Value::Integer(i2)) => Some(i1.cmp(i2)),
            (Value::Integer(i), Value::Float(f)) => (*i as f64).partial_cmp(f),
            (Value::Float(f), Value::Integer(i)) => f.partial_cmp(&(*i as f64)),
            (Value::Float(f1), Value::Float(f2)) => f1.partial_cmp(f2),

            // strings
            (Value::ShortStr(len1, s1), Value::ShortStr(len2, s2)) => Some(s1[..*len1 as usize].cmp(&s2[..*len2 as usize])),
            (Value::MidStr(s1), Value::MidStr(s2)) => Some(s1.1[..s1.0 as usize].cmp(&s2.1[..s2.0 as usize])),
            (Value::LongStr(s1), Value::LongStr(s2)) => Some(s1.cmp(s2)),

            // strings of different types
            (Value::ShortStr(len1, s1), Value::MidStr(s2)) => Some(s1[..*len1 as usize].cmp(&s2.1[..s2.0 as usize])),
            (Value::ShortStr(len1, s1), Value::LongStr(s2)) => Some(s1[..*len1 as usize].cmp(s2)),
            (Value::MidStr(s1), Value::ShortStr(len2, s2)) => Some(s1.1[..s1.0 as usize].cmp(&s2[..*len2 as usize])),
            (Value::MidStr(s1), Value::LongStr(s2)) => Some(s1.1[..s1.0 as usize].cmp(s2)),
            (Value::LongStr(s1), Value::ShortStr(len2, s2)) => Some(s1.as_ref().as_slice().cmp(&s2[..*len2 as usize])),
            (Value::LongStr(s1), Value::MidStr(s2)) => Some(s1.as_ref().as_slice().cmp(&s2.1[..s2.0 as usize])),

            (_, _) => None,
        }
    }
}
// ANCHOR_END: partialord

impl Value {
    pub fn same(&self, other: &Self) -> bool {
        // eliminate Integer and Float with same number value
        mem::discriminant(self) == mem::discriminant(other) && self == other
    }
    pub fn ty(&self) -> &'static str {
        match self {
            &Value::Nil => "nil",
            &Value::Boolean(_) => "boolean",
            &Value::Integer(_) => "number",
            &Value::Float(_) => "number",
            &Value::ShortStr(_, _) => "string",
            &Value::MidStr(_) => "string",
            &Value::LongStr(_) => "string",
            &Value::Table(_) => "table",
            &Value::RustFunction(_) => "function",
            &Value::RustClosure(_) => "function",
            &Value::LuaFunction(_) => "function",
            &Value::LuaClosure(_) => "function",
        }
    }

    pub fn index(&self, key: &Value) -> Value {
        match self {
            Value::Table(t) => t.borrow().index(key).clone(),
            _ => todo!("meta __index"),
        }
    }
    pub fn index_array(&self, i: i64) -> Value {
        match self {
            Value::Table(t) => t.borrow().index_array(i).clone(),
            _ => todo!("meta __index"),
        }
    }

    pub fn new_index(&self, key: Value, value: Value) {
        match self {
            Value::Table(t) => t.borrow_mut().new_index(key, value),
            _ => todo!("meta __index"),
        }
    }
    pub fn new_index_array(&self, i: i64, value: Value) {
        match self {
            Value::Table(t) => t.borrow_mut().new_index_array(i, value),
            _ => todo!("meta __newindex"),
        }
    }
}

// ANCHOR: hash
impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Value::Nil => (),
            Value::Boolean(b) => b.hash(state),
            Value::Integer(i) => i.hash(state),
            &Value::Float(f) =>
                if let Some(i) = ftoi(f) {
                    i.hash(state)
                } else {
                    (f.to_bits() as i64).hash(state)
                }
            Value::ShortStr(len, buf) => buf[..*len as usize].hash(state),
            Value::MidStr(s) => s.1[..s.0 as usize].hash(state),
            Value::LongStr(s) => s.hash(state),
            Value::Table(t) => Rc::as_ptr(t).hash(state),
            Value::RustFunction(f) => (*f as *const usize).hash(state),
            Value::RustClosure(f) => Rc::as_ptr(f).hash(state),
            Value::LuaFunction(f) => Rc::as_ptr(f).hash(state),
            Value::LuaClosure(f) => Rc::as_ptr(f).hash(state),
        }
    }
}
// ANCHOR_END: hash

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

// ANCHOR: from_num
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
// ANCHOR_END: from_num

// ANCHOR: from_vec_string
// convert &[u8], Vec<u8>, &str and String into Value
impl From<&[u8]> for Value {
    fn from(v: &[u8]) -> Self {
        vec_to_short_mid_str(v).unwrap_or_else(||Value::LongStr(Rc::new(v.to_vec())))
    }
}
impl From<&str> for Value {
    fn from(s: &str) -> Self {
        s.as_bytes().into() // &[u8]
    }
}

impl From<Vec<u8>> for Value {
    fn from(v: Vec<u8>) -> Self {
        vec_to_short_mid_str(&v).unwrap_or_else(||Value::LongStr(Rc::new(v)))
    }
}
impl From<String> for Value {
    fn from(s: String) -> Self {
        s.into_bytes().into() // Vec<u8>
    }
}

fn vec_to_short_mid_str(v: &[u8]) -> Option<Value> {
    let len = v.len();
    if len <= SHORT_STR_MAX {
        let mut buf = [0; SHORT_STR_MAX];
        buf[..len].copy_from_slice(v);
        Some(Value::ShortStr(len as u8, buf))

    } else if len <= MID_STR_MAX {
        let mut buf = [0; MID_STR_MAX];
        buf[..len].copy_from_slice(v);
        Some(Value::MidStr(Rc::new((len as u8, buf))))

    } else {
        None
    }
}
// ANCHOR_END: from_vec_string

// ANCHOR: to_vec_string
impl<'a> From<&'a Value> for &'a [u8] {
    fn from(v: &'a Value) -> Self {
        match v {
            Value::ShortStr(len, buf) => &buf[..*len as usize],
            Value::MidStr(s) => &s.1[..s.0 as usize],
            Value::LongStr(s) => s,
            _ => panic!("invalid string Value"),
        }
    }
}

impl<'a> From<&'a Value> for &'a str {
    fn from(v: &'a Value) -> Self {
        std::str::from_utf8(v.into()).unwrap()
    }
}

impl From<&Value> for String {
    fn from(v: &Value) -> Self {
        String::from_utf8_lossy(v.into()).to_string()
    }
}

impl From<&Value> for bool {
    fn from(v: &Value) -> Self {
        !matches!(v, Value::Nil | Value::Boolean(false))
    }
}

impl From<&Value> for i64 {
    fn from(v: &Value) -> Self {
        match v {
            Value::Integer(i) => *i,
            Value::Float(f) => *f as i64,
            Value::ShortStr(_, _) => todo!("tonumber"),
            Value::MidStr(_) => todo!("tonumber"),
            Value::LongStr(_) => todo!("tonumber"),
            _ => panic!("invalid string Value"),
        }
    }
}

// ANCHOR_END: to_vec_string
