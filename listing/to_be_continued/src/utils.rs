use std::cmp::Ordering;
use crate::value::Value;

pub fn ftoi(f: f64) -> Option<i64> {
    let i = f as i64;
    if i as f64 != f {
        None
    } else {
        Some(i)
    }
}

pub fn set_vec(vec: &mut Vec<Value>, i: usize, value: Value) {
    match i.cmp(&vec.len()) {
        Ordering::Less => vec[i] = value,
        Ordering::Equal => vec.push(value),
        Ordering::Greater => {
            vec.resize(i, Value::Nil);
            vec.push(value);
        }
    }
}
