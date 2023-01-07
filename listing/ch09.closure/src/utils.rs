pub fn ftoi(f: f64) -> Option<i64> {
    let i = f as i64;
    if i as f64 != f {
        None
    } else {
        Some(i)
    }
}
