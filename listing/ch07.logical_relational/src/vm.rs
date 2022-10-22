use std::io::{Read, Write};
use std::rc::Rc;
use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::HashMap;
use crate::bytecode::ByteCode;
use crate::value::{Value, Table};
use crate::parse::ParseProto;
use crate::utils::ftoi;

// ANCHOR: print
// "print" function in Lua's std-lib.
// It supports only 1 argument and assumes the argument is at index:1 on stack.
fn lib_print(state: &mut ExeState) -> i32 {
    println!("{}", state.stack[state.func_index + 1]);
    0
}
// ANCHOR_END: print

// ANCHOR: state
pub struct ExeState {
    globals: HashMap<String, Value>,
    stack: Vec::<Value>,
    func_index: usize,
}
// ANCHOR_END: state

// ANCHOR: new
impl ExeState {
    pub fn new() -> Self {
        let mut globals = HashMap::new();
        globals.insert("print".into(), Value::Function(lib_print));

        ExeState {
            globals,
            stack: Vec::new(),
            func_index: 0,
        }
    }
// ANCHOR_END: new

// ANCHOR: execute
    pub fn execute<R: Read>(&mut self, proto: &ParseProto<R>) {
        let mut pc = 0;
        while pc < proto.byte_codes.len() {
            println!("  [{pc}]\t{:?}", proto.byte_codes[pc]);
            match proto.byte_codes[pc] {
// ANCHOR: vm_global
                ByteCode::GetGlobal(dst, name) => {
                    let name: &str = (&proto.constants[name as usize]).into();
                    let v = self.globals.get(name).unwrap_or(&Value::Nil).clone();
                    self.set_stack(dst, v);
                }
                ByteCode::SetGlobal(name, src) => {
                    let name = &proto.constants[name as usize];
                    let value = self.stack[src as usize].clone();
                    self.globals.insert(name.into(), value);
                }
// ANCHOR_END: vm_global
                ByteCode::SetGlobalConst(name, src) => {
                    let name = &proto.constants[name as usize];
                    let value = proto.constants[src as usize].clone();
                    self.globals.insert(name.into(), value);
                }
                ByteCode::LoadConst(dst, c) => {
                    let v = proto.constants[c as usize].clone();
                    self.set_stack(dst, v);
                }
                ByteCode::LoadNil(dst, n) => {
                    self.fill_stack(dst as usize, n as usize);
                }
                ByteCode::LoadBool(dst, b) => {
                    self.set_stack(dst, Value::Boolean(b));
                }
                ByteCode::LoadInt(dst, i) => {
                    self.set_stack(dst, Value::Integer(i as i64));
                }
                ByteCode::Move(dst, src) => {
                    let v = self.stack[src as usize].clone();
                    self.set_stack(dst, v);
                }
// ANCHOR: vm_table
                ByteCode::NewTable(dst, narray, nmap) => {
                    let table = Table::new(narray as usize, nmap as usize);
                    self.set_stack(dst, Value::Table(Rc::new(RefCell::new(table))));
                }
                ByteCode::SetInt(t, i, v) => {
                    let value = self.stack[v as usize].clone();
                    self.set_table_int(t, i as i64, value);
                }
                ByteCode::SetIntConst(t, i, v) => {
                    let value = proto.constants[v as usize].clone();
                    self.set_table_int(t, i as i64, value);
                }
                ByteCode::SetField(t, k, v) => {
                    let key = proto.constants[k as usize].clone();
                    let value = self.stack[v as usize].clone();
                    self.set_table(t, key, value);
                }
                ByteCode::SetFieldConst(t, k, v) => {
                    let key = proto.constants[k as usize].clone();
                    let value = proto.constants[v as usize].clone();
                    self.set_table(t, key, value);
                }
                ByteCode::SetTable(t, k, v) => {
                    let key = self.stack[k as usize].clone();
                    let value = self.stack[v as usize].clone();
                    self.set_table(t, key, value);
                }
                ByteCode::SetTableConst(t, k, v) => {
                    let key = self.stack[k as usize].clone();
                    let value = proto.constants[v as usize].clone();
                    self.set_table(t, key, value);
                }
                ByteCode::SetList(table, n) => {
                    let ivalue = table as usize + 1;
                    if let Value::Table(table) = self.stack[table as usize].clone() {
                        let values = self.stack.drain(ivalue .. ivalue + n as usize);
                        table.borrow_mut().array.extend(values);
                    } else {
                        panic!("not table");
                    }
                }
                ByteCode::GetInt(dst, t, k) => {
                    let value = self.get_table_int(t, k as i64);
                    self.set_stack(dst, value);
                }
                ByteCode::GetField(dst, t, k) => {
                    let key = &proto.constants[k as usize];
                    let value = self.get_table(t, key);
                    self.set_stack(dst, value);
                }
                ByteCode::GetTable(dst, t, k) => {
                    let key = &self.stack[k as usize];
                    let value = self.get_table(t, key);
                    self.set_stack(dst, value);
                }
// ANCHOR_END: vm_table

                // condition structures
                ByteCode::Test(icond, jmp) => {
                    let cond = &self.stack[icond as usize];
                    if matches!(cond, Value::Nil | Value::Boolean(false)) {
                        pc = (pc as isize + jmp as isize) as usize; // jump if false
                    }
                }
                ByteCode::Jump(jmp) => {
                    pc = (pc as isize + jmp as isize) as usize;
                }

                // for-loop
// ANCHOR: for_prepare
                ByteCode::ForPrepare(dst, jmp) => {
                    // clear into 2 cases: integer and float
                    // stack: i, limit, step
                    if let (&Value::Integer(mut i), &Value::Integer(step)) =
                            (&self.stack[dst as usize], &self.stack[dst as usize + 2]) {
                        // integer case
                        if step == 0 {
                            panic!("0 step in numerical for");
                        }
                        let limit = match self.stack[dst as usize + 1] {
                            Value::Integer(limit) => limit,
                            Value::Float(limit) => {
                                let limit = for_int_limit(limit, step>0, &mut i);
                                self.set_stack(dst+1, Value::Integer(limit));
                                limit
                            }
                            // TODO convert string
                            _ => panic!("invalid limit type"),
                        };
                        if !for_check(i, limit, step>0) {
                            pc += jmp as usize;
                        }
                    } else {
                        // float case
                        let i = self.make_float(dst);
                        let limit = self.make_float(dst+1);
                        let step = self.make_float(dst+2);
                        if step == 0.0 {
                            panic!("0 step in numerical for");
                        }
                        if !for_check(i, limit, step>0.0) {
                            pc += jmp as usize;
                        }
                    }
                }
// ANCHOR_END: for_prepare
                ByteCode::ForLoop(dst, jmp) => {
                    // stack: i, limit, step
                    match self.stack[dst as usize] {
                        Value::Integer(i) => {
                            let limit = self.read_int(dst + 1);
                            let step = self.read_int(dst + 2);
                            let i = i + step;
                            if for_check(i, limit, step>0) {
                                self.set_stack(dst, Value::Integer(i));
                                pc -= jmp as usize;
                            }
                        }
                        Value::Float(f) => {
                            let limit = self.read_float(dst + 1);
                            let step = self.read_float(dst + 2);
                            let i = f + step;
                            if for_check(i, limit, step>0.0) {
                                self.set_stack(dst, Value::Float(i));
                                pc -= jmp as usize;
                            }
                        }
                        _ => panic!("xx"),
                    }
                }

                // function call
                ByteCode::Call(func, _) => {
                    self.func_index = func as usize;
                    let func = &self.stack[self.func_index];
                    if let Value::Function(f) = func {
                        f(self);
                    } else {
                        panic!("invalid function: {func:?}");
                    }
                }

                // unops
                ByteCode::Neg(dst, src) => {
                    let value = match &self.stack[src as usize] {
                        Value::Integer(i) => Value::Integer(-i),
                        Value::Float(f) => Value::Float(-f),
                        _ => panic!("invalid -"),
                    };
                    self.set_stack(dst, value);
                }
                ByteCode::Not(dst, src) => {
                    let value = match &self.stack[src as usize] {
                        Value::Nil => Value::Boolean(true),
                        Value::Boolean(b) => Value::Boolean(!b),
                        _ => Value::Boolean(false),
                    };
                    self.set_stack(dst, value);
                }
                ByteCode::BitNot(dst, src) => {
                    let value = match &self.stack[src as usize] {
                        Value::Integer(i) => Value::Integer(!i),
                        _ => panic!("invalid ~"),
                    };
                    self.set_stack(dst, value);
                }
                ByteCode::Len(dst, src) => {
                    let value = match &self.stack[src as usize] {
                        Value::ShortStr(len, _) => Value::Integer(*len as i64),
                        Value::MidStr(s) => Value::Integer(s.0 as i64),
                        Value::LongStr(s) => Value::Integer(s.len() as i64),
                        Value::Table(t) => Value::Integer(t.borrow().array.len() as i64),
                        _ => panic!("invalid -"),
                    };
                    self.set_stack(dst, value);
                }

                // binops
                ByteCode::Add(dst, a, b) => {
                    let r = exe_binop(&self.stack[a as usize], &self.stack[b as usize], |a,b|a+b, |a,b|a+b);
                    self.set_stack(dst, r);
                }
                ByteCode::AddConst(dst, a, b) => {
                    let r = exe_binop(&self.stack[a as usize], &proto.constants[b as usize], |a,b|a+b, |a,b|a+b);
                    self.set_stack(dst, r);
                }
                ByteCode::AddInt(dst, a, i) => {
                    let r = exe_binop_int(&self.stack[a as usize], i, |a,b|a+b, |a,b|a+b);
                    self.set_stack(dst, r);
                }
                ByteCode::Sub(dst, a, b) => {
                    let r = exe_binop(&self.stack[a as usize], &self.stack[b as usize], |a,b|a-b, |a,b|a-b);
                    self.set_stack(dst, r);
                }
                ByteCode::SubConst(dst, a, b) => {
                    let r = exe_binop(&self.stack[a as usize], &proto.constants[b as usize], |a,b|a-b, |a,b|a-b);
                    self.set_stack(dst, r);
                }
                ByteCode::SubInt(dst, a, i) => {
                    let r = exe_binop_int(&self.stack[a as usize], i, |a,b|a-b, |a,b|a-b);
                    self.set_stack(dst, r);
                }
                ByteCode::Mul(dst, a, b) => {
                    let r = exe_binop(&self.stack[a as usize], &self.stack[b as usize], |a,b|a*b, |a,b|a*b);
                    self.set_stack(dst, r);
                }
                ByteCode::MulConst(dst, a, b) => {
                    let r = exe_binop(&self.stack[a as usize], &proto.constants[b as usize], |a,b|a*b, |a,b|a*b);
                    self.set_stack(dst, r);
                }
                ByteCode::MulInt(dst, a, i) => {
                    let r = exe_binop_int(&self.stack[a as usize], i, |a,b|a*b, |a,b|a*b);
                    self.set_stack(dst, r);
                }
                ByteCode::Mod(dst, a, b) => {
                    let r = exe_binop(&self.stack[a as usize], &self.stack[b as usize], |a,b|a%b, |a,b|a%b);
                    self.set_stack(dst, r);
                }
                ByteCode::ModConst(dst, a, b) => {
                    let r = exe_binop(&self.stack[a as usize], &proto.constants[b as usize], |a,b|a%b, |a,b|a%b);
                    self.set_stack(dst, r);
                }
                ByteCode::ModInt(dst, a, i) => {
                    let r = exe_binop_int(&self.stack[a as usize], i, |a,b|a%b, |a,b|a%b);
                    self.set_stack(dst, r);
                }
                ByteCode::Idiv(dst, a, b) => {
                    let r = exe_binop(&self.stack[a as usize], &self.stack[b as usize], |a,b|a/b, |a,b|a/b);
                    self.set_stack(dst, r);
                }
                ByteCode::IdivConst(dst, a, b) => {
                    let r = exe_binop(&self.stack[a as usize], &proto.constants[b as usize], |a,b|a/b, |a,b|a/b);
                    self.set_stack(dst, r);
                }
                ByteCode::IdivInt(dst, a, i) => {
                    let r = exe_binop_int(&self.stack[a as usize], i, |a,b|a/b, |a,b|a/b);
                    self.set_stack(dst, r);
                }
                ByteCode::Div(dst, a, b) => {
                    let r = exe_binop_f(&self.stack[a as usize], &self.stack[b as usize], |a,b|a/b);
                    self.set_stack(dst, r);
                }
                ByteCode::DivConst(dst, a, b) => {
                    let r = exe_binop_f(&self.stack[a as usize], &proto.constants[b as usize], |a,b|a/b);
                    self.set_stack(dst, r);
                }
                ByteCode::DivInt(dst, a, i) => {
                    let r = exe_binop_int_f(&self.stack[a as usize], i, |a,b|a/b);
                    self.set_stack(dst, r);
                }
                ByteCode::Pow(dst, a, b) => {
                    let r = exe_binop_f(&self.stack[a as usize], &self.stack[b as usize], |a,b|a.powf(b));
                    self.set_stack(dst, r);
                }
                ByteCode::PowConst(dst, a, b) => {
                    let r = exe_binop_f(&self.stack[a as usize], &proto.constants[b as usize], |a,b|a.powf(b));
                    self.set_stack(dst, r);
                }
                ByteCode::PowInt(dst, a, i) => {
                    let r = exe_binop_int_f(&self.stack[a as usize], i, |a,b|a.powf(b));
                    self.set_stack(dst, r);
                }
                ByteCode::BitAnd(dst, a, b) => {
                    let r = exe_binop_i(&self.stack[a as usize], &self.stack[b as usize], |a,b|a&b);
                    self.set_stack(dst, r);
                }
                ByteCode::BitAndConst(dst, a, b) => {
                    let r = exe_binop_i(&self.stack[a as usize], &proto.constants[b as usize], |a,b|a&b);
                    self.set_stack(dst, r);
                }
                ByteCode::BitAndInt(dst, a, i) => {
                    let r = exe_binop_int_i(&self.stack[a as usize], i, |a,b|a&b);
                    self.set_stack(dst, r);
                }
                ByteCode::BitOr(dst, a, b) => {
                    let r = exe_binop_i(&self.stack[a as usize], &self.stack[b as usize], |a,b|a|b);
                    self.set_stack(dst, r);
                }
                ByteCode::BitOrConst(dst, a, b) => {
                    let r = exe_binop_i(&self.stack[a as usize], &proto.constants[b as usize], |a,b|a|b);
                    self.set_stack(dst, r);
                }
                ByteCode::BitOrInt(dst, a, i) => {
                    let r = exe_binop_int_i(&self.stack[a as usize], i, |a,b|a|b);
                    self.set_stack(dst, r);
                }
                ByteCode::BitXor(dst, a, b) => {
                    let r = exe_binop_i(&self.stack[a as usize], &self.stack[b as usize], |a,b|a^b);
                    self.set_stack(dst, r);
                }
                ByteCode::BitXorConst(dst, a, b) => {
                    let r = exe_binop_i(&self.stack[a as usize], &proto.constants[b as usize], |a,b|a^b);
                    self.set_stack(dst, r);
                }
                ByteCode::BitXorInt(dst, a, i) => {
                    let r = exe_binop_int_i(&self.stack[a as usize], i, |a,b|a^b);
                    self.set_stack(dst, r);
                }
                ByteCode::ShiftL(dst, a, b) => {
                    let r = exe_binop_i(&self.stack[a as usize], &self.stack[b as usize], |a,b|a<<b);
                    self.set_stack(dst, r);
                }
                ByteCode::ShiftLConst(dst, a, b) => {
                    let r = exe_binop_i(&self.stack[a as usize], &proto.constants[b as usize], |a,b|a<<b);
                    self.set_stack(dst, r);
                }
                ByteCode::ShiftLInt(dst, a, i) => {
                    let r = exe_binop_int_i(&self.stack[a as usize], i, |a,b|a<<b);
                    self.set_stack(dst, r);
                }
                ByteCode::ShiftR(dst, a, b) => {
                    let r = exe_binop_i(&self.stack[a as usize], &self.stack[b as usize], |a,b|a>>b);
                    self.set_stack(dst, r);
                }
                ByteCode::ShiftRConst(dst, a, b) => {
                    let r = exe_binop_i(&self.stack[a as usize], &proto.constants[b as usize], |a,b|a>>b);
                    self.set_stack(dst, r);
                }
                ByteCode::ShiftRInt(dst, a, i) => {
                    let r = exe_binop_int_i(&self.stack[a as usize], i, |a,b|a>>b);
                    self.set_stack(dst, r);
                }

                ByteCode::Concat(dst, a, b) => {
                    let r = exe_concat(&self.stack[a as usize], &self.stack[b as usize]);
                    self.set_stack(dst, r);
                }
                ByteCode::ConcatConst(dst, a, b) => {
                    let r = exe_concat(&self.stack[a as usize], &proto.constants[b as usize]);
                    self.set_stack(dst, r);
                }
                ByteCode::ConcatInt(dst, a, i) => {
                    let r = exe_concat(&self.stack[a as usize], &Value::Integer(i as i64));
                    self.set_stack(dst, r);
                }
            }

            pc += 1;
        }
    }
// ANCHOR_END: execute

// ANCHOR: set_stack
    fn set_stack(&mut self, dst: u8, v: Value) {
        let dst = dst as usize;
        match dst.cmp(&self.stack.len()) {
            Ordering::Equal => self.stack.push(v),
            Ordering::Less => self.stack[dst] = v,
            Ordering::Greater => panic!("fail in set_stack"),
        }
    }
// ANCHOR_END: set_stack
    fn fill_stack(&mut self, begin: usize, num: usize) {
        let end = begin + num;
        let len = self.stack.len();
        if begin < len {
            self.stack[begin .. len].fill(Value::Nil);
        }
        if end > len {
            self.stack.resize(end, Value::Nil);
        }
    }

    fn set_table(&mut self, t: u8, key: Value, value: Value) {
        match &key {
            Value::Integer(i) => self.set_table_int(t, *i, value), // TODO Float
            _ => self.do_set_table(t, key, value),
        }
    }
    fn set_table_int(&mut self, t: u8, i: i64, value: Value) {
        if let Value::Table(table) = &self.stack[t as usize] {
            let mut table = table.borrow_mut();
            // this is not same with Lua's official implement
            if i > 0 && (i < 4 || i < table.array.capacity() as i64 * 2) {
                set_vec(&mut table.array, i as usize - 1, value);
            } else {
                table.map.insert(Value::Integer(i), value);
            }
        } else {
            panic!("invalid table");
        }
    }
    fn do_set_table(&mut self, t: u8, key: Value, value: Value) {
        if let Value::Table(table) = &self.stack[t as usize] {
            table.borrow_mut().map.insert(key, value);
        } else {
            panic!("invalid table");
        }
    }

    fn get_table(&self, t: u8, key: &Value) -> Value {
        match key {
            Value::Integer(i) => self.get_table_int(t, *i), // TODO Float
            _ => self.do_get_table(t, key),
        }
    }
    fn get_table_int(&self, t: u8, i: i64) -> Value {
        if let Value::Table(table) = &self.stack[t as usize] {
            let table = table.borrow();
            table.array.get(i as usize - 1)
                .unwrap_or_else(|| table.map.get(&Value::Integer(i))
                    .unwrap_or(&Value::Nil)).clone()
        } else {
            panic!("set invalid table");
        }
    }
    fn do_get_table(&self, t: u8, key: &Value) -> Value {
        if let Value::Table(table) = &self.stack[t as usize] {
            let table = table.borrow();
            table.map.get(key).unwrap_or(&Value::Nil).clone()
        } else {
            panic!("set invalid table");
        }
    }

    fn make_float(&mut self, dst: u8) -> f64 {
        match self.stack[dst as usize] {
            Value::Float(f) => f,
            Value::Integer(i) => {
                let f = i as f64;
                self.set_stack(dst, Value::Float(f));
                f
            }
            // TODO convert string
            ref v => panic!("not number {v:?}"),
        }
    }
    fn read_int(&self, dst: u8) -> i64 {
        if let Value::Integer(i) = self.stack[dst as usize] {
            i
        } else {
            panic!("invalid integer");
        }
    }
    fn read_float(&self, dst: u8) -> f64 {
        if let Value::Float(f) = self.stack[dst as usize] {
            f
        } else {
            panic!("invalid integer");
        }
    }
}

fn set_vec(vec: &mut Vec<Value>, i: usize, value: Value) {
    match i.cmp(&vec.len()) {
        Ordering::Less => vec[i] = value,
        Ordering::Equal => vec.push(value),
        Ordering::Greater => {
            vec.resize(i, Value::Nil);
            vec.push(value);
        }
    }
}

fn exe_binop(v1: &Value, v2: &Value, arith_i: fn(i64,i64)->i64, arith_f: fn(f64,f64)->f64) -> Value {
    match (v1, v2) {
        (&Value::Integer(i1), &Value::Integer(i2)) => Value::Integer(arith_i(i1, i2)),
        (&Value::Integer(i1), &Value::Float(f2)) => Value::Float(arith_f(i1 as f64, f2)),
        (&Value::Float(f1), &Value::Float(f2)) => Value::Float(arith_f(f1, f2)),
        (&Value::Float(f1), &Value::Integer(i2)) => Value::Float(arith_f(f1, i2 as f64)),
        (_, _) => todo!("meta"),
    }
}
fn exe_binop_int(v1: &Value, i2: u8, arith_i: fn(i64,i64)->i64, arith_f: fn(f64,f64)->f64) -> Value {
    match v1 {
        &Value::Integer(i1) => Value::Integer(arith_i(i1, i2 as i64)),
        &Value::Float(f1) => Value::Float(arith_f(f1, i2 as f64)),
        _ => todo!("meta"),
    }
}

fn exe_binop_f(v1: &Value, v2: &Value, arith_f: fn(f64,f64)->f64) -> Value {
    let (f1, f2) = match (v1, v2) {
        (&Value::Integer(i1), &Value::Integer(i2)) => (i1 as f64, i2 as f64),
        (&Value::Integer(i1), &Value::Float(f2)) => (i1 as f64, f2),
        (&Value::Float(f1), &Value::Float(f2)) => (f1, f2),
        (&Value::Float(f1), &Value::Integer(i2)) => (f1, i2 as f64),
        (_, _) => todo!("meta"),
    };
    Value::Float(arith_f(f1, f2))
}
fn exe_binop_int_f(v1: &Value, i2: u8, arith_f: fn(f64,f64)->f64) -> Value {
    let f1 = match v1 {
        &Value::Integer(i1) => i1 as f64,
        &Value::Float(f1) => f1,
        _ => todo!("meta"),
    };
    Value::Float(arith_f(f1, i2 as f64))
}

fn exe_binop_i(v1: &Value, v2: &Value, arith_i: fn(i64,i64)->i64) -> Value {
    let (i1, i2) = match (v1, v2) {
        (&Value::Integer(i1), &Value::Integer(i2)) => (i1, i2),
        (&Value::Integer(i1), &Value::Float(f2)) => (i1, ftoi(f2).unwrap()),
        (&Value::Float(f1), &Value::Float(f2)) => (ftoi(f1).unwrap(), ftoi(f2).unwrap()),
        (&Value::Float(f1), &Value::Integer(i2)) => (ftoi(f1).unwrap(), i2),
        (_, _) => todo!("meta"),
    };
    Value::Integer(arith_i(i1, i2))
}
fn exe_binop_int_i(v1: &Value, i2: u8, arith_i: fn(i64,i64)->i64) -> Value {
    let i1 = match v1 {
        &Value::Integer(i1) => i1,
        &Value::Float(f1) => ftoi(f1).unwrap(),
        _ => todo!("meta"),
    };
    Value::Integer(arith_i(i1, i2 as i64))
}

fn exe_concat(v1: &Value, v2: &Value) -> Value {
    // TODO remove duplicated code
    let mut numbuf1: Vec<u8> = Vec::new();
    let v1 = match v1 {
        Value::Integer(i) => {
            write!(&mut numbuf1, "{}", i).unwrap();
            numbuf1.as_slice()
        }
        Value::Float(f) => {
            write!(&mut numbuf1, "{}", f).unwrap();
            numbuf1.as_slice()
        }
        _ => v1.into()
    };

    let mut numbuf2: Vec<u8> = Vec::new();
    let v2 = match v2 {
        Value::Integer(i) => {
            write!(&mut numbuf2, "{}", i).unwrap();
            numbuf2.as_slice()
        }
        Value::Float(f) => {
            write!(&mut numbuf2, "{}", f).unwrap();
            numbuf2.as_slice()
        }
        _ => v2.into()
    };

    [v1, v2].concat().into()
}

fn for_check<T: PartialOrd>(i: T, limit: T, is_step_positive: bool) -> bool {
    if is_step_positive {
        i <= limit
    } else {
        i >= limit
    }
}

fn for_int_limit(limit: f64, is_step_positive: bool, i: &mut i64) -> i64 {
    if is_step_positive {
        if limit < i64::MIN as f64 {
            // The limit is so negative that the for-loop should not run,
            // because any initial integer value is greater than such limit.
            // If we do not handle this case specially and return (limit as i64)
            // as normal, which will be converted into i64::MIN, and if the
            // initial integer is i64::MIN too, then the loop will run once,
            // which is wrong!
            // So we reset the initial integer to 0 and return limit as -1,
            // to make sure the loop must not be run.
            *i = 0;
            -1
        } else {
            limit.floor() as i64
        }
    } else {
        if limit > i64::MAX as f64 {
            *i = 0;
            1
        } else {
            limit.ceil() as i64
        }
    }
}
