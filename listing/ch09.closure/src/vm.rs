use std::io::Write;
use std::rc::Rc;
use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::HashMap;
use crate::bytecode::ByteCode;
use crate::value::{Value, Table};
use crate::parse::FuncProto;
use crate::utils::ftoi;

// ANCHOR: print
// "print" function in Lua's std-lib.
fn lib_print(state: &mut ExeState) -> i32 {
    for i in 1 ..= state.get_top() {
        if i != 1 {
            print!("\t");
        }
        print!("{}", state.get::<&Value>(i).to_string());
    }
    println!("");
    0
}
fn lib_type(state: &mut ExeState) -> i32 {
    let ty = state.get::<&Value>(1).ty();
    state.push(ty);
    1
}
// ANCHOR_END: print

// ANCHOR: state
pub struct ExeState {
    globals: HashMap<String, Value>,
    stack: Vec::<Value>,
    base: usize, // stack base of current function
}
// ANCHOR_END: state

// ANCHOR: new
impl ExeState {
    pub fn new() -> Self {
        let mut globals = HashMap::new();
        globals.insert("print".into(), Value::RustFunction(lib_print));
        globals.insert("type".into(), Value::RustFunction(lib_type));

        ExeState {
            globals,
            stack: Vec::new(),
            base: 1, // for entry function
        }
    }
// ANCHOR_END: new

// ANCHOR: execute
    pub fn execute(&mut self, proto: &FuncProto) -> usize {
        let varargs = if proto.has_varargs {
            self.stack.drain(self.base + proto.nparam ..).collect()
        } else {
            Vec::new()
        };

        let mut pc = 0;
        loop {
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
                    let value = self.get_stack(src).clone();
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
                    let v = self.get_stack(src).clone();
                    self.set_stack(dst, v);
                }
// ANCHOR: vm_table
                ByteCode::NewTable(dst, narray, nmap) => {
                    let table = Table::new(narray as usize, nmap as usize);
                    self.set_stack(dst, Value::Table(Rc::new(RefCell::new(table))));
                }
                ByteCode::SetInt(t, i, v) => {
                    let value = self.get_stack(v).clone();
                    self.set_table_int(t, i as i64, value);
                }
                ByteCode::SetIntConst(t, i, v) => {
                    let value = proto.constants[v as usize].clone();
                    self.set_table_int(t, i as i64, value);
                }
                ByteCode::SetField(t, k, v) => {
                    let key = proto.constants[k as usize].clone();
                    let value = self.get_stack(v).clone();
                    self.set_table(t, key, value);
                }
                ByteCode::SetFieldConst(t, k, v) => {
                    let key = proto.constants[k as usize].clone();
                    let value = proto.constants[v as usize].clone();
                    self.set_table(t, key, value);
                }
                ByteCode::SetTable(t, k, v) => {
                    let key = self.get_stack(k).clone();
                    let value = self.get_stack(v).clone();
                    self.set_table(t, key, value);
                }
                ByteCode::SetTableConst(t, k, v) => {
                    let key = self.get_stack(k).clone();
                    let value = proto.constants[v as usize].clone();
                    self.set_table(t, key, value);
                }
                ByteCode::SetList(table, n) => {
                    let ivalue = self.base + table as usize + 1;
                    if let Value::Table(table) = self.get_stack(table).clone() {
                        let end = if n == 0 {
                            // 0 is special, means all following values in stack
                            self.stack.len()
                        } else {
                            ivalue + n as usize
                        };
                        let values = self.stack.drain(ivalue .. end);
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
                ByteCode::GetFieldSelf(dst, t, k) => {
                    let table = self.get_stack(t).clone();
                    let key = &proto.constants[k as usize];
                    let value = self.get_table(t, key);
                    self.set_stack(dst, value);
                    self.set_stack(dst+1, table);
                }
                ByteCode::GetTable(dst, t, k) => {
                    let key = self.get_stack(k);
                    let value = self.get_table(t, key);
                    self.set_stack(dst, value);
                }
// ANCHOR_END: vm_table

                // condition structures
                ByteCode::TestAndJump(icondition, jmp) => {
                    if self.get_stack(icondition).into() { // jump if true
                        pc = (pc as isize + jmp as isize) as usize;
                    }
                }
                ByteCode::TestOrJump(icondition, jmp) => {
                    if self.get_stack(icondition).into() {} else { // jump if false
                        pc = (pc as isize + jmp as isize) as usize;
                    }
                }
                ByteCode::TestAndSetJump(dst, icondition, jmp) => {
                    let condition = self.get_stack(icondition);
                    if condition.into() { // set and jump if true
                        self.set_stack(dst, condition.clone());
                        pc += jmp as usize;
                    }
                }
                ByteCode::TestOrSetJump(dst, icondition, jmp) => {
                    let condition = self.get_stack(icondition);
                    if condition.into() {} else { // set and jump if false
                        self.set_stack(dst, condition.clone());
                        pc += jmp as usize;
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
                            (self.get_stack(dst), self.get_stack(dst + 2)) {
                        // integer case
                        if step == 0 {
                            panic!("0 step in numerical for");
                        }
                        let limit = match self.get_stack(dst + 1) {
                            &Value::Integer(limit) => limit,
                            &Value::Float(limit) => {
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
                    match self.get_stack(dst) {
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
                ByteCode::Call(func, narg_plus, want_nret) => {
                    let nret = self.call_function(func, narg_plus);

                    // move return values to @func
                    let iret = self.stack.len() - nret;
                    self.stack.drain(self.base+func as usize .. iret);

                    // want_nret==0 means 1.want all return values or 2.want no
                    // return values, while we do not need handle in both cases;
                    // otherwise, means @want_nret return values are need, and
                    // we need to fill nil if necessary.
                    let want_nret = want_nret as usize;
                    if nret < want_nret {
                        self.fill_stack(nret, want_nret - nret);
                    }
                }
                ByteCode::CallSet(dst, func, narg_plus) => {
                    let nret = self.call_function(func, narg_plus);

                    // set first return value to @dst directly
                    if nret == 0 {
                        self.set_stack(dst, Value::Nil);
                    } else {
                        // use swap() to avoid clone()
                        let iret = self.stack.len() - nret;
                        self.stack.swap(self.base+dst as usize, iret);
                    }
                    self.stack.truncate(self.base + func as usize + 1);
                }

                ByteCode::TailCall(func, narg_plus) => {
                    // clear current call-frame, and move new function entry and
                    // arguments (self.stack[@func ..]) into current call-frame
                    self.stack.drain(self.base-1 .. self.base+func as usize);

                    return self.do_call_function(narg_plus);
                }

                ByteCode::Return(iret, nret) => {
                    // if nret==0, return stack[iret .. ];
                    // otherwise, return stack[iret .. iret+nret] and truncate
                    // the stack to make sure there is no more extra temprary
                    // values, so:
                    // - we can return @nret only (but no need @iret) to
                    //   indicate the return values, so we get the same
                    //   return type with RustFunction;
                    // - the following byte code, including Return(_,0),
                    //   Call(_,_,0) or SetList(_,0), can get the
                    //   #return-values by stack top.
                    let iret = self.base + iret as usize;
                    if nret == 0 {
                        return self.stack.len() - iret;
                    } else {
                        self.stack.truncate(iret + nret as usize);
                        return nret as usize;
                    }
                }
                ByteCode::Return0 => {
                    return 0;
                }

                ByteCode::VarArgs(dst, want) => {
                    // truncate the stack to make sure there is no more
                    // extra temprary values, so the following byte code,
                    // including Return(_,0), Call(_,_,0) or SetList(_,0),
                    // can get the #varargs by stack top.
                    self.stack.truncate(self.base + dst as usize);

                    let len = varargs.len();
                    let want = want as usize;
                    if want == 0 { // 0 means all
                        self.stack.extend_from_slice(&varargs);
                    } else if want > len {
                        self.stack.extend_from_slice(&varargs);
                        self.fill_stack(dst as usize + len, want - len);
                    } else {
                        self.stack.extend_from_slice(&varargs[..want]);
                    }
                }

                // unops
                ByteCode::Neg(dst, src) => {
                    let value = match &self.get_stack(src) {
                        Value::Integer(i) => Value::Integer(-i),
                        Value::Float(f) => Value::Float(-f),
                        _ => panic!("invalid -"),
                    };
                    self.set_stack(dst, value);
                }
                ByteCode::Not(dst, src) => {
                    let value = match &self.get_stack(src) {
                        Value::Nil => Value::Boolean(true),
                        Value::Boolean(b) => Value::Boolean(!b),
                        _ => Value::Boolean(false),
                    };
                    self.set_stack(dst, value);
                }
                ByteCode::BitNot(dst, src) => {
                    let value = match &self.get_stack(src) {
                        Value::Integer(i) => Value::Integer(!i),
                        _ => panic!("invalid ~"),
                    };
                    self.set_stack(dst, value);
                }
                ByteCode::Len(dst, src) => {
                    let value = match &self.get_stack(src) {
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
                    let r = exe_binop(&self.get_stack(a), &self.get_stack(b), |a,b|a+b, |a,b|a+b);
                    self.set_stack(dst, r);
                }
                ByteCode::AddConst(dst, a, b) => {
                    let r = exe_binop(&self.get_stack(a), &proto.constants[b as usize], |a,b|a+b, |a,b|a+b);
                    self.set_stack(dst, r);
                }
                ByteCode::AddInt(dst, a, i) => {
                    let r = exe_binop_int(&self.get_stack(a), i, |a,b|a+b, |a,b|a+b);
                    self.set_stack(dst, r);
                }
                ByteCode::Sub(dst, a, b) => {
                    let r = exe_binop(&self.get_stack(a), &self.get_stack(b), |a,b|a-b, |a,b|a-b);
                    self.set_stack(dst, r);
                }
                ByteCode::SubConst(dst, a, b) => {
                    let r = exe_binop(&self.get_stack(a), &proto.constants[b as usize], |a,b|a-b, |a,b|a-b);
                    self.set_stack(dst, r);
                }
                ByteCode::SubInt(dst, a, i) => {
                    let r = exe_binop_int(&self.get_stack(a), i, |a,b|a-b, |a,b|a-b);
                    self.set_stack(dst, r);
                }
                ByteCode::Mul(dst, a, b) => {
                    let r = exe_binop(&self.get_stack(a), &self.get_stack(b), |a,b|a*b, |a,b|a*b);
                    self.set_stack(dst, r);
                }
                ByteCode::MulConst(dst, a, b) => {
                    let r = exe_binop(&self.get_stack(a), &proto.constants[b as usize], |a,b|a*b, |a,b|a*b);
                    self.set_stack(dst, r);
                }
                ByteCode::MulInt(dst, a, i) => {
                    let r = exe_binop_int(&self.get_stack(a), i, |a,b|a*b, |a,b|a*b);
                    self.set_stack(dst, r);
                }
                ByteCode::Mod(dst, a, b) => {
                    let r = exe_binop(&self.get_stack(a), &self.get_stack(b), |a,b|a%b, |a,b|a%b);
                    self.set_stack(dst, r);
                }
                ByteCode::ModConst(dst, a, b) => {
                    let r = exe_binop(&self.get_stack(a), &proto.constants[b as usize], |a,b|a%b, |a,b|a%b);
                    self.set_stack(dst, r);
                }
                ByteCode::ModInt(dst, a, i) => {
                    let r = exe_binop_int(&self.get_stack(a), i, |a,b|a%b, |a,b|a%b);
                    self.set_stack(dst, r);
                }
                ByteCode::Idiv(dst, a, b) => {
                    let r = exe_binop(&self.get_stack(a), &self.get_stack(b), |a,b|a/b, |a,b|a/b);
                    self.set_stack(dst, r);
                }
                ByteCode::IdivConst(dst, a, b) => {
                    let r = exe_binop(&self.get_stack(a), &proto.constants[b as usize], |a,b|a/b, |a,b|a/b);
                    self.set_stack(dst, r);
                }
                ByteCode::IdivInt(dst, a, i) => {
                    let r = exe_binop_int(&self.get_stack(a), i, |a,b|a/b, |a,b|a/b);
                    self.set_stack(dst, r);
                }
                ByteCode::Div(dst, a, b) => {
                    let r = exe_binop_f(&self.get_stack(a), &self.get_stack(b), |a,b|a/b);
                    self.set_stack(dst, r);
                }
                ByteCode::DivConst(dst, a, b) => {
                    let r = exe_binop_f(&self.get_stack(a), &proto.constants[b as usize], |a,b|a/b);
                    self.set_stack(dst, r);
                }
                ByteCode::DivInt(dst, a, i) => {
                    let r = exe_binop_int_f(&self.get_stack(a), i, |a,b|a/b);
                    self.set_stack(dst, r);
                }
                ByteCode::Pow(dst, a, b) => {
                    let r = exe_binop_f(&self.get_stack(a), &self.get_stack(b), |a,b|a.powf(b));
                    self.set_stack(dst, r);
                }
                ByteCode::PowConst(dst, a, b) => {
                    let r = exe_binop_f(&self.get_stack(a), &proto.constants[b as usize], |a,b|a.powf(b));
                    self.set_stack(dst, r);
                }
                ByteCode::PowInt(dst, a, i) => {
                    let r = exe_binop_int_f(&self.get_stack(a), i, |a,b|a.powf(b));
                    self.set_stack(dst, r);
                }
                ByteCode::BitAnd(dst, a, b) => {
                    let r = exe_binop_i(&self.get_stack(a), &self.get_stack(b), |a,b|a&b);
                    self.set_stack(dst, r);
                }
                ByteCode::BitAndConst(dst, a, b) => {
                    let r = exe_binop_i(&self.get_stack(a), &proto.constants[b as usize], |a,b|a&b);
                    self.set_stack(dst, r);
                }
                ByteCode::BitAndInt(dst, a, i) => {
                    let r = exe_binop_int_i(&self.get_stack(a), i, |a,b|a&b);
                    self.set_stack(dst, r);
                }
                ByteCode::BitOr(dst, a, b) => {
                    let r = exe_binop_i(&self.get_stack(a), &self.get_stack(b), |a,b|a|b);
                    self.set_stack(dst, r);
                }
                ByteCode::BitOrConst(dst, a, b) => {
                    let r = exe_binop_i(&self.get_stack(a), &proto.constants[b as usize], |a,b|a|b);
                    self.set_stack(dst, r);
                }
                ByteCode::BitOrInt(dst, a, i) => {
                    let r = exe_binop_int_i(&self.get_stack(a), i, |a,b|a|b);
                    self.set_stack(dst, r);
                }
                ByteCode::BitXor(dst, a, b) => {
                    let r = exe_binop_i(&self.get_stack(a), &self.get_stack(b), |a,b|a^b);
                    self.set_stack(dst, r);
                }
                ByteCode::BitXorConst(dst, a, b) => {
                    let r = exe_binop_i(&self.get_stack(a), &proto.constants[b as usize], |a,b|a^b);
                    self.set_stack(dst, r);
                }
                ByteCode::BitXorInt(dst, a, i) => {
                    let r = exe_binop_int_i(&self.get_stack(a), i, |a,b|a^b);
                    self.set_stack(dst, r);
                }
                ByteCode::ShiftL(dst, a, b) => {
                    let r = exe_binop_i(&self.get_stack(a), &self.get_stack(b), |a,b|a<<b);
                    self.set_stack(dst, r);
                }
                ByteCode::ShiftLConst(dst, a, b) => {
                    let r = exe_binop_i(&self.get_stack(a), &proto.constants[b as usize], |a,b|a<<b);
                    self.set_stack(dst, r);
                }
                ByteCode::ShiftLInt(dst, a, i) => {
                    let r = exe_binop_int_i(&self.get_stack(a), i, |a,b|a<<b);
                    self.set_stack(dst, r);
                }
                ByteCode::ShiftR(dst, a, b) => {
                    let r = exe_binop_i(&self.get_stack(a), &self.get_stack(b), |a,b|a>>b);
                    self.set_stack(dst, r);
                }
                ByteCode::ShiftRConst(dst, a, b) => {
                    let r = exe_binop_i(&self.get_stack(a), &proto.constants[b as usize], |a,b|a>>b);
                    self.set_stack(dst, r);
                }
                ByteCode::ShiftRInt(dst, a, i) => {
                    let r = exe_binop_int_i(&self.get_stack(a), i, |a,b|a>>b);
                    self.set_stack(dst, r);
                }

                ByteCode::Equal(a, b, r) => {
                    if (self.get_stack(a) == self.get_stack(b)) == r {
                        pc += 1;
                    }
                }
                ByteCode::EqualConst(a, b, r) => {
                    if (self.get_stack(a) == &proto.constants[b as usize]) == r {
                        pc += 1;
                    }
                }
                ByteCode::EqualInt(a, i, r) => {
                    if let &Value::Integer(ii) = self.get_stack(a) {
                        if (ii == i as i64) == r {
                            pc += 1;
                        }
                    }
                }
                ByteCode::NotEq(a, b, r) => {
                    if (self.get_stack(a) != self.get_stack(b)) == r {
                        pc += 1;
                    }
                }
                ByteCode::NotEqConst(a, b, r) => {
                    if (self.get_stack(a) != &proto.constants[b as usize]) == r {
                        pc += 1;
                    }
                }
                ByteCode::NotEqInt(a, i, r) => {
                    if let &Value::Integer(ii) = self.get_stack(a) {
                        if (ii != i as i64) == r {
                            pc += 1;
                        }
                    }
                }
                ByteCode::LesEq(a, b, r) => {
                    let cmp = self.get_stack(a).partial_cmp(self.get_stack(b)).unwrap();
                    if !matches!(cmp, Ordering::Greater) == r {
                        pc += 1;
                    }
                }
                ByteCode::LesEqConst(a, b, r) => {
                    let cmp = self.get_stack(a).partial_cmp(&proto.constants[b as usize]).unwrap();
                    if !matches!(cmp, Ordering::Greater) == r {
                        pc += 1;
                    }
                }
                ByteCode::LesEqInt(a, i, r) => {
                    let a = match self.get_stack(a) {
                        &Value::Integer(i) => i,
                        &Value::Float(f) => f as i64,
                        _ => panic!("invalid compare"),
                    };
                    if (a <= i as i64) == r {
                        pc += 1;
                    }
                }
                ByteCode::GreEq(a, b, r) => {
                    let cmp = self.get_stack(a).partial_cmp(self.get_stack(b)).unwrap();
                    if !matches!(cmp, Ordering::Less) == r {
                        pc += 1;
                    }
                }
                ByteCode::GreEqConst(a, b, r) => {
                    let cmp = self.get_stack(a).partial_cmp(&proto.constants[b as usize]).unwrap();
                    if !matches!(cmp, Ordering::Less) == r {
                        pc += 1;
                    }
                }
                ByteCode::GreEqInt(a, i, r) => {
                    let a = match self.get_stack(a) {
                        &Value::Integer(i) => i,
                        &Value::Float(f) => f as i64,
                        _ => panic!("invalid compare"),
                    };
                    if (a >= i as i64) == r {
                        pc += 1;
                    }
                }
                ByteCode::Less(a, b, r) => {
                    let cmp = self.get_stack(a).partial_cmp(self.get_stack(b)).unwrap();
                    if matches!(cmp, Ordering::Less) == r {
                        pc += 1;
                    }
                }
                ByteCode::LessConst(a, b, r) => {
                    let cmp = self.get_stack(a).partial_cmp(&proto.constants[b as usize]).unwrap();
                    if matches!(cmp, Ordering::Less) == r {
                        pc += 1;
                    }
                }
                ByteCode::LessInt(a, i, r) => {
                    let a = match self.get_stack(a) {
                        &Value::Integer(i) => i,
                        &Value::Float(f) => f as i64,
                        _ => panic!("invalid compare"),
                    };
                    if (a < i as i64) == r {
                        pc += 1;
                    }
                }
                ByteCode::Greater(a, b, r) => {
                    let cmp = self.get_stack(a).partial_cmp(self.get_stack(b)).unwrap();
                    if matches!(cmp, Ordering::Greater) == r {
                        pc += 1;
                    }
                }
                ByteCode::GreaterConst(a, b, r) => {
                    let cmp = self.get_stack(a).partial_cmp(&proto.constants[b as usize]).unwrap();
                    if matches!(cmp, Ordering::Greater) == r {
                        pc += 1;
                    }
                }
                ByteCode::GreaterInt(a, i, r) => {
                    let a = match self.get_stack(a) {
                        &Value::Integer(i) => i,
                        &Value::Float(f) => f as i64,
                        _ => panic!("invalid compare"),
                    };
                    if (a > i as i64) == r {
                        pc += 1;
                    }
                }

                ByteCode::SetFalseSkip(dst) => {
                    self.set_stack(dst, Value::Boolean(false));
                    pc += 1;
                }

                ByteCode::Concat(dst, a, b) => {
                    let r = exe_concat(&self.get_stack(a), &self.get_stack(b));
                    self.set_stack(dst, r);
                }
                ByteCode::ConcatConst(dst, a, b) => {
                    let r = exe_concat(&self.get_stack(a), &proto.constants[b as usize]);
                    self.set_stack(dst, r);
                }
                ByteCode::ConcatInt(dst, a, i) => {
                    let r = exe_concat(&self.get_stack(a), &Value::Integer(i as i64));
                    self.set_stack(dst, r);
                }
            }

            pc += 1;
        }
    }
// ANCHOR_END: execute

    fn get_stack(&self, dst: u8) -> &Value {
        &self.stack[self.base + dst as usize]
    }
// ANCHOR: set_stack
    fn set_stack(&mut self, dst: u8, v: Value) {
        set_vec(&mut self.stack, self.base + dst as usize, v);
    }
// ANCHOR_END: set_stack
    fn fill_stack(&mut self, begin: usize, num: usize) {
        let begin = self.base + begin;
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
        if let Value::Table(table) = &self.get_stack(t) {
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
        if let Value::Table(table) = &self.get_stack(t) {
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
        if let Value::Table(table) = self.get_stack(t) {
            let table = table.borrow();
            table.array.get(i as usize - 1)
                .unwrap_or_else(|| table.map.get(&Value::Integer(i))
                    .unwrap_or(&Value::Nil)).clone()
        } else {
            panic!("get invalid table: {:?}", t);
        }
    }
    fn do_get_table(&self, t: u8, key: &Value) -> Value {
        if let Value::Table(table) = &self.get_stack(t) {
            let table = table.borrow();
            table.map.get(key).unwrap_or(&Value::Nil).clone()
        } else {
            panic!("set invalid table");
        }
    }

    // call function
    // return the number of return values which are at the stack end
    fn call_function(&mut self, func: u8, narg_plus: u8) -> usize {
        self.base += func as usize + 1; // get into new world
        let nret = self.do_call_function(narg_plus);
        self.base -= func as usize + 1; // come back
        nret
    }

    // Before calling, the function entry is at @self.base-1, and the
    // arguments follows with:
    // - narg_plus==0 means variable arguments, and all stack values
    //   following the function entry are arguments;
    // - otherwise means (narg_plus-1) fixed arguments, and there may
    //   be temprary values following which need be truncated sometime.
    //
    // After calling, the return values lay at the top of stack.
    //
    // Return the number of return values.
    fn do_call_function(&mut self, narg_plus: u8) -> usize {
        match self.stack[self.base - 1].clone() {
            Value::RustFunction(f) => {
                // drop potential temprary stack usage, for get_top()
                if narg_plus != 0 {
                    self.stack.truncate(self.base + narg_plus as usize - 1);
                }

                f(self) as usize
            }
            Value::LuaFunction(f) => {
                let narg = if narg_plus == 0 {
                    self.stack.len() - self.base
                } else {
                    narg_plus as usize - 1
                };

                if narg < f.nparam {
                    // fill nil to meet f.nparam
                    self.fill_stack(narg, f.nparam - narg);
                } else if f.has_varargs && narg_plus != 0 {
                    // drop potential temprary stack usage for VarArgs
                    self.stack.truncate(self.base + narg);
                }

                self.execute(&f)
            }
            v => panic!("invalid function: {v:?}"),
        }
    }

    fn make_float(&mut self, dst: u8) -> f64 {
        match self.get_stack(dst) {
            &Value::Float(f) => f,
            &Value::Integer(i) => {
                let f = i as f64;
                self.set_stack(dst, Value::Float(f));
                f
            }
            // TODO convert string
            ref v => panic!("not number {v:?}"),
        }
    }
    fn read_int(&self, dst: u8) -> i64 {
        if let &Value::Integer(i) = self.get_stack(dst) {
            i
        } else {
            panic!("invalid integer");
        }
    }
    fn read_float(&self, dst: u8) -> f64 {
        if let &Value::Float(f) = self.get_stack(dst) {
            f
        } else {
            panic!("invalid integer");
        }
    }
}

// API
impl<'a> ExeState {
    pub fn get_top(&self) -> usize {
        self.stack.len() - self.base
    }
    pub fn get<T>(&'a self, i: usize) -> T where T: From<&'a Value> {
        (&self.stack[self.base + i - 1]).into()
    }
    pub fn push(&mut self, v: impl Into<Value>) {
        self.stack.push(v.into());
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
