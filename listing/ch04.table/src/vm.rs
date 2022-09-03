use std::io::Read;
use std::cmp::Ordering;
use std::collections::HashMap;
use crate::bytecode::ByteCode;
use crate::value::Value;
use crate::parse::ParseProto;

// ANCHOR: print
// "print" function in Lua's std-lib.
// It supports only 1 argument and assumes the argument is at index:1 on stack.
fn lib_print(state: &mut ExeState) -> i32 {
    println!("{:?}", state.stack[state.func_index + 1]);
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
        for code in proto.byte_codes.iter() {
            match *code {
// ANCHOR: vm_global
                ByteCode::GetGlobal(dst, name) => {
                    let name: &str = (&proto.constants[name as usize]).into();
                    let v = self.globals.get(name).unwrap_or(&Value::Nil).clone();
                    self.set_stack(dst.into(), v);
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
                ByteCode::SetGlobalGlobal(name, src) => {
                    let name = &proto.constants[name as usize];
                    let src: &str = (&proto.constants[src as usize]).into();
                    let value = self.globals.get(src).unwrap_or(&Value::Nil).clone();
                    self.globals.insert(name.into(), value);
                }
                ByteCode::LoadConst(dst, c) => {
                    let v = proto.constants[c as usize].clone();
                    self.set_stack(dst, v);
                }
                ByteCode::LoadNil(dst) => {
                    self.set_stack(dst, Value::Nil);
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
                ByteCode::Call(func, _) => {
                    self.func_index = func as usize;
                    let func = &self.stack[self.func_index];
                    if let Value::Function(f) = func {
                        f(self);
                    } else {
                        panic!("invalid function: {func:?}");
                    }
                }
            }
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
}
