use std::cmp::Ordering;
use std::collections::HashMap;
use crate::bytecode::ByteCode;
use crate::value::Value;
use crate::parse::ParseProto;

// ANCHOR: print
// "print" function in Lua's std-lib.
// It supports only 1 argument and assumes the argument is at index:1 on stack.
fn lib_print(state: &mut ExeState) -> i32 {
    println!("{:?}", state.stack[1]);
    0
}
// ANCHOR_END: print

// ANCHOR: state
pub struct ExeState {
    globals: HashMap<String, Value>,
    stack: Vec::<Value>,
}
// ANCHOR_END: state

// ANCHOR: new
impl ExeState {
    pub fn new() -> Self {
        let mut globals = HashMap::new();
        globals.insert(String::from("print"), Value::Function(lib_print));

        ExeState {
            globals,
            stack: Vec::new(),
        }
    }
// ANCHOR_END: new

// ANCHOR: execute
    pub fn execute(&mut self, proto: &ParseProto) {
        for code in proto.byte_codes.iter() {
            match *code {
                ByteCode::GetGlobal(dst, name) => {
                    let name = &proto.constants[name as usize];
                    if let Value::String(key) = name {
                        let v = self.globals.get(key).unwrap_or(&Value::Nil).clone();
                        self.set_stack(dst, v);
                    } else {
                        panic!("invalid global key: {name:?}");
                    }
                }
                ByteCode::LoadConst(dst, c) => {
                    let v = proto.constants[c as usize].clone();
                    self.set_stack(dst, v);
                }
                ByteCode::Call(func, _) => {
                    let func = &self.stack[func as usize];
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
