use std::fs::File;
use crate::lex::{Lex, Token};
use crate::bytecode::ByteCode;
use crate::value::Value;

// ANCHOR: proto
#[derive(Debug)]
pub struct ParseProto {
    pub constants: Vec::<Value>,
    pub byte_codes: Vec::<ByteCode>,
}
// ANCHOR_END: proto

// ANCHOR: load
pub fn load(input: File) -> ParseProto {
    let mut constants = Vec::new();
    let mut byte_codes = Vec::new();
    let mut lex = Lex::new(input);

    loop {
        match lex.next() {
            Token::Name(name) => { // `Name LiteralString` as function call
                // function, global variable only
                let ic = add_const(&mut constants, Value::String(name));
                byte_codes.push(ByteCode::GetGlobal(0, ic as u8));

                // argument, (var) or "string"
                match lex.next() {
                    Token::ParL => { // '('
                        let code = match lex.next() {
                            Token::Nil => ByteCode::LoadNil(1),
                            Token::True => ByteCode::LoadBool(1, true),
                            Token::False => ByteCode::LoadBool(1, false),
                            Token::Integer(i) =>
                                if let Ok(ii) = i16::try_from(i) {
                                    ByteCode::LoadInt(1, ii)
                                } else {
                                    load_const(&mut constants, 1, Value::Integer(i))
                                }
                            Token::Float(f) => load_const(&mut constants, 1, Value::Float(f)),
                            Token::String(s) => load_const(&mut constants, 1, Value::String(s)),
                            _ => panic!("invalid argument"),
                        };
                        byte_codes.push(code);

                        if lex.next() != Token::ParR { // ')'
                            panic!("expected `)`");
                        }
                    }
                    Token::String(s) => {
                        let code = load_const(&mut constants, 1, Value::String(s));
                        byte_codes.push(code);
                    }
                    _ => panic!("expected string"),
                }

                byte_codes.push(ByteCode::Call(0, 1));
            }
            Token::Eos => break,
            t => panic!("unexpected token: {t:?}"),
        }
    }

    dbg!(&constants);
    println!("byte_codes:");
    for c in byte_codes.iter() {
        println!("  {:?}", c);
    }
    ParseProto { constants, byte_codes }
}
// ANCHOR_END: load

fn add_const(constants: &mut Vec<Value>, c: Value) -> usize {
    constants.iter().rposition(|v| v == &c)
        .unwrap_or_else(|| {
            constants.push(c);
            constants.len() - 1
        })
}

fn load_const(constants: &mut Vec<Value>, dst: usize, c: Value) -> ByteCode {
    ByteCode::LoadConst(dst as u8, add_const(constants, c) as u8)
}
