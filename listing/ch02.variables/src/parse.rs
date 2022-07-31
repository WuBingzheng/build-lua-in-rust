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
    let mut locals = Vec::new();
    let mut lex = Lex::new(input);

    loop {
        match lex.next() {
            Token::Name(name) => { // `Name LiteralString` as function call
                let ifunc = locals.len();
                let iarg = ifunc + 1;

                // function, variable
                byte_codes.push(load_var(&mut constants, &mut locals, ifunc, name));

                // argument, (var) or "string"
                match lex.next() {
                    Token::ParL => { // '('
                        load_exp(&mut byte_codes, &mut constants, &mut locals, lex.next(), iarg);

                        if lex.next() != Token::ParR { // ')'
                            panic!("expected `)`");
                        }
                    }
                    Token::String(s) => {
                        let code = load_const(&mut constants, iarg, Value::String(s));
                        byte_codes.push(code);
                    }
                    _ => panic!("expected string"),
                }

                byte_codes.push(ByteCode::Call(ifunc as u8, 1));
            }
// ANCHOR: parse_local
            Token::Local => { // local name = exp
                let var = if let Token::Name(var) = lex.next() {
                    var
                } else {
                    panic!("expected variable");
                };

                if lex.next() != Token::Assign {
                    panic!("expected `=`");
                }

                load_exp(&mut byte_codes, &mut constants, &locals, lex.next(), locals.len());

                // add to locals after load_exp()
                locals.push(var);
            }
// ANCHOR_END: parse_local
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
    constants.iter().position(|v| v == &c)
        .unwrap_or_else(|| {
            constants.push(c);
            constants.len() - 1
        })
}

fn load_const(constants: &mut Vec<Value>, dst: usize, c: Value) -> ByteCode {
    ByteCode::LoadConst(dst as u8, add_const(constants, c) as u8)
}

fn load_var(constants: &mut Vec<Value>, locals: &Vec<String>, dst: usize, name: String) -> ByteCode {
    if let Some(i) = locals.iter().rposition(|v| v == &name) {
        // local variable
        ByteCode::Move(dst as u8, i as u8)
    } else {
        // global variable
        let ic = add_const(constants, Value::String(name));
        ByteCode::GetGlobal(dst as u8, ic as u8)
    }
}

fn load_exp(byte_codes: &mut Vec<ByteCode>, constants: &mut Vec<Value>,
        locals: &Vec<String>, token: Token, dst: usize) {

    let code = match token {
        Token::Nil => ByteCode::LoadNil(dst as u8),
        Token::True => ByteCode::LoadBool(dst as u8, true),
        Token::False => ByteCode::LoadBool(dst as u8, false),
        Token::Integer(i) =>
            if let Ok(ii) = i16::try_from(i) {
                ByteCode::LoadInt(dst as u8, ii)
            } else {
                load_const(constants, dst, Value::Integer(i))
            }
        Token::Float(f) => load_const(constants, dst, Value::Float(f)),
        Token::String(s) => load_const(constants, dst, Value::String(s)),
        Token::Name(var) => load_var(constants, locals, dst, var),
        _ => panic!("invalid argument"),
    };
    byte_codes.push(code);
}
