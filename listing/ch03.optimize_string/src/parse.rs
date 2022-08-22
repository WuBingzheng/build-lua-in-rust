use std::io::Read;
use crate::lex::{Lex, Token};
use crate::bytecode::ByteCode;
use crate::value::Value;

// ANCHOR: proto
#[derive(Debug)]
pub struct ParseProto<R: Read> {
    pub constants: Vec::<Value>,
    pub byte_codes: Vec::<ByteCode>,

    locals: Vec::<String>,
    lex: Lex<R>,
}
// ANCHOR_END: proto

impl<R: Read> ParseProto<R> {
    pub fn load(input: R) -> Self {
        let mut proto = ParseProto {
            constants: Vec::new(),
            byte_codes: Vec::new(),
            locals: Vec::new(),
            lex: Lex::new(input),
        };

        proto.chunk();

        println!("constants: {:?}", &proto.constants);
        println!("byte_codes:");
        for c in proto.byte_codes.iter() {
            println!("  {:?}", c);
        }

        proto
    }

    fn chunk(&mut self) {
        loop {
            match self.lex.next() {
                Token::Name(name) => {
                    if self.lex.peek() == &Token::Assign {
                        self.assignment(name);
                    } else {
                        self.function_call(name);
                    }
                }
                Token::Local => self.local(),
                Token::Eos => break,
                t => panic!("unexpected token: {t:?}"),
            }
        }
    }

    // Name LiteralString
    // Name ( exp )
    fn function_call(&mut self, name: String) {
        let ifunc = self.locals.len();
        let iarg = ifunc + 1;

        // function, variable
        let code = self.load_var(ifunc, name);
        self.byte_codes.push(code);

        // argument, (exp) or "string"
        match self.lex.next() {
            Token::ParL => { // '('
                self.load_exp(iarg);

                if self.lex.next() != Token::ParR { // ')'
                    panic!("expected `)`");
                }
            }
            Token::String(s) => {
                let code = self.load_const(iarg, s);
                self.byte_codes.push(code);
            }
            t => panic!("expected string {t:?}"),
        }

        self.byte_codes.push(ByteCode::Call(ifunc as u8, 1));
    }

    // local Name = exp
    fn local(&mut self) {
        let var = if let Token::Name(var) = self.lex.next() {
            var
        } else {
            panic!("expected variable");
        };

        if self.lex.next() != Token::Assign {
            panic!("expected `=`");
        }

        self.load_exp(self.locals.len());

        // add to locals after load_exp()
        self.locals.push(var);
    }

    fn assignment(&mut self, var: String) {
        self.lex.next(); // `=`

        if let Some(i) = self.get_local(&var) {
            // local variable
            self.load_exp(i);
        } else {
            // global variable
            let dst = self.add_const(var) as u8;

            let code = match self.lex.next() {
                // from const values
                Token::Nil => ByteCode::SetGlobalConst(dst, self.add_const(()) as u8),
                Token::True => ByteCode::SetGlobalConst(dst, self.add_const(true) as u8),
                Token::False => ByteCode::SetGlobalConst(dst, self.add_const(false) as u8),
                Token::Integer(i) => ByteCode::SetGlobalConst(dst, self.add_const(i) as u8),
                Token::Float(f) => ByteCode::SetGlobalConst(dst, self.add_const(f) as u8),
                Token::String(s) => ByteCode::SetGlobalConst(dst, self.add_const(s) as u8),

                // from variable
                Token::Name(var) =>
                    if let Some(i) = self.get_local(&var) {
                        // local variable
                        ByteCode::SetGlobal(dst, i as u8)
                    } else {
                        // global variable
                        ByteCode::SetGlobalGlobal(dst, self.add_const(var) as u8)
                    }

                _ => panic!("invalid argument"),
            };
            self.byte_codes.push(code);
        }
    }

// ANCHOR: add_const
    fn add_const(&mut self, c: impl Into<Value>) -> usize {
        let c = c.into();
// ANCHOR_END: add_const
        let constants = &mut self.constants;
        constants.iter().position(|v| v == &c)
            .unwrap_or_else(|| {
                constants.push(c);
                constants.len() - 1
            })
    }

    fn load_const(&mut self, dst: usize, c: impl Into<Value>) -> ByteCode {
        ByteCode::LoadConst(dst as u8, self.add_const(c) as u16)
    }

    fn load_var(&mut self, dst: usize, name: String) -> ByteCode {
        if let Some(i) = self.get_local(&name) {
            // local variable
            ByteCode::Move(dst as u8, i as u8)
        } else {
            // global variable
            let ic = self.add_const(name);
            ByteCode::GetGlobal(dst as u8, ic as u8)
        }
    }

    fn get_local(&self, name: &str) -> Option<usize> {
        self.locals.iter().rposition(|v| v == name)
    }

    fn load_exp(&mut self, dst: usize) {
        let code = match self.lex.next() {
            Token::Nil => ByteCode::LoadNil(dst as u8),
            Token::True => ByteCode::LoadBool(dst as u8, true),
            Token::False => ByteCode::LoadBool(dst as u8, false),
            Token::Integer(i) =>
                if let Ok(ii) = i16::try_from(i) {
                    ByteCode::LoadInt(dst as u8, ii)
                } else {
                    self.load_const(dst, i)
                }
            Token::Float(f) => self.load_const(dst, f),
            Token::String(s) => self.load_const(dst, s),
            Token::Name(var) => self.load_var(dst, var),
            _ => panic!("invalid argument"),
        };
        self.byte_codes.push(code);
    }
}
