use std::io::Read;
use crate::lex::{Lex, Token};
use crate::bytecode::ByteCode;
use crate::value::Value;

#[derive(Debug, PartialEq)]
enum ExpDesc {
    Nil,
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(Vec<u8>),
    Const(usize), // add above types into ParseProto.constants
    Local(usize), // on stack, including local and temprary variables
    Global(usize), // global variable
}

// ANCHOR: proto
#[derive(Debug)]
pub struct ParseProto<R: Read> {
    pub constants: Vec::<Value>,
    pub byte_codes: Vec::<ByteCode>,

    sp: usize,
    locals: Vec::<String>,
    lex: Lex<R>,
}
// ANCHOR_END: proto

impl<R: Read> ParseProto<R> {
    pub fn load(input: R) -> Self {
        let mut proto = ParseProto {
            constants: Vec::new(),
            byte_codes: Vec::new(),
            sp: 0,
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
            // reset sp before each statement
            self.sp = self.locals.len();

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

        // function, variable
        let code = self.load_var(ifunc, name);
        self.byte_codes.push(code);
        self.sp = ifunc + 1; // tmp

        // argument, (exp) or "string"
        match self.lex.next() {
            Token::ParL => { // '('
                self.load_exp();

                if self.lex.next() != Token::ParR { // ')'
                    panic!("expected `)`");
                }
            }
            Token::CurlyL => { // '{'
                self.table_constructor();
            }
            Token::String(s) => {
                let code = self.load_const(ifunc+1, s);
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

        self.load_exp();

        // add to locals after load_exp()
        self.locals.push(var);
    }

    fn assignment(&mut self, var: String) {
        self.lex.next(); // `=`

        if let Some(i) = self.get_local(&var) {
            // local variable
            let desc = self.exp();
            self.discharge(i, desc);
        } else {
            // global variable
            let dst = self.add_const(var) as u8;

            let desc = self.exp();
            let code = match self.try_into_const(desc) {
                ExpDesc::Const(i) => ByteCode::SetGlobalConst(dst, i as u8),
                ExpDesc::Local(i) => ByteCode::SetGlobal(dst, i as u8),
                ExpDesc::Global(i) => ByteCode::SetGlobalGlobal(dst, i as u8),
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

    fn load_exp(&mut self) {
        let sp0 = self.sp;
        let desc = self.exp();
        self.discharge(sp0, desc);
    }

    fn exp(&mut self) -> ExpDesc {
        match self.lex.next() {
            Token::Nil => ExpDesc::Nil,
            Token::True => ExpDesc::Boolean(true),
            Token::False => ExpDesc::Boolean(false),
            Token::Integer(i) => ExpDesc::Integer(i),
            Token::Float(f) => ExpDesc::Float(f),
            Token::String(s) => ExpDesc::String(s),
            Token::Name(var) => self.simple_name(var),
            Token::CurlyL => self.table_constructor(),
            t => panic!("invalid exp: {:?}", t),
        }
    }

    fn simple_name(&mut self, name: String) -> ExpDesc {
        // search reversely, so new variable covers old one with same name
        if let Some(ilocal) = self.locals.iter().rposition(|v| v == &name) {
            ExpDesc::Local(ilocal)
        } else {
            ExpDesc::Global(self.add_const(name))
        }
    }

    fn discharge_tmp(&mut self, desc: ExpDesc) -> usize {
        self.discharge_if_need(self.sp, desc)
    }

    fn discharge_if_need(&mut self, dst: usize, desc: ExpDesc) -> usize {
        if let ExpDesc::Local(i) = desc {
            i
        } else {
            self.discharge(dst, desc);
            dst
        }
    }

    // discharge @desc into @dst, and set self.sp=dst+1
    fn discharge(&mut self, dst: usize, desc: ExpDesc) {
        let code = match desc {
            ExpDesc::Nil => ByteCode::LoadNil(dst as u8),
            ExpDesc::Boolean(b) => ByteCode::LoadBool(dst as u8, b),
            ExpDesc::Integer(i) =>
                if let Ok(i) = i16::try_from(i) {
                    ByteCode::LoadInt(dst as u8, i)
                } else {
                    self.load_const(dst, i)
                }
            ExpDesc::Float(f) => self.load_const(dst, f),
            ExpDesc::String(s) => self.load_const(dst, s),
            ExpDesc::Const(i) => ByteCode::LoadConst(dst as u8, i as u16),
            ExpDesc::Local(src) =>
                if dst != src {
                    ByteCode::Move(dst as u8, src as u8)
                } else {
                    return;
                }
            ExpDesc::Global(iname) => ByteCode::GetGlobal(dst as u8, iname as u8),
        };
        self.byte_codes.push(code);
        self.sp = dst + 1;
    }

    fn try_into_const(&mut self, desc: ExpDesc) -> ExpDesc {
        match desc {
            ExpDesc::Nil => ExpDesc::Const(self.add_const(())),
            ExpDesc::Boolean(b) => ExpDesc::Const(self.add_const(b)),
            ExpDesc::Integer(i) => ExpDesc::Const(self.add_const(i)),
            ExpDesc::Float(f) => ExpDesc::Const(self.add_const(f)),
            ExpDesc::String(s) => ExpDesc::Const(self.add_const(s)),
            _ => desc,
        }
    }

// ANCHOR: table_constructor
    fn table_constructor(&mut self) -> ExpDesc {
        let table = self.sp;
        self.sp += 1;

        let inew = self.byte_codes.len();
        self.byte_codes.push(ByteCode::NewTable(table as u8, 0, 0));

        let mut narray = 0;
        let mut nmap = 0;
        loop {
            match self.lex.peek() {
                Token::CurlyR => { // `}`
                    self.lex.next();
                    break;
                }
                Token::SqurL => { // `[` exp `]` `=` exp
                    nmap += 1;
                    self.lex.next();
                    let sp0 = self.sp;

                    let key = self.exp(); // key
                    self.lex.expect(Token::SqurR); // `]`
                    self.lex.expect(Token::Assign); // `=`
                    let value = self.exp(); // value

                    let (op, opk, key): (fn(u8,u8,u8)->ByteCode, fn(u8,u8,u8)->ByteCode, usize) = match key {
                        ExpDesc::Local(i) => (ByteCode::SetTable, ByteCode::SetTableConst, i),
                        ExpDesc::String(s) => (ByteCode::SetField, ByteCode::SetFieldConst, self.add_const(s)),
                        ExpDesc::Integer(i) if u8::try_from(i).is_ok() => (ByteCode::SetInt, ByteCode::SetIntConst, i as usize),
                        ExpDesc::Nil => panic!("nil can not be table key"),
                        ExpDesc::Float(f) if f.is_nan() => panic!("NaN can not be table key"),
                        _ => (ByteCode::SetTable, ByteCode::SetTableConst, self.discharge_tmp(key)),
                    };
                    self.new_table_entry(op, opk, table, key, value);
                    self.sp = sp0;
                },
                Token::Name(_) => { // Name `=` exp
                    nmap += 1;
                    let sp0 = self.sp;

                    let key = if let Token::Name(key) = self.lex.next() {
                        self.add_const(key)
                    } else {
                        panic!("impossible");
                    };
                    self.lex.expect(Token::Assign); // `=`

                    let value = self.exp();

                    self.new_table_entry(ByteCode::SetField, ByteCode::SetFieldConst, table, key, value);
                    self.sp = sp0;
                },
                _ => { // exp
                    narray += 1;
                    self.load_exp();

                    if self.sp - (table + 1) > 50 { // too many, reset it
                        self.byte_codes.push(ByteCode::SetList(table as u8, (self.sp - (table + 1)) as u8));
                        self.sp = table + 1;
                    }
                },
            }

            match self.lex.next() {
                Token::SemiColon | Token::Comma => (),
                Token::CurlyR => break,
                t => panic!("invalid table {t:?}"),
            }
        }

        if self.sp > table + 1 {
            self.byte_codes.push(ByteCode::SetList(table as u8, (self.sp - (table + 1)) as u8));
        }

        // reset narray and nmap
        self.byte_codes[inew] = ByteCode::NewTable(table as u8, narray, nmap);

        self.sp = table + 1;
        ExpDesc::Local(table)
    }
// ANCHOR_END: table_constructor

    fn new_table_entry(&mut self, op: fn(u8,u8,u8)->ByteCode, opk: fn(u8,u8,u8)->ByteCode,
            t: usize, key: usize, value: ExpDesc) {

        let code = match self.try_into_const(value) {
            ExpDesc::Const(i) => opk(t as u8, key as u8, i as u8),
            desc => op(t as u8, key as u8, self.discharge_tmp(desc) as u8),
        };
        self.byte_codes.push(code);
    }
}
