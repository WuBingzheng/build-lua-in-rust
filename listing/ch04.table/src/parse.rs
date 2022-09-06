use std::io::Read;
use std::cmp::Ordering;
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
    Index(usize, usize),
    IndexStr(usize, usize),
    IndexInt(usize, u8),
    Call,
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
        self.block()
    }

    // BNF:
    //   block ::= {stat} [retstat]
    //   stat ::= `;` |
    // 	   varlist `=` explist |
    // 	   functioncall |
    // 	   label |
    // 	   break |
    // 	   goto Name |
    // 	   do block end |
    // 	   while exp do block end |
    // 	   repeat block until exp |
    // 	   if exp then block {elseif exp then block} [else block] end |
    // 	   for Name `=` exp `,` exp [`,` exp] do block end |
    // 	   for namelist in explist do block end |
    // 	   function funcname funcbody |
    // 	   local function Name funcbody |
    // 	   local attnamelist [`=` explist]
    fn block(&mut self) {
        loop {
            // reset sp before each statement
            self.sp = self.locals.len();

            match self.lex.next() {
                Token::SemiColon => (),
                Token::Name(name) => { // functioncall or assignment
                    let desc = self.prefixexp(Token::Name(name));
                    if desc == ExpDesc::Call {
                        // do nothing
                    } else {
                        self.assignment(desc);
                    }
                }
                Token::Local => self.local(),
                Token::Eos => break,
                t => panic!("unexpected token: {t:?}"),
            }
        }
    }

    // BNF:
    //   local attnamelist [`=` explist]
    //   attnamelist ::=  Name attrib {`,` Name attrib}
    fn local(&mut self) {
        let mut vars = Vec::new();
        let nexp = loop {
            vars.push(self.read_name());

            match self.lex.peek() {
                Token::Comma => {
                    self.lex.next(); // more var
                }
                Token::Assign => {
                    self.lex.next();
                    break self.explist();
                }
                _ => break 0, // no explist
            }
        };

        if nexp < vars.len() {
            let ivar = self.locals.len() + nexp;
            let nnil = vars.len() - nexp;
            self.byte_codes.push(ByteCode::LoadNil(ivar as u8, nnil as u8));
        }

        // append vars into self.locals after evaluating explist
        self.locals.append(&mut vars);
    }

// ANCHOR: assignment
    // BNF:
    //   varlist = explist
    fn assignment(&mut self, first_var: ExpDesc) {
        // read varlist into @vars
        let mut vars = vec![first_var];
        loop {
            match self.lex.next() {
                Token::Comma => { // more variable
                    let token = self.lex.next();
                    vars.push(self.prefixexp(token));
                }
                Token::Assign => break,
                t => panic!("invalid assign {t:?}"),
            }
        }

        // simlar to explist(), except that does not discharge the last exp.
        let exp_sp0 = self.sp;  // discharge front exps starts at here
        let mut nfexp = 0;  // number of discharged exps, excluding the last one
        let last_exp = loop {
            let desc = self.exp();

            if self.lex.peek() == &Token::Comma {
                // there are more exps, so discharge the previous one
                self.lex.next();
                self.discharge(exp_sp0 + nfexp, desc);
                nfexp += 1;
            } else { // the last exp
                break desc;
            }
        };

        // assignment last variable
        match (nfexp + 1).cmp(&vars.len()) {
            Ordering::Equal => {
                // assign last variable directly to avoid potential discharging
                let last_var = vars.pop().unwrap();
                self.assign_var(last_var, last_exp);
            }
            Ordering::Less => {
                todo!("expand last exps");
            }
            Ordering::Greater => {
                // drop extra exps
                nfexp = vars.len();
            }
        }

        // assign previous variables from tmp registers, in reverse order
        while let Some(var) = vars.pop() {
            nfexp -= 1;
            self.assign_from_reg(var, exp_sp0 + nfexp);
        }
    }
// ANCHOR_END: assignment

// ANCHOR: assign_var
    fn assign_var(&mut self, var: ExpDesc, value: ExpDesc) {
        if let ExpDesc::Local(i) = var {
            // self.sp will be set to i+1 in self.discharge(), which is
            // NOT expected, but it's ok because self.sp will not be used
            // before next statement.
            self.discharge(i, value);
        } else {
            let value = self.try_into_const(value);
            if let ExpDesc::Const(i) = value {
                self.assign_from_const(var, i);
            } else {
                let i = self.discharge_tmp(value);
                self.assign_from_reg(var, i);
            }
        }
    }
// ANCHOR_END: assign_var

// ANCHOR: assign_from_reg_const
    fn assign_from_reg(&mut self, desc: ExpDesc, value: usize) {
        let code = match desc {
            ExpDesc::Local(i) => ByteCode::Move(i as u8, value as u8),
            ExpDesc::Global(name) => ByteCode::SetGlobal(name as u8, value as u8),
            ExpDesc::Index(t, key) => ByteCode::SetTable(t as u8, key as u8, value as u8),
            ExpDesc::IndexStr(t, key) => ByteCode::SetField(t as u8, key as u8, value as u8),
            ExpDesc::IndexInt(t, key) => ByteCode::SetInt(t as u8, key, value as u8),
            _ => panic!("assign from reg"),
        };
        self.byte_codes.push(code);
    }

    fn assign_from_const(&mut self, desc: ExpDesc, value: usize) {
        let code = match desc {
            ExpDesc::Local(i) => ByteCode::LoadConst(i as u8, value as u16),
            ExpDesc::Global(name) => ByteCode::SetGlobalConst(name as u8, value as u8),
            ExpDesc::Index(t, key) => ByteCode::SetTableConst(t as u8, key as u8, value as u8),
            ExpDesc::IndexStr(t, key) => ByteCode::SetFieldConst(t as u8, key as u8, value as u8),
            ExpDesc::IndexInt(t, key) => ByteCode::SetIntConst(t as u8, key, value as u8),
            _ => panic!("assign from const"),
        };
        self.byte_codes.push(code);
    }
// ANCHOR_END: assign_from_reg_const

    fn add_const(&mut self, c: impl Into<Value>) -> usize {
        let c = c.into();
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

    fn load_exp(&mut self) {
        let sp0 = self.sp;
        let desc = self.exp();
        self.discharge(sp0, desc);
    }

    // explist ::= exp {`,` exp}
    fn explist(&mut self) -> usize {
        let mut n = 0;
        let sp0 = self.sp;
        loop {
            let desc = self.exp();
            self.discharge(sp0 + n, desc);

            n += 1;
            if self.lex.peek() != &Token::Comma {
                return n;
            }
            self.lex.next();
        }
    }

    // BNF:
    //   exp ::= nil | false | true | Numeral | LiteralString | `...` | functiondef |
    //           prefixexp | tableconstructor | exp binop exp | unop exp
    //
    // Remove left recursion:
    //
    //   exp ::= (nil | false | true | Numeral | LiteralString | `...` | functiondef |
    //           prefixexp | tableconstructor | unop exp) A'
    // where:
    //   A' ::= binop exp A' | Epsilon
    fn exp(&mut self) -> ExpDesc {
        self.exp_limit(0)
    }
    fn exp_limit(&mut self, _: i32) -> ExpDesc {
        // beta
        match self.lex.next() {
            Token::Nil => ExpDesc::Nil,
            Token::True => ExpDesc::Boolean(true),
            Token::False => ExpDesc::Boolean(false),
            Token::Integer(i) => ExpDesc::Integer(i),
            Token::Float(f) => ExpDesc::Float(f),
            Token::String(s) => ExpDesc::String(s),
            Token::Function => todo!("Function"),
            Token::CurlyL => self.table_constructor(),
            Token::Sub | Token::Not | Token::BitXor | Token::Len => todo!("unop"),
            Token::Dots => todo!("dots"),
            t => self.prefixexp(t),
        }
        // TODO loop for binop
    }

    // BNF:
    //   prefixexp ::= var | functioncall | `(` exp `)`
    //   var ::=  Name | prefixexp `[` exp `]` | prefixexp `.` Name
    //   functioncall ::=  prefixexp args | prefixexp `:` Name args
    //
    // We need to remove left recursion amount these 3 rules.
    //
    // First unfold 'var' and 'functioncall' in 'prefixexp' to remove indirect recursion:
    //
    //   prefixexp ::= Name | prefixexp `[` exp `]` | prefixexp `.` Name | prefixexp args | prefixexp `:` Name args | `(` exp `)`
    //
    // Then remove the direct left recursion following:
    //   A ::= A alpha | beta
    // into
    //   A ::= beta A'
    //   A' ::= alpha A' | Epsilon
    //
    // so
    //   prefixexp ::= prefixexp (`[` exp `]` | `.` Name | args | `:` Name args) | Name | `(` exp `)`
    //               = prefixexp alpha | beta
    // where
    //   alpha ::= `[` exp `]` | `.` Name | args | `:` Name args
    //   beta ::= Name | `(` exp `)`
    //
    // Finally we get:
    //   prefixexp ::= beta A'
    //               = (Name | `(` exp `)`) A'
    // where:
    //   A' ::= alpha A' | Epsilon
    //        = (`[` exp `]` | `.` Name | args | `:` Name args) A' | Epsilon
// ANCHOR: prefixexp
    fn prefixexp(&mut self, ahead: Token) -> ExpDesc {
        let sp0 = self.sp;

        // beta
        let mut desc = match ahead {
            Token::Name(name) => self.simple_name(name),
            Token::ParL => { // `(` exp `)`
                let desc = self.exp();
                self.lex.expect(Token::ParR);
                desc
            }
            t => panic!("invalid prefixexp {t:?}"),
        };

        // A' = alpha A'
        loop {
            match self.lex.peek() {
                Token::SqurL => { // `[` exp `]`
                    self.lex.next();
                    let itable = self.discharge_if_need(sp0, desc);
                    desc = match self.exp() {
                        ExpDesc::String(s) => ExpDesc::IndexStr(itable, self.add_const(s)),
                        ExpDesc::Integer(i) if u8::try_from(i).is_ok() => ExpDesc::IndexInt(itable, u8::try_from(i).unwrap()),
                        key => ExpDesc::Index(itable, self.discharge_if_need(sp0, key)),
                    };

                    self.lex.expect(Token::SqurR);
                }
                Token::Dot => { // .Name
                    self.lex.next();
                    let name = self.read_name();
                    let itable = self.discharge_if_need(sp0, desc);
                    desc = ExpDesc::IndexStr(itable, self.add_const(name));
                }
                Token::Colon => todo!("args"), // :Name args
                Token::ParL | Token::CurlyL | Token::String(_) => { // args
                    self.discharge(sp0, desc);
                    desc = self.args();
                }
                _ => { // Epsilon
                    return desc;
                }
            }
        }
    }
// ANCHOR_END: prefixexp

    fn simple_name(&mut self, name: String) -> ExpDesc {
        // search reversely, so new variable covers old one with same name
        if let Some(ilocal) = self.locals.iter().rposition(|v| v == &name) {
            ExpDesc::Local(ilocal)
        } else {
            ExpDesc::Global(self.add_const(name))
        }
    }

    // args ::= `(` [explist] `)` | tableconstructor | LiteralString
    fn args(&mut self) -> ExpDesc {
        let ifunc = self.sp - 1;
        let argn = match self.lex.next() {
            Token::ParL => {
                if self.lex.peek() != &Token::ParR {
                    let argn = self.explist();
                    self.lex.expect(Token::ParR);
                    argn
                } else {
                    self.lex.next();
                    0
                }
            }
            Token::CurlyL => {
                self.table_constructor();
                1
            }
            Token::String(s) => {
                self.discharge(ifunc+1, ExpDesc::String(s));
                1
            }
            t => panic!("invalid args {t:?}"),
        };

        self.byte_codes.push(ByteCode::Call(ifunc as u8, argn as u8));
        ExpDesc::Call
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
            ExpDesc::Nil => ByteCode::LoadNil(dst as u8, 1),
            ExpDesc::Boolean(b) => ByteCode::LoadBool(dst as u8, b),
            ExpDesc::Integer(i) =>
                if let Ok(i) = i16::try_from(i) {
                    ByteCode::LoadInt(dst as u8, i)
                } else {
                    self.load_const(dst, i)
                }
            ExpDesc::Float(f) => self.load_const(dst, f),
            ExpDesc::String(s) => self.load_const(dst, s),
            ExpDesc::Const(_) => panic!("no here"),
            ExpDesc::Local(src) =>
                if dst != src {
                    ByteCode::Move(dst as u8, src as u8)
                } else {
                    return;
                }
            ExpDesc::Global(iname) => ByteCode::GetGlobal(dst as u8, iname as u8),
            ExpDesc::Index(itable, ikey) => ByteCode::GetTable(dst as u8, itable as u8, ikey as u8),
            ExpDesc::IndexStr(itable, ikey) => ByteCode::GetField(dst as u8, itable as u8, ikey as u8),
            ExpDesc::IndexInt(itable, ikey) => ByteCode::GetInt(dst as u8, itable as u8, ikey),
            ExpDesc::Call => todo!("discharge Call"),
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
                }
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
                }
                _ => { // exp
                    narray += 1;
                    self.load_exp();

                    if self.sp - (table + 1) > 50 { // too many, reset it
                        self.byte_codes.push(ByteCode::SetList(table as u8, (self.sp - (table + 1)) as u8));
                        self.sp = table + 1;
                    }
                }
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

    fn read_name(&mut self) -> String {
        if let Token::Name(name) = self.lex.next() {
            name
        } else {
            panic!("expect name");
        }
    }
}
