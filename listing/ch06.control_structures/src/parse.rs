use std::io::Read;
use std::cmp::Ordering;
use crate::lex::{Lex, Token};
use crate::bytecode::ByteCode;
use crate::value::Value;
use crate::utils::ftoi;

#[derive(PartialEq)]
enum ExpDesc {
    // constants
    Nil,
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(Vec<u8>),

    // variables
    Local(usize), // including temprary variables on stack
    Global(usize),

    // table index
    Index(usize, usize),
    IndexField(usize, usize),
    IndexInt(usize, u8),

    // function call
    Call,

    // operators
    UnaryOp(fn(u8,u8)->ByteCode, usize), // (opcode, operand)
    BinaryOp(fn(u8,u8,u8)->ByteCode, usize, usize), // (opcode, left-operand, right-operand)
}

enum ConstStack {
    Const(usize),
    Stack(usize),
}

#[derive(Debug)]
struct GotoLabel {
    name: String,
    icode: usize,
    nvar: usize,
}

// ANCHOR: proto
#[derive(Debug)]
pub struct ParseProto<R: Read> {
    pub constants: Vec::<Value>,
    pub byte_codes: Vec::<ByteCode>,

    sp: usize,
    locals: Vec::<String>,
    break_blocks: Vec::<Vec::<usize>>,
    continue_blocks: Vec::<Vec::<(usize, usize)>>,
    gotos: Vec<GotoLabel>,
    labels: Vec<GotoLabel>,
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
            break_blocks: Vec::new(),
            continue_blocks: Vec::new(),
            gotos: Vec::new(),
            labels: Vec::new(),
            lex: Lex::new(input),
        };

        proto.chunk();

        println!("constants: {:?}", &proto.constants);
        println!("byte_codes:");
        for (i,c) in proto.byte_codes.iter().enumerate() {
            println!("  {i}\t{c:?}");
        }

        proto
    }

    fn chunk(&mut self) {
        assert_eq!(self.block(), Token::Eos);
        if let Some(goto) = self.gotos.first() {
            panic!("goto {} no destination", &goto.name);
        }
    }

    // BNF:
    //   block ::= {stat} [retstat]
    //   stat ::= `;` |
    //     varlist `=` explist |
    //     functioncall |
    //     label |
    //     break |
    //     goto Name |
    //     do block end |
    //     while exp do block end |
    //     repeat block until exp |
    //     if exp then block {elseif exp then block} [else block] end |
    //     for Name `=` exp `,` exp [`,` exp] do block end |
    //     for namelist in explist do block end |
    //     function funcname funcbody |
    //     local function Name funcbody |
    //     local attnamelist [`=` explist]
    fn block(&mut self) -> Token {
        let nvar = self.locals.len();
        let end_token = self.block_scope();
        self.locals.truncate(nvar); // expire internal local variables
        end_token
    }
    fn block_scope(&mut self) -> Token {
        let igoto = self.gotos.len();
        let ilabel = self.labels.len();
        loop {
            // reset sp before each statement
            self.sp = self.locals.len();

// ANCHOR: func_or_assign
            match self.lex.next() {
                Token::SemiColon => (),
                t@Token::Name(_) | t@Token::ParL => {
                    // this is not standard!
                    if self.try_continue_stat(&t) {
                        continue;
                    }

                    // functioncall and var-assignment both begin with
                    // `prefixexp` which begins with `Name` or `(`.
                    let desc = self.prefixexp(t);
                    if desc == ExpDesc::Call {
                        // prefixexp() matches the whole functioncall
                        // statement, so nothing more to do
                    } else {
                        // prefixexp() matches only the first variable, so we
                        // continue the statement
                        self.assignment(desc);
                    }
                }
// ANCHOR_END: func_or_assign
                Token::Local => self.local(),
                Token::If => self.if_stat(),
                Token::While => self.while_stat(),
                Token::Repeat => self.repeat_stat(),
                Token::For => self.for_stat(),
                Token::Break => self.break_stat(),
                Token::Do => self.do_stat(),
                Token::DoubColon => self.label_stat(),
                Token::Goto => self.goto_stat(),
                t => {
                    self.close_goto_labels(igoto, ilabel);
                    break t;
                }
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
    //   varlist ::= var {`,` var}
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
            self.assign_from_stack(var, exp_sp0 + nfexp);
        }
    }
// ANCHOR_END: assignment

    // BNF:
    //   if exp then block {elseif exp then block} [else block] end
    fn if_stat(&mut self) {
        let mut jmp_ends = Vec::new();

        // == if exp then block
        let mut end_token = self.do_if_block(&mut jmp_ends);

        // == {elseif exp then block}
        while end_token == Token::Elseif {
            end_token = self.do_if_block(&mut jmp_ends);
        }

        // == [else block]
        if end_token == Token::Else {
            end_token = self.block();
        }

        assert_eq!(end_token, Token::End);

        let iend = self.byte_codes.len() - 1;
        for i in jmp_ends.into_iter() {
            self.byte_codes[i] = ByteCode::Jump((iend - i) as i16);
        }
    }

    fn do_if_block(&mut self, jmp_ends: &mut Vec<usize>) -> Token {
        let icond = self.exp_discharge_any();
        self.lex.expect(Token::Then);

        // Test the condition and jump to the end of this block
        // if condition is false.
        // Make a fake byte-code to hold the place, and fix it at
        // end of this function.
        self.byte_codes.push(ByteCode::Test(0, 0));
        let itest = self.byte_codes.len() - 1;

        let end_token = self.block();

        // If there are following 'elseif' or 'else' blocks,
        // jump to the very end of this whole if-statment at the
        // end of this block.
        // Make a fake byte-code to hold the place, and fix it
        // at the end of whole if-statment.
        if matches!(end_token, Token::Elseif | Token::Else) {
            self.byte_codes.push(ByteCode::Jump(0));
            jmp_ends.push(self.byte_codes.len() - 1);
        }

        // fix the Test byte-code
        let iend = self.byte_codes.len() - 1;
        self.byte_codes[itest] = ByteCode::Test(icond as u8, (iend - itest) as i16);

        end_token
    }

    // BNF:
    //   while exp do block end
    fn while_stat(&mut self) {
        let istart = self.byte_codes.len();

        let icond = self.exp_discharge_any();
        self.lex.expect(Token::Do);

        // Test the condition and jump to the end of this block
        // if condition is false.
        // Make a fake byte-code to hold the place, and fix it at
        // end of this function.
        self.byte_codes.push(ByteCode::Test(0, 0));
        let itest = self.byte_codes.len() - 1;

        self.push_loop_block();

        assert_eq!(self.block(), Token::End);

        // jump back
        let iend = self.byte_codes.len();
        self.byte_codes.push(ByteCode::Jump(-((iend - istart) as i16) - 1));

        self.pop_loop_block(istart);

        // fix the Test byte-code
        self.byte_codes[itest] = ByteCode::Test(icond as u8, (iend - itest) as i16);
    }

    // BNF:
    //   repeat block until exp
    fn repeat_stat(&mut self) {
        let istart = self.byte_codes.len();

        self.push_loop_block();

        let nvar = self.locals.len();

        assert_eq!(self.block_scope(), Token::Until);
        let iend1 = self.byte_codes.len();

        let icond = self.exp_discharge_any();

        let iend2 = self.byte_codes.len();
        self.byte_codes.push(ByteCode::Test(icond as u8, -((iend2 - istart + 1) as i16)));

        self.pop_loop_block(iend1);

        // expire internal local variables AFTER reading condition exp
        // and pop_loop_block()
        self.locals.truncate(nvar);
    }

    // * numerical: for Name `=` ...
    // * generic:   for Name {, Name} in ...
    fn for_stat(&mut self) {
        let name = self.read_name();
        if self.lex.peek() == &Token::Assign {
            self.for_numerical(name);
        } else {
            todo!("generic for");
        }
    }

    // BNF:
    //   for Name `=` exp `,` exp [`,` exp] do block end
    fn for_numerical(&mut self, name: String) {
        self.lex.next(); // skip `=`

        match self.explist() {
            2 => self.discharge(self.sp, ExpDesc::Integer(1)),
            3 => (),
            _ => panic!("invalid numerical for exp"),
        }

        self.locals.push(name);
        self.locals.push(String::from(""));
        self.locals.push(String::from(""));

        self.lex.expect(Token::Do);

        self.byte_codes.push(ByteCode::ForPrepare(0, 0));
        let iprepare = self.byte_codes.len() - 1;
        let iname = self.sp - 3;

        self.push_loop_block();

        assert_eq!(self.block(), Token::End);

        self.locals.pop();
        self.locals.pop();
        self.locals.pop();

        let d = self.byte_codes.len() - iprepare;
        self.byte_codes.push(ByteCode::ForLoop(iname as u8, d as u16));
        self.byte_codes[iprepare] = ByteCode::ForPrepare(iname as u8, d as u16);

        self.pop_loop_block(self.byte_codes.len() - 1);
    }

    fn break_stat(&mut self) {
        if let Some(breaks) = self.break_blocks.last_mut() {
            self.byte_codes.push(ByteCode::Jump(0));
            breaks.push(self.byte_codes.len() - 1);
        } else {
            panic!("break outside loop");
        }
    }

    fn try_continue_stat(&mut self, name: &Token) -> bool {
        if let Token::Name(name) = name {
            if name.as_str() != "continue" {
                return false;
            }
            if !matches!(self.lex.peek(), Token::End | Token::Elseif | Token::Else) {
                return false;
            }

            if let Some(continues) = self.continue_blocks.last_mut() {
                self.byte_codes.push(ByteCode::Jump(0));
                continues.push((self.byte_codes.len() - 1, self.locals.len()));
            } else {
                panic!("continue outside loop");
            }
            true
        } else {
            false
        }
    }

    // before entering loop block
    fn push_loop_block(&mut self) {
        self.break_blocks.push(Vec::new());
        self.continue_blocks.push(Vec::new());
    }
    // after leaving loop block, fix `break` and `continue` Jumps
    fn pop_loop_block(&mut self, icontinue: usize) {
        // breaks
        let iend = self.byte_codes.len() - 1;
        for i in self.break_blocks.pop().unwrap().into_iter() {
            self.byte_codes[i] = ByteCode::Jump((iend - i) as i16);
        }

        // continues
        let end_nvar = self.locals.len();
        for (i, i_nvar) in self.continue_blocks.pop().unwrap().into_iter() {
            if i_nvar < end_nvar {
                panic!("continue jump into local scope");
            }
            self.byte_codes[i] = ByteCode::Jump((icontinue as isize - i as isize) as i16 - 1);
        }
    }

    // BNF:
    //   do block end
    fn do_stat(&mut self) {
        assert_eq!(self.block(), Token::End);
    }

    // BNF:
    //   label ::= `::` Name `::`
    fn label_stat(&mut self) {
        let name = self.read_name();
        self.lex.expect(Token::DoubColon);

        if self.labels.iter().any(|l|l.name == name) {
            panic!("duplicate label {name}");
        }

        self.labels.push(GotoLabel {
            name,
            icode: self.byte_codes.len(),
            nvar: self.locals.len() });
    }

    // BNF:
    //   goto Name
    fn goto_stat(&mut self) {
        let name = self.read_name();

        self.byte_codes.push(ByteCode::Jump(0));

        self.gotos.push(GotoLabel {
            name,
            icode: self.byte_codes.len() - 1,
            nvar: self.locals.len() });
    }

    // match the gotos and labels, and close the labels at the end of block
    fn close_goto_labels(&mut self, igoto: usize, ilabel: usize) {
        // try to match gotos defined in this block between all labels
        let mut no_dsts = Vec::new();
        for goto in self.gotos.drain(igoto..) {
            if let Some(label) = self.labels.iter().rev().find(|l|l.name == goto.name) {
                if label.icode != self.byte_codes.len() && label.nvar > goto.nvar {
                    // check if jump into local variables' scope:
                    // 1. label's byte-code is not the last one, meaning there
                    //    are non-void expressions following;
                    // 2. label's #vars is more than goto's, meaning there are
                    //    new local variables defined between the goto and label.
                    panic!("goto jump into scope {}", goto.name);
                }
                let d = (label.icode as isize - goto.icode as isize) as i16;
                self.byte_codes[goto.icode] = ByteCode::Jump(d - 1);
            } else {
                // no matched label
                no_dsts.push(goto);
            }
        }
        self.gotos.append(&mut no_dsts);

        // close the labels defined in this block
        self.labels.truncate(ilabel);
    }

    fn exp_discharge_any(&mut self) -> usize {
        let e = self.exp();
        self.discharge_any(e)
    }

// ANCHOR: assign_helper
    // process assignment: var = value
    fn assign_var(&mut self, var: ExpDesc, value: ExpDesc) {
        if let ExpDesc::Local(i) = var {
            // self.sp will be set to i+1 in self.discharge(), which is
            // NOT expected, but it's ok because self.sp will not be used
            // before next statement.
            self.discharge(i, value);
        } else {
            match self.discharge_const(value) {
                ConstStack::Const(i) => self.assign_from_const(var, i),
                ConstStack::Stack(i) => self.assign_from_stack(var, i),
            }
        }
    }

    fn assign_from_stack(&mut self, var: ExpDesc, value: usize) {
        let code = match var {
            ExpDesc::Local(i) => ByteCode::Move(i as u8, value as u8),
            ExpDesc::Global(name) => ByteCode::SetGlobal(name as u8, value as u8),
            ExpDesc::Index(t, key) => ByteCode::SetTable(t as u8, key as u8, value as u8),
            ExpDesc::IndexField(t, key) => ByteCode::SetField(t as u8, key as u8, value as u8),
            ExpDesc::IndexInt(t, key) => ByteCode::SetInt(t as u8, key, value as u8),
            _ => panic!("assign from stack"),
        };
        self.byte_codes.push(code);
    }

    fn assign_from_const(&mut self, var: ExpDesc, value: usize) {
        let code = match var {
            ExpDesc::Global(name) => ByteCode::SetGlobalConst(name as u8, value as u8),
            ExpDesc::Index(t, key) => ByteCode::SetTableConst(t as u8, key as u8, value as u8),
            ExpDesc::IndexField(t, key) => ByteCode::SetFieldConst(t as u8, key as u8, value as u8),
            ExpDesc::IndexInt(t, key) => ByteCode::SetIntConst(t as u8, key, value as u8),
            _ => panic!("assign from const"),
        };
        self.byte_codes.push(code);
    }
// ANCHOR_END: assign_helper

    fn add_const(&mut self, c: impl Into<Value>) -> usize {
        let c = c.into();
        let constants = &mut self.constants;
        constants.iter().position(|v| v.same(&c))
            .unwrap_or_else(|| {
                constants.push(c);
                constants.len() - 1
            })
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
    fn exp_limit(&mut self, limit: i32) -> ExpDesc {
        let ahead = self.lex.next();
        self.do_exp(limit, ahead)
    }
    fn exp_with_ahead(&mut self, ahead: Token) -> ExpDesc {
        self.do_exp(0, ahead)
    }
    fn do_exp(&mut self, limit: i32, ahead: Token) -> ExpDesc {
        // beta
        let mut desc = match ahead {
            Token::Nil => ExpDesc::Nil,
            Token::True => ExpDesc::Boolean(true),
            Token::False => ExpDesc::Boolean(false),
            Token::Integer(i) => ExpDesc::Integer(i),
            Token::Float(f) => ExpDesc::Float(f),
            Token::String(s) => ExpDesc::String(s),

            Token::Dots => todo!("dots"),
            Token::Function => todo!("Function"),
            Token::CurlyL => self.table_constructor(),

            Token::Sub => self.unop_neg(),
            Token::Not => self.unop_not(),
            Token::BitNot => self.unop_bitnot(),
            Token::Len => self.unop_len(),

            t => self.prefixexp(t),
        };

        // A' = alpha A'
        loop {
            /* Expand only if next operator has priority higher than 'limit'.
             * Non-operator tokens' priority is -1(lowest) so they always break here. */
            let (left_pri, right_pri) = binop_pri(self.lex.peek());
            if left_pri <= limit {
                return desc;
            }

            /* Discharge left operand before reading right operand, which may
             * affect the evaluation of left operand. e.g. `t.k + f(t) * 1`,
             * the `f(t)` may change `t.k`, so we must evaluate `t.k` before
             * calling `f(t)`.
             *
             * But we do not discharge constants, because they will not be
             * affected by right operand. Besides we try to fold constants
             * in process_binop() later.
             */
            if !matches!(desc, ExpDesc::Integer(_) | ExpDesc::Float(_) | ExpDesc::String(_)) {
                desc = ExpDesc::Local(self.discharge_any(desc));
            }

            let binop = self.lex.next();
            let right_desc = self.exp_limit(right_pri);
            desc = self.process_binop(binop, desc, right_desc);
        }
    }

    // used for unary operand
// ANCHOR: exp_unop
    fn exp_unop(&mut self) -> ExpDesc {
        self.exp_limit(12) // 12 is all unary operators' priority
    }
// ANCHOR_END: exp_unop

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
                        ExpDesc::String(s) => ExpDesc::IndexField(itable, self.add_const(s)),
                        ExpDesc::Integer(i) if u8::try_from(i).is_ok() => ExpDesc::IndexInt(itable, u8::try_from(i).unwrap()),
                        key => ExpDesc::Index(itable, self.discharge_any(key)),
                    };

                    self.lex.expect(Token::SqurR);
                }
                Token::Dot => { // .Name
                    self.lex.next();
                    let name = self.read_name();
                    let itable = self.discharge_if_need(sp0, desc);
                    desc = ExpDesc::IndexField(itable, self.add_const(name));
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

// ANCHOR: unop_neg
    fn unop_neg(&mut self) -> ExpDesc {
        match self.exp_unop() {
            ExpDesc::Integer(i) => ExpDesc::Integer(-i),
            ExpDesc::Float(f) => ExpDesc::Float(-f),
            ExpDesc::Nil | ExpDesc::Boolean(_) | ExpDesc::String(_) => panic!("invalid - operator"),
            desc => ExpDesc::UnaryOp(ByteCode::Neg, self.discharge_any(desc))
        }
    }
// ANCHOR_END: unop_neg
    fn unop_not(&mut self) -> ExpDesc {
        match self.exp_unop() {
            ExpDesc::Nil => ExpDesc::Boolean(true),
            ExpDesc::Boolean(b) => ExpDesc::Boolean(!b),
            ExpDesc::Integer(_) | ExpDesc::Float(_) | ExpDesc::String(_) => ExpDesc::Boolean(false),
            desc => ExpDesc::UnaryOp(ByteCode::Not, self.discharge_any(desc))
        }
    }
    fn unop_bitnot(&mut self) -> ExpDesc {
        match self.exp_unop() {
            ExpDesc::Integer(i) => ExpDesc::Integer(!i),
            ExpDesc::Nil | ExpDesc::Boolean(_) | ExpDesc::Float(_) | ExpDesc::String(_) => panic!("invalid ~ operator"),
            desc => ExpDesc::UnaryOp(ByteCode::BitNot, self.discharge_any(desc))
        }
    }
    fn unop_len(&mut self) -> ExpDesc {
        match self.exp_unop() {
            ExpDesc::String(s) => ExpDesc::Integer(s.len() as i64),
            ExpDesc::Nil | ExpDesc::Boolean(_) | ExpDesc::Integer(_) | ExpDesc::Float(_) => panic!("invalid ~ operator"),
            desc => ExpDesc::UnaryOp(ByteCode::Len, self.discharge_any(desc))
        }
    }

    fn process_binop(&mut self, binop: Token, left: ExpDesc, right: ExpDesc) -> ExpDesc {
        if let Some(r) = fold_const(&binop, &left, &right) {
            return r;
        }

        match binop {
            Token::Add => self.do_binop(left, right, ByteCode::Add, ByteCode::AddInt, ByteCode::AddConst),
            Token::Sub => self.do_binop(left, right, ByteCode::Sub, ByteCode::SubInt, ByteCode::SubConst),
            Token::Mul => self.do_binop(left, right, ByteCode::Mul, ByteCode::MulInt, ByteCode::MulConst),
            Token::Mod => self.do_binop(left, right, ByteCode::Mod, ByteCode::ModInt, ByteCode::ModConst),
            Token::Idiv => self.do_binop(left, right, ByteCode::Idiv, ByteCode::IdivInt, ByteCode::IdivConst),
            Token::Div => self.do_binop(left, right, ByteCode::Div, ByteCode::DivInt, ByteCode::DivConst),
            Token::Pow => self.do_binop(left, right, ByteCode::Pow, ByteCode::PowInt, ByteCode::PowConst),
            Token::BitAnd => self.do_binop(left, right, ByteCode::BitAnd, ByteCode::BitAndInt, ByteCode::BitAndConst),
            Token::BitNot => self.do_binop(left, right, ByteCode::BitXor, ByteCode::BitXorInt, ByteCode::BitXorConst),
            Token::BitOr  => self.do_binop(left, right, ByteCode::BitOr, ByteCode::BitOrInt, ByteCode::BitOrConst),
            Token::ShiftL => self.do_binop(left, right, ByteCode::ShiftL, ByteCode::ShiftLInt, ByteCode::ShiftLConst),
            Token::ShiftR => self.do_binop(left, right, ByteCode::ShiftR, ByteCode::ShiftRInt, ByteCode::ShiftRConst),
            Token::Concat => self.do_binop(left, right, ByteCode::Concat, ByteCode::ConcatInt, ByteCode::ConcatConst),
            _ => panic!("impossible"),
        }
    }

// ANCHOR: do_binop
    fn do_binop(&mut self, mut left: ExpDesc, mut right: ExpDesc, opr: fn(u8,u8,u8)->ByteCode,
            opi: fn(u8,u8,u8)->ByteCode, opk: fn(u8,u8,u8)->ByteCode) -> ExpDesc {

        if opr == ByteCode::Add || opr == ByteCode::Mul { // commutative
            if matches!(left, ExpDesc::Integer(_) | ExpDesc::Float(_)) {
                // swap the left-const-operand to right, in order to use opi/opk
                (left, right) = (right, left);
            }
        }

        let left = self.discharge_any(left);

        let (op, right) = match right {
            ExpDesc::Integer(i) =>
                if let Ok(i) = u8::try_from(i) {
                    (opi, i as usize)
                } else {
                    (opk, self.add_const(i))
                }
            ExpDesc::Float(f) => (opk, self.add_const(f)),
            _ => (opr, self.discharge_any(right)),
        };

        ExpDesc::BinaryOp(op, left, right)
    }
// ANCHOR_END: do_binop

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

// ANCHOR: discharge_helper
    // discharge @desc into the top of stack, if need
    fn discharge_any(&mut self, desc: ExpDesc) -> usize {
        self.discharge_if_need(self.sp, desc)
    }

    // discharge @desc into @dst, if need
    fn discharge_if_need(&mut self, dst: usize, desc: ExpDesc) -> usize {
        if let ExpDesc::Local(i) = desc {
            i // no need
        } else {
            self.discharge(dst, desc);
            dst
        }
    }
// ANCHOR_END: discharge_helper

    // discharge @desc into @dst, and update self.sp=dst+1
    fn discharge(&mut self, dst: usize, desc: ExpDesc) {
        let code = match desc {
            ExpDesc::Nil => ByteCode::LoadNil(dst as u8, 1),
            ExpDesc::Boolean(b) => ByteCode::LoadBool(dst as u8, b),
            ExpDesc::Integer(i) =>
                if let Ok(i) = i16::try_from(i) {
                    ByteCode::LoadInt(dst as u8, i)
                } else {
                    ByteCode::LoadConst(dst as u8, self.add_const(i) as u16)
                }
            ExpDesc::Float(f) => ByteCode::LoadConst(dst as u8, self.add_const(f) as u16),
            ExpDesc::String(s) => ByteCode::LoadConst(dst as u8, self.add_const(s) as u16),
            ExpDesc::Local(src) =>
                if dst != src {
                    ByteCode::Move(dst as u8, src as u8)
                } else {
                    return;
                }
            ExpDesc::Global(iname) => ByteCode::GetGlobal(dst as u8, iname as u8),
            ExpDesc::Index(itable, ikey) => ByteCode::GetTable(dst as u8, itable as u8, ikey as u8),
            ExpDesc::IndexField(itable, ikey) => ByteCode::GetField(dst as u8, itable as u8, ikey as u8),
            ExpDesc::IndexInt(itable, ikey) => ByteCode::GetInt(dst as u8, itable as u8, ikey),
            ExpDesc::Call => todo!("discharge Call"),
            ExpDesc::UnaryOp(op, i) => op(dst as u8, i as u8),
            ExpDesc::BinaryOp(op, left, right) => op(dst as u8, left as u8, right as u8),
        };
        self.byte_codes.push(code);
        self.sp = dst + 1;
    }

// ANCHOR: discharge_const
    // for constant types, add @desc to constants;
    // otherwise, discharge @desc into the top of stack
    fn discharge_const(&mut self, desc: ExpDesc) -> ConstStack {
        match desc {
            // add const
            ExpDesc::Nil => ConstStack::Const(self.add_const(())),
            ExpDesc::Boolean(b) => ConstStack::Const(self.add_const(b)),
            ExpDesc::Integer(i) => ConstStack::Const(self.add_const(i)),
            ExpDesc::Float(f) => ConstStack::Const(self.add_const(f)),
            ExpDesc::String(s) => ConstStack::Const(self.add_const(s)),

            // discharge to stack
            _ => ConstStack::Stack(self.discharge_any(desc)),
        }
    }
// ANCHOR_END: discharge_const

// ANCHOR: table_constructor
    fn table_constructor(&mut self) -> ExpDesc {
        let table = self.sp;
        self.sp += 1;

        let inew = self.byte_codes.len();
        self.byte_codes.push(ByteCode::NewTable(table as u8, 0, 0));

        enum TableEntry {
            Map((fn(u8,u8,u8)->ByteCode, fn(u8,u8,u8)->ByteCode, usize)),
            Array(ExpDesc),
        }

        let mut narray = 0;
        let mut nmap = 0;
        loop {
            let sp0 = self.sp;

            // parse entry of map or array?
            let entry = match self.lex.peek() {
                Token::CurlyR => { // `}`
                    self.lex.next();
                    break;
                }
                Token::SqurL => { // `[` exp `]` `=` exp
                    self.lex.next();

                    let key = self.exp(); // key
                    self.lex.expect(Token::SqurR); // `]`
                    self.lex.expect(Token::Assign); // `=`

                    TableEntry::Map(match key {
                        ExpDesc::Local(i) => (ByteCode::SetTable, ByteCode::SetTableConst, i),
                        ExpDesc::String(s) => (ByteCode::SetField, ByteCode::SetFieldConst, self.add_const(s)),
                        ExpDesc::Integer(i) if u8::try_from(i).is_ok() => (ByteCode::SetInt, ByteCode::SetIntConst, i as usize),
                        ExpDesc::Nil => panic!("nil can not be table key"),
                        ExpDesc::Float(f) if f.is_nan() => panic!("NaN can not be table key"),
                        _ => (ByteCode::SetTable, ByteCode::SetTableConst, self.discharge_any(key)),
                    })
                }
                Token::Name(_) => {
                    let name = self.read_name();
                    if self.lex.peek() == &Token::Assign { // Name `=` exp
                        self.lex.next();
                        TableEntry::Map((ByteCode::SetField, ByteCode::SetFieldConst, self.add_const(name)))
                    } else { // Name
                        TableEntry::Array(self.exp_with_ahead(Token::Name(name)))
                    }
                }
                _ => { // exp
                    TableEntry::Array(self.exp())
                }
            };

            // insert the entry into table
            match entry {
                TableEntry::Map((op, opk, key)) => {
                    let value = self.exp();
                    let code = match self.discharge_const(value) {
                        ConstStack::Const(i) => opk(table as u8, key as u8, i as u8),
                        ConstStack::Stack(i) => op(table as u8, key as u8, i as u8),
                    };
                    self.byte_codes.push(code);

                    nmap += 1;
                    self.sp = sp0;
                }
                TableEntry::Array(desc) => {
                    self.discharge(sp0, desc);

                    narray += 1;
                    if narray % 2 == 50 { // reset the array members every 50
                        self.byte_codes.push(ByteCode::SetList(table as u8, 50));
                        self.sp = table + 1;
                    }
                }
            }

            // any more entry?
            match self.lex.next() {
                Token::SemiColon | Token::Comma => (), // yes
                Token::CurlyR => break, // no
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

    fn read_name(&mut self) -> String {
        if let Token::Name(name) = self.lex.next() {
            name
        } else {
            panic!("expect name");
        }
    }
}

// ANCHOR: binop_pri
fn binop_pri(binop: &Token) -> (i32, i32) {
    match binop {
        Token::Pow => (14, 13), // right associative
        Token::Mul | Token::Mod | Token::Div | Token::Idiv => (11, 11),
        Token::Add | Token::Sub => (10, 10),
        Token::Concat => (9, 8), // right associative
        Token::ShiftL | Token::ShiftR => (7, 7),
        Token::BitAnd => (6, 6),
        Token::BitNot => (5, 5),
        Token::BitOr => (4, 4),
        Token::Equal | Token::NotEq | Token::Less | Token::Greater | Token::LesEq | Token::GreEq => (3, 3),
        Token::And => (2, 2),
        Token::Or => (1, 1),
        _ => (-1, -1)
    }
}
// ANCHOR_END: binop_pri

fn fold_const(binop: &Token, left: &ExpDesc, right: &ExpDesc) -> Option<ExpDesc> {
    match binop {
        Token::Add => do_fold_const(left, right, |a,b|a+b, |a,b|a+b),
        Token::Sub => do_fold_const(left, right, |a,b|a-b, |a,b|a-b),
        Token::Mul => do_fold_const(left, right, |a,b|a*b, |a,b|a*b),
        Token::Mod => do_fold_const(left, right, |a,b|a%b, |a,b|a%b),
        Token::Idiv => do_fold_const(left, right, |a,b|a/b, |a,b|a/b),

        Token::Div => do_fold_const_float(left, right, |a,b|a/b),
        Token::Pow => do_fold_const_float(left, right, |a,b|a.powf(b)),

        Token::BitAnd => do_fold_const_int(left, right, |a,b|a&b),
        Token::BitNot => do_fold_const_int(left, right, |a,b|a^b),
        Token::BitOr  => do_fold_const_int(left, right, |a,b|a|b),
        Token::ShiftL => do_fold_const_int(left, right, |a,b|a<<b),
        Token::ShiftR => do_fold_const_int(left, right, |a,b|a>>b),

        Token::Concat => {
            if let (ExpDesc::String(s1), ExpDesc::String(s2)) = (left, right) {
                Some(ExpDesc::String([s1.as_slice(), s2.as_slice()].concat()))
            } else {
                None
            }
        }
        _ => panic!("impossible"),
    }
}

fn do_fold_const(left: &ExpDesc, right: &ExpDesc, arith_i: fn(i64,i64)->i64, arith_f: fn(f64,f64)->f64) -> Option<ExpDesc> {
    match (left, right) {
        (ExpDesc::Integer(i1), ExpDesc::Integer(i2)) => Some(ExpDesc::Integer(arith_i(*i1, *i2))),
        (ExpDesc::Float(f1), ExpDesc::Float(f2)) => Some(ExpDesc::Float(arith_f(*f1, *f2))),
        (ExpDesc::Float(f1), ExpDesc::Integer(i2)) => Some(ExpDesc::Float(arith_f(*f1, *i2 as f64))),
        (ExpDesc::Integer(i1), ExpDesc::Float(f2)) => Some(ExpDesc::Float(arith_f(*i1 as f64, *f2))),
        (_, _) => None,
    }
}

fn do_fold_const_int(left: &ExpDesc, right: &ExpDesc, arith_i: fn(i64,i64)->i64) -> Option<ExpDesc> {
    let (i1, i2) = match (left, right) {
        (&ExpDesc::Integer(i1), &ExpDesc::Integer(i2)) => (i1, i2),
        (&ExpDesc::Float(f1), &ExpDesc::Float(f2)) => (ftoi(f1).unwrap(), ftoi(f2).unwrap()),
        (&ExpDesc::Float(f1), &ExpDesc::Integer(i2)) => (ftoi(f1).unwrap(), i2),
        (&ExpDesc::Integer(i1), &ExpDesc::Float(f2)) => (i1, ftoi(f2).unwrap()),
        (_, _) => return None,
    };
    Some(ExpDesc::Integer(arith_i(i1, i2)))
}

fn do_fold_const_float(left: &ExpDesc, right: &ExpDesc, arith_f: fn(f64,f64)->f64) -> Option<ExpDesc> {
    let (f1, f2) = match (left, right) {
        (&ExpDesc::Integer(i1), &ExpDesc::Integer(i2)) => (i1 as f64, i2 as f64),
        (&ExpDesc::Float(f1), &ExpDesc::Float(f2)) => (f1, f2),
        (&ExpDesc::Float(f1), &ExpDesc::Integer(i2)) => (f1, i2 as f64),
        (&ExpDesc::Integer(i1), &ExpDesc::Float(f2)) => (i1 as f64, f2),
        (_, _) => return None,
    };
    Some(ExpDesc::Float(arith_f(f1, f2)))
}
