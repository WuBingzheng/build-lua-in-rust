use std::mem;
use std::io::{Read, Bytes};
use std::iter::Peekable;

#[derive(Debug, PartialEq)]
pub enum Token {
    // keywords
    And,    Break,  Do,     Else,   Elseif, End,
    False,  For,    Function, Goto, If,     In,
    Local,  Nil,    Not,    Or,     Repeat, Return,
    Then,   True,   Until,  While,

 // +       -       *       /       %       ^       #
    Add,    Sub,    Mul,    Div,    Mod,    Pow,    Len,
 // &       ~       |       <<      >>      //
    BitAnd, BitNot, BitOr,  ShiftL, ShiftR, Idiv,
 // ==       ~=     <=      >=      <       >        =
    Equal,  NotEq,  LesEq,  GreEq,  Less,   Greater, Assign,
 // (       )       {       }       [       ]       ::
    ParL,   ParR,   CurlyL, CurlyR, SqurL,  SqurR,  DoubColon,
 // ;               :       ,       .       ..      ...
    SemiColon,      Colon,  Comma,  Dot,    Concat, Dots,

    // constant values
    Integer(i64),
    Float(f64),
    String(Vec<u8>),

    // name of variables or table keys
    Name(String),

    // end
    Eos,
}

#[derive(Debug)]
pub struct Lex<R: Read> {
    input: Peekable::<Bytes::<R>>,
    ahead: Token,
}

impl<R: Read> Lex<R> {
    pub fn new(input: R) -> Self {
        Lex {
            input: input.bytes().peekable(),
            ahead: Token::Eos,
        }
    }

    pub fn next(&mut self) -> Token {
        if self.ahead == Token::Eos {
            self.do_next()
        } else {
            mem::replace(&mut self.ahead, Token::Eos)
        }
    }

    pub fn peek(&mut self) -> &Token {
        if self.ahead == Token::Eos {
            self.ahead = self.do_next();
        }
        &self.ahead
    }
//
    pub fn expect(&mut self, t: Token) {
        assert_eq!(self.next(), t);
    }

    fn do_next(&mut self) -> Token {
        if let Some(byt) = self.next_byte() {
            match byt {
                b'\n' | b'\r' | b'\t' | b' ' => self.do_next(),
                b'+' => Token::Add,
                b'*' => Token::Mul,
                b'%' => Token::Mod,
                b'^' => Token::Pow,
                b'#' => Token::Len,
                b'&' => Token::BitAnd,
                b'|' => Token::BitOr,
                b'(' => Token::ParL,
                b')' => Token::ParR,
                b'{' => Token::CurlyL,
                b'}' => Token::CurlyR,
                b'[' => Token::SqurL,
                b']' => Token::SqurR,
                b';' => Token::SemiColon,
                b',' => Token::Comma,
                b'/' => self.check_ahead(b'/', Token::Idiv, Token::Div),
                b'=' => self.check_ahead(b'=', Token::Equal, Token::Assign),
                b'~' => self.check_ahead(b'=', Token::NotEq, Token::BitNot),
                b':' => self.check_ahead(b':', Token::DoubColon, Token::Colon),
                b'<' => self.check_ahead2(b'=', Token::LesEq, b'<', Token::ShiftL, Token::Less),
                b'>' => self.check_ahead2(b'=', Token::GreEq, b'>', Token::ShiftR, Token::Greater),
                b'\'' | b'"' => self.read_string(byt),
                b'.' => match self.peek_byte() {
                    b'.' => {
                        self.next_byte();
                        if self.peek_byte() == b'.' {
                            self.next_byte();
                            Token::Dots
                        } else {
                            Token::Concat
                        }
                    }
                    b'0'..=b'9' => self.read_decimal('.'),
                    _ => Token::Dot,
                }
                b'-' => {
                    if self.peek_byte() == b'-' {
                        self.next_byte();
                        self.read_comment();
                        self.do_next()
                    } else {
                        Token::Sub
                    }
                }
                ch@b'0'..=b'9' => self.read_decimal(ch as char),
                b'A'..=b'Z' | b'a'..=b'z' | b'_' => self.read_name(byt),
                _ => panic!("invalid char {byt}"),
            }
        } else {
            Token::Eos
        }
    }

    fn peek_byte(&mut self) -> u8 {
        match self.input.peek() {
            Some(Ok(byt)) => *byt,
            Some(_) => panic!("lex peek error"),
            None => b'\0', // good for usage
        }
    }
    fn next_byte(&mut self) -> Option<u8> {
        self.input.next().map(|r| r.unwrap())
    }

    fn check_ahead(&mut self, ahead: u8, long: Token, short: Token) -> Token {
        if self.peek_byte() == ahead {
            self.next_byte();
            long
        } else {
            short
        }
    }
    fn check_ahead2(&mut self, ahead1: u8, long1: Token, ahead2: u8, long2: Token, short: Token) -> Token {
        let byt = self.peek_byte();
        if byt == ahead1 {
            self.next_byte();
            long1
        } else if byt == ahead2 {
            self.next_byte();
            long2
        } else {
            short
        }
    }

    fn read_decimal(&mut self, ahead: char) -> Token {
        // TODO heximal
        let mut is_float = ahead == '.';
        let mut buf = String::new();
        buf.push(ahead);
        loop {
            let byt = self.peek_byte();
            match byt {
                b'0' ..= b'9' => buf.push(byt as char),
                b'.' | b'e' | b'E' | b'+' | b'-' => {
                    buf.push(byt as char);
                    is_float = true;
                }
                _ => break,
            }
            self.next_byte();
        }

        if is_float {
            Token::Float(buf.parse::<f64>().unwrap())
        } else {
            Token::Integer(buf.parse::<i64>().unwrap())
        }
    }

    fn read_string(&mut self, quote: u8) -> Token {
        let mut s = Vec::new();
        loop {
            match self.next_byte().expect("unfinished string") {
                b'\n' => panic!("unfinished string"),
                b'\\' => s.push(self.read_escape()),
                byt if byt == quote => break,
                byt => s.push(byt),
            }
        }
        Token::String(s)
    }
    fn read_escape(&mut self) -> u8 {
        match self.next_byte().expect("string escape") {
            b'a' => 0x07,
            b'b' => 0x08,
            b'f' => 0x0c,
            b'v' => 0x0b,
            b'n' => b'\n',
            b'r' => b'\r',
            b't' => b'\t',
            b'\\' => b'\\',
            b'"' => b'"',
            b'\'' => b'\'',
            b'x' => { // format: \xXX
                let n1 = char::to_digit(self.next_byte().unwrap() as char, 16).unwrap();
                let n2 = char::to_digit(self.next_byte().unwrap() as char, 16).unwrap();
                (n1 * 16 + n2) as u8
            }
            ch@b'0'..=b'9' => { // format: \d[d[d]]
                let mut n = char::to_digit(ch as char, 10).unwrap(); // TODO no unwrap
                if let Some(d) = char::to_digit(self.peek_byte() as char, 10) {
                    self.next_byte();
                    n = n * 10 + d;
                    if let Some(d) = char::to_digit(self.peek_byte() as char, 10) {
                        self.next_byte();
                        n = n * 10 + d;
                    }
                }
                u8::try_from(n).expect("decimal escape too large")
            }
            _ => panic!("invalid string escape")
        }
    }

    fn read_name(&mut self, first: u8) -> Token {
        let mut s = String::new();
        s.push(first as char);

        loop {
            let ch = self.peek_byte() as char;
            if ch.is_alphanumeric() || ch == '_' {
                self.next_byte();
                s.push(ch);
            } else {
                break;
            }
        }

        match &s as &str { // TODO optimize by hash
            "and"      => Token::And,
            "break"    => Token::Break,
            "do"       => Token::Do,
            "else"     => Token::Else,
            "elseif"   => Token::Elseif,
            "end"      => Token::End,
            "false"    => Token::False,
            "for"      => Token::For,
            "function" => Token::Function,
            "goto"     => Token::Goto,
            "if"       => Token::If,
            "in"       => Token::In,
            "local"    => Token::Local,
            "nil"      => Token::Nil,
            "not"      => Token::Not,
            "or"       => Token::Or,
            "repeat"   => Token::Repeat,
            "return"   => Token::Return,
            "then"     => Token::Then,
            "true"     => Token::True,
            "until"    => Token::Until,
            "while"    => Token::While,
            _          => Token::Name(s),
        }
    }

    // '--' has been read
    fn read_comment(&mut self) {
        match self.next_byte() {
            None => (),
            Some(b'[') => todo!("long comment"),
            Some(_) => { // line comment
                while let Some(byt) = self.next_byte() {
                    if byt == b'\n' {
                        break;
                    }
                }
            }
        }
    }
}
