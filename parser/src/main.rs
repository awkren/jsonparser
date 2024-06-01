use std::{collections::HashMap, iter::Peekable, marker::PhantomData, str::Chars};

#[derive(Default, Debug, Clone)]
pub enum JsonValue {
    String(String),
    Number(f64),
    Bool(bool),
    Object(HashMap<String, Id<JsonValue>>),
    List(Vec<Id<JsonValue>>),
    #[default]
    Null,
}

struct Lex<'json> {
    code: Peekable<Chars<'json>>,
}

impl<'json> Lex<'json> {
    fn new(code: &'json str) -> Self {
        let code = code.chars().peekable();
        Self { code }
    }
}

#[derive(Debug)]
enum Token {
    Str(String),
    Num(f64),
    False,
    True,
    Null,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Comma,
    Colon,
    Eof,
    IllegalIdent(String),
}

impl<'json> Lex<'json> {
    fn next_token(&mut self) -> Token {
        if let Some(chr) = self.code.peek() {
            match chr {
                ' ' | '\n' | '\t' | '\r' => {
                    self.code.next();
                    self.next_token()
                }
                '"' => self.str(),
                ':' => self.just(Token::Colon),
                ',' => self.just(Token::Comma),
                '[' => self.just(Token::LBracket),
                ']' => self.just(Token::RBracket),
                '{' => self.just(Token::LBrace),
                '}' => self.just(Token::RBrace),
                n if n.is_ascii_digit() => self.num(),
                _ => self.ident(),
            }
        } else {
            Token::Eof
        }
    }

    fn str(&mut self) -> Token {
        self.code.next();
        let s = self
            .code
            .by_ref()
            .take_while(|c| *c != '"')
            .collect::<String>();
        Token::Str(s)
    }

    fn num(&mut self) -> Token {
        let mut s = String::new();
        let mut is_float = false;
        while let Some(chr) = self.code.peek() {
            match chr {
                '0'..='9' => s.push(self.code.next().unwrap()),
                '.' if !is_float => {
                    s.push(self.code.next().unwrap());
                    is_float = true;
                }
                _ => break,
            }
        }

        if is_float {
            match s.parse::<f64>() {
                Ok(n) => Token::Num(n),
                Err(_) => Token::IllegalIdent(s),
            }
        } else {
            match s.parse::<i64>() {
                Ok(n) => Token::Num(n as f64),
                Err(_) => Token::IllegalIdent(s),
            }
        }
    }

    fn just(&mut self, t: Token) -> Token {
        self.code.next();
        t
    }

    fn ident(&mut self) -> Token {
        let mut s = String::new();
        while let Some(chr) = self.code.peek() {
            if chr.is_alphanumeric() {
                s.push(self.code.next().unwrap());
            } else {
                break;
            }
        }
        if &s == "false" {
            Token::False
        } else if &s == "true" {
            Token::True
        } else if &s == "null" {
            Token::Null
        } else {
            Token::IllegalIdent(s)
        }
    }
}

pub struct Par<'json> {
    cur: Token,
    nxt: Token,
    lex: Lex<'json>,
    mem: Allocator<JsonValue>,
    list: Vec<Id<JsonValue>>,
    obj: HashMap<String, Id<JsonValue>>,
}

impl<'json> Par<'json> {
    fn init(mut lex: Lex<'json>, mem: usize) -> Self {
        let cur = lex.next_token();
        let nxt = lex.next_token();
        let mem = Allocator::make(mem);
        let list = Vec::new();
        let obj = HashMap::new();
        Self {
            cur,
            nxt,
            lex,
            mem,
            list,
            obj,
        }
    }

    fn advance(&mut self) -> Token {
        let mut ret = self.lex.next_token();
        std::mem::swap(&mut self.nxt, &mut self.cur);
        std::mem::swap(&mut self.nxt, &mut ret);
        ret
    }

    pub fn parse(
        src: &'json str,
        mem: usize,
    ) -> Result<Vec<(JsonValue, Allocator<JsonValue>)>, String> {
        let mut parser = Self::init(Lex::new(src), mem);
        let mut results = Vec::new();
        loop {
            let result = parser.go_parse()?;
            results.push((result, parser.mem.clone()));
            if matches!(parser.cur, Token::Eof) {
                break;
            }
            if matches!(parser.cur, Token::Comma) {
                parser.advance();
            }
        }
        Ok(results)
    }

    pub fn go_parse(&mut self) -> Result<JsonValue, String> {
        let tk = match &mut self.cur {
            Token::False => Ok(JsonValue::Bool(false)),
            Token::True => Ok(JsonValue::Bool(true)),
            Token::Null => Ok(JsonValue::Null),
            Token::Str(s) => Ok(JsonValue::String(std::mem::take(s))),
            Token::Num(n) => Ok(JsonValue::Number(std::mem::take(n))),

            Token::LBracket => {
                self.list.clear();
                self.advance();
                loop {
                    if matches!(self.cur, Token::RBracket) {
                        break;
                    }
                    if matches!(self.cur, Token::Comma) {
                        self.advance();
                    }
                    let e = self.go_parse()?;
                    let id = self.mem.alloc(e);
                    self.list.push(id);
                }
                Ok(JsonValue::List(std::mem::take(&mut self.list)))
            }
            Token::RBracket => {
                self.advance();
                if matches!(
                    self.cur,
                    Token::Eof
                        | Token::RBracket
                        | Token::RBrace
                        | Token::Comma
                        | Token::Colon
                        | Token::LBrace
                ) {
                    Ok(JsonValue::List(std::mem::take(&mut self.list)))
                } else {
                    Err("Unexpected ']'.".to_string())
                }
            }

            Token::LBrace => {
                println!("inside lbrace");
                println!("self.cur {:?}", self.cur);
                self.obj.clear();
                self.advance();
                loop {
                    if matches!(self.cur, Token::RBrace) {
                        break;
                    }
                    if matches!(self.cur, Token::Comma) {
                        println!("found comma");
                        println!(" next is {:?}", self.nxt);
                        self.advance();
                    }
                    let key = self.expect_str()?;
                    if matches!(self.cur, Token::Colon) {
                        self.advance();
                    } else {
                        return Err("Expected ':'.".to_string());
                    }
                    let val = self.go_parse()?;
                    let id = self.mem.alloc(val);
                    self.obj.insert(key, id);
                }
                Ok(JsonValue::Object(std::mem::take(&mut self.obj)))
            }

            Token::RBrace => {
                self.advance();
                if matches!(
                    self.cur,
                    Token::Eof | Token::Comma | Token::RBracket | Token::RBrace
                ) {
                    Ok(JsonValue::Object(std::mem::take(&mut self.obj)))
                } else {
                    Err("Unexpected '}}'.".to_string())
                }
            }

            Token::Comma => {
                self.advance();
                if matches!(self.cur, Token::Eof | Token::RBracket | Token::RBrace) {
                    return Err("Unexpected end of input after ','.".to_string());
                }
                Ok(JsonValue::Null)
            }

            Token::Colon => {
                self.advance();
                if let Token::Colon = self.cur {
                    self.advance();
                    Ok(JsonValue::Null)
                } else {
                    Err("Expected ':'.".to_string())
                }
            }

            Token::Eof => return Err("Reached EOF.".to_string()),

            Token::IllegalIdent(s) => {
                if matches!(self.nxt, Token::RBrace) {
                    return Err(format!("Unexpected '{s}' after '}}'."));
                } else {
                    return Err(format!("Unexpected '{s}'."));
                }
            }
        };
        self.advance();
        tk
    }

    fn expect_str(&mut self) -> Result<String, String> {
        let s = match &mut self.cur {
            Token::Str(s) => std::mem::take(s),
            _ => return Err("Key is not a String".to_string()),
        };
        self.advance();
        Ok(s)
    }
}

fn main() {
    let src = include_str!("../file.json");

    match Par::parse(src, 1 << 4) {
        Ok(results) => {
            for (res, mem) in results {
                for el in mem.vec {
                    println!("{:?}", el);
                }
                println!("{res:?}");
            }
        }
        Err(e) => eprintln!("{e}"),
    }
}

pub struct Allocator<T> {
    curr: usize,
    size: usize,
    vec: Vec<T>,
}

impl<T: Clone> Clone for Allocator<T> {
    fn clone(&self) -> Self {
        Allocator {
            curr: self.curr,
            size: self.size,
            vec: self.vec.clone(),
        }
    }
}

#[derive(Debug)]
pub struct Id<T>(usize, PhantomData<T>);

impl<T> Id<T> {
    pub fn id(id: usize) -> Self {
        Self(id, PhantomData)
    }
}

impl<T> Clone for Id<T> {
    fn clone(&self) -> Self {
        Id(self.0, PhantomData)
    }
}

impl<T> Allocator<T> {
    pub fn make(size: usize) -> Self {
        assert!(size > 0);
        let vec = Vec::with_capacity(size - 1);
        Self {
            curr: 0,
            size: size - 1,
            vec,
        }
    }

    pub fn alloc(&mut self, el: T) -> Id<T> {
        let id = self.curr;
        assert!(id < self.size);
        self.vec.push(el);
        self.curr += 1;
        Id(id, PhantomData)
    }

    pub fn fetch(&self, Id(id, ..): Id<T>) -> &T {
        &self.vec[id]
    }
}
