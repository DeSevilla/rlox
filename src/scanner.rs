use std::collections::HashMap;
use std::str::Chars;
// use std::iter::Peekable
use lookahead::{Lookahead, lookahead};
use rlox::{Token, TokTy, Literal, RloxError, ErrorType};


pub struct Scanner<'a> {
    current: String,
    line: usize,
    source: Lookahead<Chars<'a>>,
    tokens: Vec<Result<Token, RloxError>>,
    keywords: HashMap<String, TokTy>,
}

impl Scanner<'_> {
    pub fn new(source: &String) -> Scanner {
        Scanner {
            current: "".into(),
            line: 1,
            source: lookahead(source.chars()),
            tokens: Vec::new(),
            keywords: HashMap::from([ 
                ("and".to_owned(), TokTy::And),
                ("class".to_owned(), TokTy::Class),
                ("else".to_owned(), TokTy::Else),
                ("false".to_owned(), TokTy::False),
                ("for".to_owned(), TokTy::For),
                ("fun".to_owned(), TokTy::Fun),
                ("if".to_owned(), TokTy::If),
                ("nil".to_owned(), TokTy::Nil),
                ("or".to_owned(), TokTy::Or),
                ("print".to_owned(), TokTy::Print),
                ("return".to_owned(), TokTy::Return),
                ("super".to_owned(), TokTy::Super),
                ("this".to_owned(), TokTy::This),
                ("true".to_owned(), TokTy::True),
                ("var".to_owned(), TokTy::Var),
                ("while".to_owned(), TokTy::While),
            ])
        }
    }

    fn at_end(&mut self) -> bool {
        self.source.lookahead(0).is_none()
    }

    fn advance(&mut self) -> Option<char> {
        let c = self.source.next()?;
        self.current.push(c);
        Some(c)
    }

    fn lookahead(&mut self, n: usize) -> char {
        match self.source.lookahead(n) {
            Some(c) => *c,
            None => '\0'
        }
    }

    fn match_next(&mut self, expected: char) -> bool {
        if self.lookahead(0) == expected {
            self.advance();
            true
        }
        else {
            false
        }
    }

    fn add_token(&mut self, ty: TokTy) {
        self.add_token_literal(ty, Literal::None)
    }

    fn add_token_literal(&mut self, ty: TokTy, lit: Literal) {
        self.tokens.push(Ok(Token {
            ty: ty,
            lexeme: self.current.clone(),
            literal: lit,
            line: self.line,
        }));
    }

    fn add_error(&mut self, ty: ErrorType, msg: String) {
        self.tokens.push(Err(RloxError { 
            ty: ty,
            line: self.line, 
            info: "".to_string(), 
            msg: msg 
        }))
    }

    fn string(&mut self) {
        let start_line = self.line;
        while self.lookahead(0) != '"' && !self.at_end() {
            if self.lookahead(0) == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.at_end() {
            self.add_error(ErrorType::ParseError, format!("Unterminated string starting at line {start_line}"))
        }
        else {
            self.advance();
            let mut value = self.current.clone();
            value.pop();
            if value.len() > 0 {
                value.remove(0);
            }
            self.add_token_literal(TokTy::String, Literal::Str(value))
        }
    }

    fn number(&mut self) {
        while self.lookahead(0).is_numeric() {
            self.advance();
        }

        if self.lookahead(0) == '.' && self.lookahead(1).is_numeric() {
            self.advance();
            while self.lookahead(0).is_numeric() {
                self.advance();
            }
        }
        let result = self.current.parse();
        match result {
            Ok(lit) => self.add_token_literal(TokTy::Number, Literal::Num(lit)),
            Err(err) => self.add_error(ErrorType::ParseError, format!("Could not parse number from {} ({})", self.current, err))
        }
    }

    fn is_identifier(c: char) -> bool {
        c.is_alphanumeric() || c == '_'
    }

    fn identifier(&mut self) {
        while Self::is_identifier(self.lookahead(0)) {
            self.advance();
        }
        match self.keywords.get(&self.current) {
            Some(ty) => self.add_token(*ty),
            None => self.add_token(TokTy::Identifier)
        }
    }

    fn scan_token(&mut self) -> Option<()> {
        let c = self.advance()?;
        // self.current.push(c);
        match c {
            '(' => self.add_token(TokTy::LeftParen),
            ')' => self.add_token(TokTy::RightParen),
            '{' => self.add_token(TokTy::LeftBrace),
            '}' => self.add_token(TokTy::RightBrace),
            ',' => self.add_token(TokTy::Comma),
            '.' => self.add_token(TokTy::Dot),
            '+' => self.add_token(TokTy::Plus),
            '-' => self.add_token(TokTy::Minus),
            ';' => self.add_token(TokTy::Semicolon),
            '*' => self.add_token(TokTy::Star),
            '!' => if self.match_next('=') { self.add_token(TokTy::BangEqual) } else { self.add_token(TokTy::Bang) },
            '=' => if self.match_next('=') { self.add_token(TokTy::EqualEqual) } else { self.add_token(TokTy::Equal) },
            '<' => if self.match_next('=') { self.add_token(TokTy::LessEqual) } else { self.add_token(TokTy::Less) },
            '>' => if self.match_next('=') { self.add_token(TokTy::GreaterEqual) } else { self.add_token(TokTy::Greater) },
            '/' => if self.match_next('/') {
                    while self.lookahead(0) != '\n' && !self.at_end() {
                        self.advance();
                    }
                } else { self.add_token(TokTy::Slash) },
            ' ' | '\r' | '\t' => (),
            '\n' => { self.line += 1 }
            '"' => self.string(),
            '0'..='9' => self.number(),
            '_' | 'A'..='Z' | 'a'..='z' => self.identifier(),
            x => self.add_error(ErrorType::InvalidChar, x.to_string())
        }
        Some(())
    }

    pub fn scan(mut self) -> Vec<Result<Token, RloxError>> {
        while !self.at_end() {
            self.current = "".into();
            self.scan_token();
        }
        // self.tokens.push(Ok(Token {
        //     ty: TokTy::EOF,
        //     lexeme: "".into(),
        //     line: self.line,
        //     literal: Literal::None,
        // }));
        self.tokens
    }
}