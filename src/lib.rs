use std::fmt;

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum ErrorType {
    InvalidChar,
    ParseError,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct RloxError {
    pub ty: ErrorType,
    pub line: usize, 
    pub info: String,
    pub msg: String
}

impl fmt::Display for RloxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self { ty, line, info, msg } => write!(f, "[{line}] {ty:?} Error {info}: {msg}")
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum TokTy {
    LeftParen, RightParen, LeftBrace, RightBrace,
    Comma, Dot, Minus, Plus, Semicolon, Slash, Star,

    Bang, BangEqual, Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,

    Identifier, String, Number,

    And, Class, Else, False, Fun, For, If, Nil, Or,
    Print, Return, Super, This, True, Var, While,

    EOF
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Num(f64),
    Str(String),
    Bool(bool),
    None
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub ty: TokTy,
    pub lexeme: String,
    pub literal: Literal,
    pub line: usize,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} {} {:?} ({})", self.ty, self.lexeme, self.literal, self.line)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Binary { left: Box<Expr>, op: Token, right: Box<Expr> },
    Unary { op: Token, right: Box<Expr> },
    Grouping(Box<Expr>),
    Literal(Literal),
}

impl Expr {
    pub fn parenthesize(&self) -> String {
        let mut output = "(".to_owned();
        output = match self {
            Expr::Binary { left, op, right } => output + &op.lexeme + " " + &left.parenthesize() + " " + &right.parenthesize() + ")",
            Expr::Unary { op, right } => output + &op.lexeme + " " + &right.parenthesize() + ")",
            Expr::Grouping(ex) => output + "group " + &ex.parenthesize() + ")",
            Expr::Literal(lit) => match lit {
                Literal::Num(n) => n.to_string(),
                Literal::Str(s) => { let mut s = s.clone(); s.insert(0, '"'); s.push('"'); s },
                Literal::Bool(b) => b.to_string(),
                Literal::None => "nil".to_owned(),
            },
            // _ => output,
        };
        output
    }

    pub fn test() -> String {
        let tree = Expr::Binary { 
            left: Box::new(Expr::Unary { 
                op: Token { ty: TokTy::Minus, lexeme: "-".into(), literal: Literal::None, line: 1 }, 
                right: Box::new(Expr::Literal(Literal::Num(123.0))) 
            }), 
            op: Token { ty: TokTy::Star, lexeme: "*".into(), literal: Literal::None, line: 1 }, 
            right: Box::new(Expr::Grouping(
                Box::new(Expr::Literal(Literal::Num(45.67)))
            ))
        };
        tree.parenthesize()
    }
}