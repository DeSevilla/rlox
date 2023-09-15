use std::fmt;

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum ErrorType {
    InvalidChar,
    ParseError,
    TypeError,
    ValueError,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct RloxError {
    pub ty: ErrorType,
    pub line: usize, 
    pub info: String,
    pub msg: String
}

impl RloxError {
    fn new_err<T>(ty: ErrorType, line: usize, info: impl Into<String>, msg: impl Into<String>) -> Result<T, RloxError> {
        Err(RloxError { ty: ty, line: line, info: info.into(), msg: msg.into() })
    }
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

impl Literal {
    fn is_truthy(&self) -> bool {
        match self {
            Self::Bool(b) => *b,
            Self::None => false,
            _ => true,
        }
    }

    fn as_num(&self) -> Result<f64, RloxError> {
        match self {
            Self::Num(x) => Ok(*x),
            _ => RloxError::new_err(ErrorType::TypeError, 0, "", format!("{self:?} is not a Num"))
        }
    }

    fn as_bool(&self) -> Result<bool, RloxError> {
        match self {
            Self::Bool(x) => Ok(*x),
            _ => RloxError::new_err(ErrorType::TypeError, 0, "", format!("{self:?} is not a Bool"))
        }
    }

    fn as_string(&self) -> Result<String, RloxError> {
        match self {
            Self::Str(x) => Ok(x.clone()),
            _ => RloxError::new_err(ErrorType::TypeError, 0, "", format!("{self:?} is not a String"))
        }
    }
}

impl ToString for Literal {
    fn to_string(&self) -> String {
        match self {
            Self::None => "nil".to_owned(),
            Self::Bool(b) => b.to_string(),
            Self::Num(x) => {
                let s = x.to_string();
                s.trim_end_matches(".0").to_owned()
            },
            Self::Str(x) => "\"".to_owned() + x + "\"",
        }
    }
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

    pub fn evaluate(self) -> Result<Literal, RloxError> {
        match self {
            Expr::Literal(lit) => Ok(lit),
            Expr::Grouping(e) => e.evaluate(),
            Expr::Unary { op, right } => {
                let right = right.evaluate()?;
                match op.ty {
                    TokTy::Minus => Ok(Literal::Num(-right.as_num()?)), 
                    TokTy::Bang => Ok(Literal::Bool(!right.is_truthy())),
                    _ => RloxError::new_err(ErrorType::ValueError, op.line, "", "Unreachable invalid unary operation")
                }
            }
            Expr::Binary { left, op, right } => {
                let left = left.evaluate()?;
                let right = right.evaluate()?;
                match op.ty {
                    TokTy::Minus => {
                        Ok(Literal::Num(left.as_num()? - right.as_num()?))
                    },
                    TokTy::Star => {
                        Ok(Literal::Num(left.as_num()? * right.as_num()?))
                    },
                    TokTy::Slash => {
                        Ok(Literal::Num(left.as_num()? / right.as_num()?))
                    },
                    TokTy::Plus => {
                        match (left, right) {
                            (Literal::Num(l), Literal::Num(r)) => Ok(Literal::Num(l + r)),
                            (Literal::Str(l), Literal::Str(r)) => Ok(Literal::Str(l + &r)),
                            _ => RloxError::new_err(ErrorType::TypeError, op.line, "", "Arguments to + must be either Num or String"),
                        }
                    },
                    TokTy::Greater => {
                        Ok(Literal::Bool(left.as_num()? > right.as_num()?))
                    },
                    TokTy::GreaterEqual => {
                        Ok(Literal::Bool(left.as_num()? >= right.as_num()?))
                    },
                    TokTy::Less => {
                        Ok(Literal::Bool(left.as_num()? < right.as_num()?))
                    },
                    TokTy::LessEqual => {
                        Ok(Literal::Bool(left.as_num()? <= right.as_num()?))
                    },
                    TokTy::BangEqual => Ok(Literal::Bool(left != right)),
                    TokTy::EqualEqual => Ok(Literal::Bool(left == right)),
                    _ => RloxError::new_err(ErrorType::ValueError, op.line, "", "Unreachable invalid binary operation")
                }
            }
        }
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