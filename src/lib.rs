use std::{cell::RefCell, collections::HashMap, fmt, rc::Rc};

#[derive(Debug, Clone)]
pub enum ErrorType {
    InvalidChar,
    ParseError,
    TypeError,
    ValueError,
    ArityError,
    NameError,
    Return(Literal)
}

#[derive(Debug, Clone)]
pub struct RloxError {
    pub ty: ErrorType,
    pub line: usize, 
    pub info: String,
    pub msg: String
}

impl RloxError {
    pub fn new_err<T>(ty: ErrorType, line: usize, info: impl Into<String>, msg: impl Into<String>) -> Result<T, RloxError> {
        Err(RloxError { ty, line, info: info.into(), msg: msg.into() })
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

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum NativeFunction {
    Time
}


#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Num(f64),
    Str(String),
    Bool(bool),
    Function { params: Vec<Token>, body: Box<Stmt>, env: Rc<RefCell<Environment>> },
    NatFunc { params: Vec<Token>, name: NativeFunction },
    None
}

impl Literal {
    pub fn is_truthy(&self) -> bool {
        match self {
            Self::Bool(b) => *b,
            Self::None => false,
            _ => true,
        }
    }

    pub fn as_num(&self) -> Result<f64, RloxError> {
        match self {
            Self::Num(x) => Ok(*x),
            _ => RloxError::new_err(ErrorType::TypeError, 0, "", format!("{self:?} is not a Num"))
        }
    }

    pub fn as_bool(&self) -> Result<bool, RloxError> {
        match self {
            Self::Bool(x) => Ok(*x),
            _ => RloxError::new_err(ErrorType::TypeError, 0, "", format!("{self:?} is not a Bool"))
        }
    }

    pub fn as_string(&self) -> Result<String, RloxError> {
        match self {
            Self::Str(x) => Ok(x.clone()),
            _ => RloxError::new_err(ErrorType::TypeError, 0, "", format!("{self:?} is not a String"))
        }
    }

    pub fn callable(&self) -> bool {
        match self {
            Self::Function { .. } | Self::NatFunc { .. } => true,
            _ => false
        }
    }

}

impl From<String> for Literal {
    fn from(value: String) -> Self {
        Self::Str(value)
    }
}

impl From<f64> for Literal {
    fn from(value: f64) -> Self {
        Self::Num(value)
    }
}

impl From<bool> for Literal {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

impl<T> From<Option<T>> for Literal where T: Into<Literal> {
    fn from(value: Option<T>) -> Self {
        match value {
            Some(val) => val.into(),
            None => Self::None
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
            _ => "TODO not representable as string".to_owned(),
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Variable {
    pub name: String,
    pub loc: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Assign { var: Variable, value: Box<Expr> },
    Binary { left: Box<Expr>, op: Token, right: Box<Expr> },
    Logical { left: Box<Expr>, op: Token, right: Box<Expr> },
    Unary { op: Token, right: Box<Expr> },
    Grouping(Box<Expr>),
    Literal(Literal),
    Variable(Variable),
    Call { callee: Box<Expr>, args: Vec<Expr>, loc: usize },
}

impl Expr {
    pub fn parenthesize(&self) -> String {
        let mut output = "(".to_owned();
        output = match self {
            Expr::Binary { left, op, right } => output + &op.lexeme + " " + &left.parenthesize() + " " + &right.parenthesize() + ")",
            Expr::Logical { left, op, right } => output + &op.lexeme + " " + &left.parenthesize() + " " + &right.parenthesize() + ")",
            Expr::Unary { op, right } => output + &op.lexeme + " " + &right.parenthesize() + ")",
            Expr::Grouping(ex) => output + "group " + &ex.parenthesize() + ")",
            Expr::Literal(lit) => match lit {
                Literal::Num(n) => n.to_string(),
                Literal::Str(s) => { let mut s = s.clone(); s.insert(0, '"'); s.push('"'); s },
                Literal::Bool(b) => b.to_string(),
                Literal::None => "nil".to_owned(),
                _ => "TODO not representable".to_owned(),
            },
            Expr::Variable(v) => output + "var " + &v.name + ")",
            Expr::Assign { var, value } => output + &var.name + " = " + &value.parenthesize() + ")",
            Expr::Call { callee, args, loc: _ } => output + &callee.parenthesize() + "(" + 
                &args.iter().map(|x| x.parenthesize()).collect::<Vec<_>>().join(", ") + "))",
            // _ => output + "unimplemented parenthesize",
        };
        output
    }


    pub fn test() -> String {
        let tree = Expr::Binary { 
            left: Box::new(Expr::Unary { 
                op: Token { ty: TokTy::Minus, lexeme: "-".into(), literal: Literal::None, line: 1 }, 
                right: Box::new(Expr::from(123.0))
            }), 
            op: Token { ty: TokTy::Star, lexeme: "*".into(), literal: Literal::None, line: 1 }, 
            right: Box::new(Expr::Grouping(
                Box::new(Expr::from(45.67))
            ))
        };
        tree.parenthesize()
    }
}

impl<T> From<T> for Expr where T: Into<Literal> {
    fn from(value: T) -> Self {
        Self::Literal(value.into())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Expression(Expr),
    Print(Expr),
    Var { name: Token, initializer: Expr },
    Block(Vec<Stmt>),
    If { cond: Expr, then_br: Box<Stmt>, else_br: Box<Stmt> },
    While { cond: Expr, body: Box<Stmt> },
    Fun { name: Token, params: Vec<Token>, body: Box<Stmt> },
    Return { keyword: Token, value: Expr },
}

// impl From<Expr> for Stmt {
//     fn from(value: Expr) -> Self {
//         Self::Expression(value)
//     }
// }

impl<T> From<T> for Stmt where T: Into<Expr> {
    fn from(value: T) -> Self {
        Self::Expression(value.into())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    pub enclosing: Option<Rc<RefCell<Environment>>>,
    pub values: HashMap<String, Literal>
}


impl Environment {
    pub fn new() -> Environment {
        Environment { enclosing: None, values: HashMap::new() }
    }

    pub fn as_rc(self) -> Rc<RefCell<Environment>> {
        Rc::new(RefCell::new(self))
    }

    pub fn define(&mut self, name: &str, value: Literal) {
        self.values.insert(name.to_owned(), value);
    }

    pub fn assign(&mut self, var: &Variable, value: Literal) -> Result<(), RloxError> {
        match self.values.get(&var.name) {
            Some(_) => match self.values.insert(var.name.clone(), value) {
                Some(_) => Ok(()),
                None => RloxError::new_err(ErrorType::ValueError, 0,  "", "Failed to insert variable") // TODO not ideal
            },
            None => match &self.enclosing {
                Some(e) => e.borrow_mut().assign(var, value),
                None => RloxError::new_err(ErrorType::ValueError, 0, "", format!("Undefined variable {}", var.name))
            },
        }
    }

    pub fn assign_at(&mut self, depth: usize, var: &Variable, value: Literal) -> Result<(), RloxError> {
        if depth == 0 {
            self.assign(var, value)
        }
        else {
            self.enclosing.as_ref().expect("Resolver gave wrong depth for assign").borrow_mut().assign_at(depth - 1, var, value)
        }
    }

    pub fn get(&self, name: &String) -> Option<Literal> {
        let result = self.values.get(name);
        // let enclosing = self.enclosing
        if result.is_some() {
            result.cloned()
        }
        else {
            self.enclosing.as_ref()?.borrow_mut().get(name)
        }
    }

    pub fn get_at(&self, depth: usize, name: &String) -> Option<Literal> {
        if depth == 0 {
            self.get(name)
        }
        else {
            self.enclosing.as_ref().expect("Resolver gave wrong depth for get").borrow_mut().get_at(depth - 1, name)
        }
    }
}
