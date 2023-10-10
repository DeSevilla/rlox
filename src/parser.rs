use std::vec::IntoIter;
use rlox::{Token, TokTy, Expr, RloxError, Literal, ErrorType, Stmt};
use lookahead::{Lookahead, lookahead};

pub struct Parser {
    source: Lookahead<IntoIter<Token>>,
    previous: Option<Token>,
}

impl Parser {
    pub fn new(source: Vec<Token>) -> Parser {
        Parser {
            source: lookahead(source.into_iter()),
            previous: None,
        }
    }

    pub fn parse(&mut self) -> Vec<Result<Stmt, RloxError>> {
        let mut statements = Vec::new();
        while self.lookahead(0).is_some() {
            let new_stmt = self.declaration();
            statements.push(new_stmt);
        }
        statements
    }

    fn statement(&mut self) -> Result<Stmt, RloxError> {
        if self.match_ty([TokTy::Print].into_iter()).is_some() {
            self.print_stmt()
        }
        else {
            self.expr_stmt()
        }
    }

    fn print_stmt(&mut self) -> Result<Stmt, RloxError> {
        let e = self.expression()?;
        self.consume(TokTy::Semicolon, "Expect ';' after print statement.".to_owned())?;
        Ok(Stmt::Print(e))
    }

    fn expr_stmt(&mut self) -> Result<Stmt, RloxError> {
        let stmt = self.expression()?;
        self.consume(TokTy::Semicolon, "Expect ';' after expression.".to_owned())?;
        Ok(Stmt::Expression(stmt))
    }

    fn error(&mut self, tok: Option<Token>, msg: String) -> RloxError {
        let line = tok.map(|t| t.line).unwrap_or(0);
        return RloxError {
            ty: ErrorType::ParseError,
            line,
            info: "parser".into(),
            msg
        }
    }

    fn advance(&mut self) -> Option<&Token> {
        let next = self.source.next();
        match next {
            Some(tok) => {  self.previous = Some(tok); self.previous.as_ref() },
            None => None
        }
    }

    fn lookahead(&mut self, n: usize) -> Option<&Token> {
        self.source.lookahead(n)
    }

    fn check(&mut self, t: TokTy) -> bool {
        match self.lookahead(0) {
            Some(tok) => tok.ty == t,
            None => false
        }
    }

    fn consume(&mut self, t: TokTy, msg: String) -> Result<Token, RloxError> {
        if self.check(t) {
            match self.advance() {
                Some(tok) => Ok(tok.clone()),
                None => unreachable!()
            }
        }
        else {
            let tok = self.lookahead(0).cloned();
            Err(self.error(tok, msg))
        }
    }

    fn match_ty(&mut self, types: impl Iterator<Item=TokTy>) -> Option<&Token> {
        for ty in types {
            if self.check(ty) {
                return self.advance();
            }
        }
        None
    }

    fn synchronize(&mut self) {
        while let Some(tok) = self.advance() {
            println!("Synchronizing... {:?}", tok);
            if tok.ty == TokTy::Semicolon {
                return
            }
            else {
                match self.lookahead(0) {
                    Some(t) => match t.ty {
                        TokTy::Class | TokTy::Fun | TokTy::Var | TokTy::For 
                            | TokTy::If | TokTy::While | TokTy::Print | TokTy::Return => return,
                        _ => (),
                    }
                    None => (),
                }
            }
        }
    }

    fn expression(&mut self) -> Result<Expr, RloxError> {
        self.assignment()
    }

    fn bin_exp<F>(&mut self, mut sub_parser: F, types: impl Clone + Iterator<Item=TokTy>) -> Result<Expr, RloxError>
            where F: FnMut(&mut Self) -> Result<Expr, RloxError> {
        let mut expr = sub_parser(self)?;
        while let Some(op) = self.match_ty(types.clone()).cloned() {
            let right = sub_parser(self)?;
            expr = Expr::Binary { left: Box::new(expr), op: op.clone(), right: Box::new(right) }
        }
        Ok(expr)
    }

    fn assignment(&mut self) -> Result<Expr, RloxError> {
        let expr = self.equality()?;
        match self.match_ty([TokTy::Equal].into_iter()) {
            Some(_) => {
                let value = self.assignment()?;
                let name = match &expr {
                    Expr::Variable(name) => Ok(name.clone()),
                    _ => Err(self.error(self.previous.clone(), "Tried to assign to an invalid expression (must be a variable)".to_owned())),
                }?;
                Ok(Expr::Assign { name, value: Box::new(value) })
            }
            None => Ok(expr),
        }
    }

    fn equality(&mut self) -> Result<Expr, RloxError> {
        self.bin_exp(|s| s.comparison(), [TokTy::EqualEqual, TokTy::BangEqual].into_iter())
    }

    fn comparison(&mut self) -> Result<Expr, RloxError> {
        self.bin_exp(|s| s.term(), [TokTy::Greater, TokTy::GreaterEqual, TokTy::Less, TokTy::LessEqual].into_iter())
    }

    fn term(&mut self) -> Result<Expr, RloxError> {
        self.bin_exp(|s| s.factor(), [TokTy::Plus, TokTy::Minus].into_iter())
    }

    fn factor(&mut self) -> Result<Expr, RloxError> {
        self.bin_exp(|s| s.unary(), [TokTy::Star, TokTy::Slash].into_iter())
    }

    fn unary(&mut self) -> Result<Expr, RloxError> {
        if let Some(op) = self.match_ty([TokTy::Bang, TokTy::Minus].into_iter()).cloned() {
            let right = self.unary()?;
            Ok(Expr::Unary { op, right: Box::new(right) })
        }
        else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Result<Expr, RloxError> {
        if let Some(t) = self.match_ty([TokTy::False, TokTy::True, TokTy::Nil, TokTy::Number, TokTy::String, TokTy::LeftParen, TokTy::Identifier].into_iter()) {
            match t.ty {
                TokTy::False => Ok(Expr::Literal(Literal::Bool(false))),
                TokTy::True => Ok(Expr::Literal(Literal::Bool(true))),
                TokTy::Nil => Ok(Expr::Literal(Literal::None)),
                TokTy::Number | TokTy::String => Ok(Expr::Literal(t.literal.clone())),
                TokTy::LeftParen => {
                    let e = self.expression()?;
                    self.match_ty([TokTy::RightParen].into_iter());
                    Ok(Expr::Grouping(Box::new(e)))
                }
                TokTy::Identifier => {
                    Ok(Expr::Variable(t.clone()))
                }
                _ => {let tok = self.lookahead(0).cloned(); Err(self.error(tok, "Unreachable token type mismatch".into())) }
            }
        }
        else {
            let tok = self.lookahead(0).cloned();
            Err(self.error(tok, "Expect expression".into()))
        }
    }

    fn declaration(&mut self) -> Result<Stmt, RloxError> {
        let result = self.match_ty([TokTy::Var].into_iter());
        let result = match result {
            Some(_) => self.var_declaration(),
            None => self.statement(),
        };
        match result {
            Ok(s) => Ok(s),
            Err(e) => { self.synchronize(); Err(e)},
        }
    }

    fn var_declaration(&mut self) -> Result<Stmt, RloxError> {
        let name = self.consume(TokTy::Identifier, "Expect variable name".to_owned())?;
        let initializer = if self.match_ty([TokTy::Equal].into_iter()).is_some() {
            self.expression()?
        }
        else {
            Expr::Literal(Literal::None)
        };
        self.consume(TokTy::Semicolon, "Expect ';' after variable declaraton".to_owned())?;
        Ok(Stmt::Var { name, initializer })
    }

}