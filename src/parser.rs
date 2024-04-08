use std::vec::IntoIter;
use rlox::{ErrorType, Expr, Literal, RloxError, Stmt, TokTy, Token, Variable};
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
        if self.match_ty([TokTy::For].into_iter()).is_some() {
            self.for_stmt()
        }
        else if self.match_ty([TokTy::If].into_iter()).is_some() {
            self.if_stmt()
        }
        else if self.match_ty([TokTy::Print].into_iter()).is_some() {
            self.print_stmt()
        }
        else if self.match_ty([TokTy::Return].into_iter()).is_some() {
            self.return_stmt()
        }
        else if self.match_ty([TokTy::While].into_iter()).is_some() {
            self.while_stmt()
        }
        else if self.match_ty([TokTy::LeftBrace].into_iter()).is_some() {
            self.block()
        }
        else {
            self.expr_stmt()
        }
    }

    fn return_stmt(&mut self) -> Result<Stmt, RloxError> {
        let keyword = self.previous.clone().expect("Should be impossible to call return_stmt without a previous token");
        let value = if !self.check(TokTy::Semicolon) {
            self.expression()?
        }
        else {
            Literal::None.into()
        };
        self.consume(TokTy::Semicolon, "Expect ';' after return statement")?;
        Ok(Stmt::Return { keyword, value })
    }

    fn for_stmt(&mut self) -> Result<Stmt, RloxError> {
        self.consume(TokTy::LeftParen, "Expect '(' after 'for'")?;
        let initializer = if self.match_ty([TokTy::Semicolon].into_iter()).is_some() {
            None
        }
        else if self.match_ty([TokTy::Var].into_iter()).is_some() {
            Some(self.var_declaration()?)
        }
        else {
            Some(self.expr_stmt()?)
        };
        let condition = if !self.check(TokTy::Semicolon) {
            Some(self.expression()?)
        }
        else {
            None
        };
        self.consume(TokTy::Semicolon, "Expect ';' after loop condition")?;
        let increment = if !self.check(TokTy::RightParen) {
            Some(self.expression()?)
        }
        else {
            None
        };
        self.consume(TokTy::RightParen, "Expect ')' after for clauses")?;
        let mut body = self.statement()?;
        match increment {
            Some(inc) => body = Stmt::Block(vec![body, inc.into()]),
            None => ()
        }
        let cond = condition.unwrap_or(Literal::Bool(true).into());
        body = Stmt::While { cond: cond, body: Box::new(body) };
        match initializer {
            Some(init) => body = Stmt::Block(vec![init.into(), body]),
            None => ()
        }
        Ok(body)
    }

    fn if_stmt(&mut self) -> Result<Stmt, RloxError> {
        self.consume(TokTy::LeftParen, "Expect '(' after if")?;
        let cond = self.expression()?;
        self.consume(TokTy::RightParen, "Expect ')' after condition")?;
        let then_br = Box::new(self.statement()?);
        let else_br = Box::new(
            if self.match_ty([TokTy::Else].into_iter()).is_some() {
                self.statement()?
            } else {
                Stmt::Expression(Expr::Literal(Literal::None))
            }
        );
        Ok(Stmt::If { cond, then_br, else_br })
    }

    fn print_stmt(&mut self) -> Result<Stmt, RloxError> {
        let e = self.expression()?;
        self.consume(TokTy::Semicolon, "Expect ';' after print statement.")?;
        Ok(Stmt::Print(e))
    }

    fn while_stmt(&mut self) -> Result<Stmt, RloxError> {
        self.consume(TokTy::LeftParen, "Expect '(' after while")?;
        let cond = self.expression()?;
        self.consume(TokTy::RightParen, "Expect ')' after while condition")?;
        let body = Box::new(self.statement()?);
        Ok(Stmt::While { cond, body })
    }

    fn block(&mut self) -> Result<Stmt, RloxError> {
        let mut stmts = Vec::new();
        while !(self.check(TokTy::RightBrace)) && self.lookahead(0).is_some() {
            stmts.push(self.declaration()?)
        }
        self.consume(TokTy::RightBrace, "Block must terminate with a '}'")?;
        Ok(Stmt::Block(stmts))
    }

    fn expr_stmt(&mut self) -> Result<Stmt, RloxError> {
        let stmt = self.expression()?;
        self.consume(TokTy::Semicolon, "Expect ';' after expression.")?;
        Ok(Stmt::Expression(stmt))
    }

    fn error(&mut self, tok: Option<Token>, msg: &str) -> RloxError {
        let line = tok.map(|t| t.line).unwrap_or(0);
        return RloxError {
            ty: ErrorType::ParseError,
            line,
            info: "parser".into(),
            msg: msg.to_owned()
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

    fn consume(&mut self, t: TokTy, msg: &str) -> Result<Token, RloxError> {
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

    fn logical_exp<F>(&mut self, mut sub_parser: F, types: impl Clone + Iterator<Item=TokTy>) -> Result<Expr, RloxError>
            where F: FnMut(&mut Self) -> Result<Expr, RloxError> {
        let mut expr = sub_parser(self)?;
        while let Some(op) = self.match_ty(types.clone()).cloned() {
            let right = sub_parser(self)?;
            expr = Expr::Logical { left: Box::new(expr), op: op.clone(), right: Box::new(right) }
        }
        Ok(expr)
    }

    fn assignment(&mut self) -> Result<Expr, RloxError> {
        let expr = self.or()?;
        match self.match_ty([TokTy::Equal].into_iter()) {
            Some(_) => {
                let value = self.assignment()?;
                let var = match &expr {
                    Expr::Variable(var) => Ok(var.clone()),
                    _ => Err(self.error(self.previous.clone(), "Tried to assign to an invalid expression (must be a variable)")),
                }?;
                Ok(Expr::Assign { var, value: Box::new(value) })
            }
            None => Ok(expr),
        }
    }

    fn or(&mut self) -> Result<Expr, RloxError> {
        self.logical_exp(|s| s.and(), [TokTy::Or].into_iter())
    }

    fn and(&mut self) -> Result<Expr, RloxError> {
        self.logical_exp(|s| s.equality(), [TokTy::And].into_iter())
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
            self.call()
        }
    }

    fn call(&mut self) -> Result<Expr, RloxError> {
        let mut expr = self.primary()?;
        loop {
            if self.match_ty([TokTy::LeftParen].into_iter()).is_some() {
                expr = self.finish_call(expr)?;
            }
            else {
                break;
            }
        }
        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr, RloxError> {
        let mut args = Vec::new();
        if !self.check(TokTy::RightParen) {
            args.push(self.expression()?);
            while self.match_ty([TokTy::Comma].into_iter()).is_some() {
                args.push(self.expression()?);
            }
        }
        let paren = self.consume(TokTy::RightParen, "Expect ')' after arguments")?;
        Ok(Expr::Call { callee: Box::new(callee), args, loc: paren.line })
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
                    Ok(Expr::Variable(Variable { name: t.lexeme.clone(), loc: t.line }))
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
        let result = if self.match_ty([TokTy::Fun].into_iter()).is_some() {
            self.function("function")
        }
        else if self.match_ty([TokTy::Var].into_iter()).is_some() {
            self.var_declaration()
        }
        else {
            self.statement()
        };
        match result {
            Ok(s) => Ok(s),
            Err(e) => { self.synchronize(); Err(e)},
        }
    }

    fn function(&mut self, kind: &str) -> Result<Stmt, RloxError> {
        let name = self.consume(TokTy::Identifier, &format!("Expect {kind} name."))?;
        self.consume(TokTy::LeftParen, &format!("Expect '(' after {kind} name"))?;
        let mut params = Vec::new();
        if !self.check(TokTy::RightParen) {
            params.push(self.consume(TokTy::Identifier, "Expect parameter name")?);
            while self.match_ty([TokTy::Comma].into_iter()).is_some() {
                params.push(self.consume(TokTy::Identifier, "Expect parameter name")?);
            }
        }
        self.consume(TokTy::RightParen, "Expect ')' after parameters")?;
        self.consume(TokTy::LeftBrace, &format!("Expect '{{' before {kind} body"))?;
        let body = self.block()?;
        Ok(Stmt::Fun { name, params, body: Box::new(body) })
    }

    fn var_declaration(&mut self) -> Result<Stmt, RloxError> {
        let name = self.consume(TokTy::Identifier, "Expect variable name")?;
        let initializer = if self.match_ty([TokTy::Equal].into_iter()).is_some() {
            self.expression()?
        }
        else {
            Expr::Literal(Literal::None)
        };
        self.consume(TokTy::Semicolon, "Expect ';' after variable declaraton")?;
        Ok(Stmt::Var { name, initializer })
    }

}