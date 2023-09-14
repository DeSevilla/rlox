use std::vec::IntoIter;
use rlox::{Token, TokTy, Expr, RloxError, Literal, ErrorType};
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

    pub fn parse(&mut self) -> Result<Expr, RloxError> {
        self.expression()
    }

    fn error(&mut self, ty: ErrorType, msg: String) -> RloxError {
        let line = self.previous.as_ref().map(|t| t.line).unwrap_or(0);
        return RloxError {
            ty,
            line,
            info: "parser".into(),
            msg
        }
    }

    fn advance(&mut self) -> Option<&Token> {
        let next = self.source.next();
        if let Some(tok) = next {
            self.previous = Some(tok);
        }
        self.previous.as_ref()
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

    fn match_ty(&mut self, types: impl Iterator<Item=TokTy>) -> Option<&Token> {
        for ty in types {
            if self.check(ty) {
                return self.advance();
            }
        }
        None
    }

    fn expression(&mut self) -> Result<Expr, RloxError> {
        self.equality()
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
        if let Some(t) = self.match_ty([TokTy::False, TokTy::True, TokTy::Nil, TokTy::Number, TokTy::String, TokTy::LeftParen].into_iter()) {
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
                _ => Err(self.error(ErrorType::ParseError, "Unreachable token type mismatch".into()))
            }
        }
        else {
            Err(self.error(ErrorType::ParseError, "Expect expression".into()))
        }
    }

}