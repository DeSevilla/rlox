use std::vec::IntoIter;
use rlox::{Token, TokTy, Expr};
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

    fn advance(&mut self) -> Option<&Token> {
        let tok = self.source.next();
        self.previous = tok;
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

    fn expression(&mut self) -> Expr {
        self.equality()
    }

    fn equality(&mut self) -> Expr {
        let mut expr = self.comparison();
        while let Some(op) = self.match_ty([TokTy::Bang, TokTy::BangEqual].into_iter()).cloned() {
            let right = self.comparison();
            expr = Expr::Binary { left: Box::new(expr), op: op.clone(), right: Box::new(right) }
        }
        expr
    }

    fn comparison(&mut self) -> Expr {
        let mut expr = self.term();
        while let Some(op) = self.match_ty([TokTy::Greater, TokTy::GreaterEqual, TokTy::Less, TokTy::LessEqual].into_iter()).cloned() {
            let right = self.term();
            expr = Expr::Binary { left: Box::new(expr), op: op.clone(), right: Box::new(right) }
        }
        expr
    }

    fn term(&mut self) -> Expr {
        unimplemented!()
    }

}