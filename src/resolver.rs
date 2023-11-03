use std::collections::HashMap;
use crate::interpreter::Interpreter;
use rlox::{Stmt, Token, Expr, RloxError};

pub struct Resolver {
    scopes: Vec<HashMap<String, bool>>,
    interp: Interpreter,
}

impl Resolver {
    pub fn new(interpreter: Interpreter) -> Resolver {
        Resolver { scopes: Vec::new(), interp: interpreter }
    }

    pub fn resolve(&mut self, stmt: &Stmt) -> Result<(), RloxError> {
        match stmt {
            Stmt::Block(stmts) => {
                self.begin_scope();
                for st in stmts { 
                    self.resolve(st)?;
                }
                self.end_scope();
                Ok(())
            },
            Stmt::Var { name, initializer } => {
                self.declare(name);
                self.resolve_expr(initializer)?;
                self.define(name);
                Ok(())
            },
            Stmt::Fun { name, params, body } => {
                self.declare(name);
                self.define(name);
                self.begin_scope();
                for param in params {
                    self.declare(param);
                    self.define(param);
                }
                self.resolve(body)?;
                self.end_scope();
                Ok(())
            },
            Stmt::Expression(expr) => {
                self.resolve_expr(expr)
            },
            Stmt::If { cond, then_br, else_br } => {
                self.resolve_expr(cond)?;
                self.resolve(then_br)?;
                self.resolve(else_br)
            },
            Stmt::Print(expr) => {
                self.resolve_expr(expr)
            },
            Stmt::Return { value, .. } => {
                self.resolve_expr(value)
            },
            Stmt::While { cond, body } => {
                self.resolve_expr(cond)?;
                self.resolve(body)
            },
        }
    }

    pub fn resolve_expr(&mut self, expr: &Expr) -> Result<(), RloxError> {
        match expr {
            Expr::Variable(v) => {
                if let Some(scope) = self.scopes.last_mut() {
                    if let Some(false) = scope.get(&v.lexeme) {
                        return RloxError::new_err(rlox::ErrorType::NameError, v.line, "", "Can't read local variable in its own initializer");
                    }
                }
                self.resolve_local(expr, v)?;
                Ok(())
            },
            Expr::Assign { name, value } => {
                self.resolve_expr(value)?;
                self.resolve_local(expr, name)?;
                Ok(())
            },
            Expr::Binary { left, right, .. } => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)
            },
            Expr::Call { callee, args, .. } => {
                self.resolve_expr(callee)?;
                for arg in args {
                    self.resolve_expr(arg)?;
                }
                Ok(())
            },
            Expr::Grouping(expr) => {
                self.resolve_expr(expr)
            },
            Expr::Literal(_) => Ok(()),
            Expr::Logical { left, right, .. } => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)
            },
            Expr::Unary { right, .. } => {
                self.resolve_expr(right)
            },
        }
    }

    pub fn resolve_local(&mut self, expr: &Expr, name: &Token) -> Result<(), RloxError> {
        for (i, scope) in self.scopes.iter().rev().enumerate() {
            if scope.contains_key(&name.lexeme) {
                self.interp.resolve(expr, i)?;
            }
        }
        Ok(())
    }

    pub fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn end_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn declare(&mut self, name: &Token) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.lexeme.clone(), false);
        }
    }

    pub fn define(&mut self, name: &Token) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.lexeme.clone(), true);
        }
    }
}