use std::collections::HashMap;
use crate::interpreter::Interpreter;
use rlox::{Expr, FunctionType, RloxError, Stmt, Token, Variable};

pub struct Resolver {
    scopes: Vec<HashMap<String, bool>>,
    pub interpreter: Interpreter,
    current_function: FunctionType
}

impl Resolver {
    pub fn new(interpreter: Interpreter) -> Resolver {
        Resolver { scopes: Vec::new(), interpreter, current_function: FunctionType::None }
    }
    
    pub fn resolve_many(&mut self, stmts: &Vec<Stmt>) -> Result<(), RloxError> {
        for stmt in stmts {
            self.resolve(stmt)?;
        }
        Ok(())
    }

    pub fn resolve(&mut self, stmt: &Stmt) -> Result<(), RloxError> {
        match stmt {
            Stmt::Block(stmts) => {
                self.begin_scope();
                // println!("Began block scope");
                for st in stmts { 
                    self.resolve(st)?;
                }
                self.end_scope();
                // println!("Ended block scope");
                Ok(())
            },
            Stmt::Var { name, initializer } => {
                // println!("Resolving Var {} at line {}", name.lexeme, name.line);
                self.declare(name)?;
                self.resolve_expr(initializer)?;
                self.define(name);
                Ok(())
            },
            Stmt::Fun { name, params, body } => {
                // println!("Resolving function {} at line {}", name.lexeme, name.line);
                let enclosing_function = self.current_function;
                self.current_function = FunctionType::Function;
                self.declare(name)?;
                self.define(name);
                self.begin_scope();
                for param in params {
                    self.declare(param)?;
                    self.define(param);
                }
                self.resolve(body)?;
                self.end_scope();
                self.current_function = enclosing_function;
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
            Stmt::Return { value, keyword } => {
                if self.current_function == FunctionType::None {
                    return RloxError::new_err(rlox::ErrorType::BadReturn, keyword.line, "", "Return statement can only exist inside function")
                }
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
                // println!("Resolving variable {v:?}");
                if let Some(scope) = self.scopes.last_mut() {
                    if let Some(false) = scope.get(&v.name) {
                        return RloxError::new_err(rlox::ErrorType::NameError, v.loc, "", "Can't read local variable in its own initializer");
                    }
                }
                self.resolve_local(v)
            },
            Expr::Assign { var, value } => {
                // println!("Resolving assign to {var:?} of value {value:?}");
                self.resolve_expr(value)?;
                self.resolve_local(var)
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

    pub fn resolve_local(&mut self, var: &Variable) -> Result<(), RloxError> {
        for (i, scope) in self.scopes.iter().rev().enumerate() {
            if scope.contains_key(&var.name) {
                self.interpreter.resolve(var, i);
                return Ok(())
            }
        }
        // println!("Resolver says global {var:?}");
        Ok(())
    }

    pub fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn end_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn declare(&mut self, name: &Token) -> Result<(), RloxError> {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(&name.lexeme) {
                return RloxError::new_err(rlox::ErrorType::NameError, name.line, "", "Variable with this name already exists");
            }
            scope.insert(name.lexeme.clone(), false);
        }
        Ok(())
    }

    pub fn define(&mut self, name: &Token) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.lexeme.clone(), true);
        }
    }
}