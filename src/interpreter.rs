use std::{collections::HashMap, cell::RefCell, rc::Rc};
use std::time::{SystemTime, UNIX_EPOCH};
use rlox::{Token, TokTy, Literal, Expr, Stmt, RloxError, ErrorType, NativeFunction};

pub struct Interpreter {
    env: Rc<RefCell<Environment>>,
    global: Rc<RefCell<Environment>>,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        let mut global = Environment::new();
        global.define("clock", Literal::NatFunc { arity: 0, name: NativeFunction::Time });
        let global = global.as_rc();
        let env = Rc::clone(&global);
        Interpreter {
            env,
            global,
        }
    }

    pub fn clear_env(&mut self) {
        self.env.replace(Environment::new());
    }

    pub fn call(&mut self, func: Literal, args: Vec<Literal>) -> Result<Literal, RloxError> {
        match func {
            Literal::NatFunc { name, .. } => {
                match name {
                    NativeFunction::Time => {
                        let timespan = SystemTime::now().duration_since(UNIX_EPOCH).expect("time travel").as_millis() as f64;
                        Ok(timespan.into())
                    }
                }
            }
            _ => RloxError::new_err(ErrorType::TypeError, 0, "", format!("Cannot call {func:?} as function")),
        }
    }
    pub fn evaluate(&mut self, expr: &Expr) -> Result<Literal, RloxError> {
        match expr {
            Expr::Literal(lit) => Ok(lit.clone()),
            Expr::Grouping(e) => self.evaluate(e),
            Expr::Variable(name) => match self.env.borrow_mut().get(&name.lexeme) {
                Some(lit) => Ok(lit.clone()),
                None => RloxError::new_err(ErrorType::ValueError, name.line, "", format!("Variable {} is undefined", name.lexeme))
            }
            Expr::Assign { name, value } => {
                let val = self.evaluate(value)?;
                self.env.borrow_mut().assign(name, val.clone())?;
                Ok(val)
            }
            Expr::Unary { op, right } => {
                let right = self.evaluate(right)?;
                match op.ty {
                    TokTy::Minus => Ok(Literal::Num(-right.as_num()?)), 
                    TokTy::Bang => Ok(Literal::Bool(!right.is_truthy())),
                    _ => unreachable!()
                }
            }
            Expr::Binary { left, op, right } => {
                let left = self.evaluate(left)?;
                let right = self.evaluate(right)?;
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
                    TokTy::Plus => match (left, right) {
                        (Literal::Num(l), Literal::Num(r)) => Ok(Literal::Num(l + r)),
                        (Literal::Str(l), Literal::Str(r)) => Ok(Literal::Str(l + &r)),
                        _ => RloxError::new_err(ErrorType::TypeError, op.line, "", "Arguments to + must be either Num or String and match types"),
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
                    _ => unreachable!()
                }
            }
            Expr::Logical { left, op, right } => {
                let left = self.evaluate(left)?;
                match op.ty {
                    TokTy::And => if left.is_truthy() { self.evaluate(right) } else { Ok(left) }
                    TokTy::Or => if !left.is_truthy() { self.evaluate(right) } else { Ok(left) }
                    _ => unreachable!()
                }
            }
            Expr::Call { callee, args, loc } => {
                let args: Result<Vec<_>, _> = args.into_iter().map(|x| self.evaluate(x)).collect();
                let args = args?;
                let func = self.evaluate(callee)?;
                // if !callee.callable() {
                //     return ;
                // }
                match func {
                    Literal::Function { arity, .. } | Literal::NatFunc { arity, .. } => if arity == args.len() {
                        self.call(func, args)
                    } else {
                        RloxError::new_err(ErrorType::ArityError, *loc, "", format!("Function requires {} args but was given {}", arity, args.len()))
                    },
                    _ => RloxError::new_err(ErrorType::ValueError, *loc, "", "Tried to call a non-function")
                }
            }
        }
    }

    pub fn execute(&mut self, stmt: &Stmt) -> Result<(), RloxError> {
        match stmt {
            Stmt::Expression(e) => self.evaluate(e).map(|_| ()),
            Stmt::Print(e) => { println!("{}", self.evaluate(e)?.to_string()); Ok(()) }
            Stmt::Var { name, initializer } => {
                let value = self.evaluate(initializer)?;
                self.env.borrow_mut().define(&name.lexeme, value);
                Ok(())
            },
            Stmt::Block(stmts) => {
                self.env = Environment::wrapped_new(Rc::clone(&self.env));
                for st in stmts {
                    let res = self.execute(st);
                    if res.is_err() {
                        let outer = self.env.borrow().enclosing.clone().expect("Block scope failed to preserve outer environment");
                        self.env = outer;
                        return res;
                    }
                }
                let outer = self.env.borrow().enclosing.clone().expect("Block scope failed to preserve outer environment");
                self.env = outer;
                Ok(())
            },
            Stmt::If { cond, then_br, else_br } => {
                if self.evaluate(cond)?.is_truthy() {
                    self.execute(then_br)
                }
                else {
                    self.execute(else_br)
                }
            },
            Stmt::While { cond, body } => {
                while self.evaluate(cond)?.is_truthy() {
                    self.execute(body)?;
                }
                Ok(())
            }
        }
    }

    pub fn interpret(&mut self, statements: Vec<Stmt>) {
        for stmt in statements.into_iter() {
            let result = self.execute(&stmt);
            match result {
                Ok(_) => (),
                Err(e) => { println!("Runtime error: {e:?}"); return }
            }
        }
    }
}

#[derive(Clone)]
pub struct Environment {
    enclosing: Option<Rc<RefCell<Environment>>>,
    values: HashMap<String, Literal>
}


impl Environment {
    pub fn new() -> Environment {
        Environment { enclosing: None, values: HashMap::new() }
    }

    pub fn as_rc(self) -> Rc<RefCell<Environment>> {
        Rc::new(RefCell::new(self))
    }

    pub fn add_new(self) -> Environment {
        Environment { enclosing: Some(Rc::new(RefCell::new(self))), values: HashMap::new() }
    }

    pub fn wrapped_new(env: Rc<RefCell<Environment>>) -> Rc<RefCell<Environment>> {
        Rc::new(RefCell::new(Environment { enclosing: Some(env), values: HashMap::new() }))
    }

    // pub fn wrapped_pop(env: &Rc<RefCell<Environment>>) -> Option<Rc<RefCell<Environment>>> {
    //     match &env.borrow().enclosing {
    //         Some(e) => Some(Rc::clone(e)),
    //         None => None
    //     }
    // }

    // fn pop(self) -> Rc<RefCell<Environment>> {
    //     self.enclosing.expect("Can only call pop on an RC with an enclosing environment")
    // }

    // pub fn globals(&mut self) -> &mut Environment {
    //     // if self.enclosing.is_some() {
    //     //     self.enclosing.unwrap().globals()
    //     // }
    //     // else {
    //     //     &self
    //     // }
    //     match self.enclosing {
    //         Some(mut e) => e.borrow_mut().globals(),
    //         None => self
    //     }
    // }

    pub fn define(&mut self, name: &str, value: Literal) {
        self.values.insert(name.to_owned(), value);
    }

    pub fn assign(&mut self, name: &Token, value: Literal) -> Result<(), RloxError> {
        match self.values.get(&name.lexeme) {
            Some(_) => match self.values.insert(name.lexeme.clone(), value) {
                Some(_) => Ok(()),
                None => RloxError::new_err(ErrorType::ValueError, name.line,  "", "Failed to insert variable")
            },
            None => match &self.enclosing {
                Some(e) => e.borrow_mut().assign(name, value),
                None => RloxError::new_err(ErrorType::ValueError, name.line, "", format!("Undefined variable {}", name.lexeme))
            },
        }
    }

    pub fn get(&self, name: &String) -> Option<Literal> {
        let result = self.values.get(name);
        // let enclosing = self.enclosing
        if result.is_some() {
            return result.cloned();
        }
        else {
            return self.enclosing.as_ref()?.borrow().get(name);
        }
    }
}
