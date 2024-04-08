use std::{cell::RefCell, rc::Rc, collections::HashMap, time::{SystemTime, UNIX_EPOCH}};
use rlox::{Environment, ErrorType, Expr, Literal, NativeFunction, RloxError, Stmt, TokTy, Variable};

pub struct Interpreter {
    env: Rc<RefCell<Environment>>,
    global: Rc<RefCell<Environment>>,
    locals: HashMap<Variable, usize>,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        let mut global = Environment::new();
        global.define("clock", Literal::NatFunc { params: Vec::new(), name: NativeFunction::Time });
        let global = global.as_rc();
        let env = Rc::clone(&global);
        Interpreter {
            env,
            global,
            locals: HashMap::new(),
        }
    }

    pub fn clear_env(&mut self) {
        self.global.replace(Environment::new());
        self.env = Rc::clone(&self.global);
    }

    pub fn add_env(&mut self) {
        self.env = Rc::new(RefCell::new(Environment { enclosing: Some(Rc::clone(&self.env)), values: HashMap::new() }))
    }

    pub fn pop_env(&mut self) {
        let outer = self.env.borrow().enclosing.clone();
        match outer {
            Some(e) => { self.env = e },
            None => ()
        }
    }

    pub fn resolve(&mut self, var: &Variable, depth: usize) {
        // println!("Interpreter resolving {var:?} at depth {depth}");
        self.locals.insert(var.clone(), depth);
        // match expr {
        //     Expr::Variable(var) => {
        //         self.locals.insert(var.clone(), depth);
        //         Ok(())
        //     },
        //     _ => RloxError::new_err(ErrorType::TypeError, 0, "", format!("Tried to resolve non-variable expression {expr:?}"))
        // }
    }

    pub fn call(&mut self, func: &Literal, args: Vec<Literal>) -> Result<Literal, RloxError> {
        match func {
            Literal::NatFunc { name, .. } => {
                match name {
                    NativeFunction::Time => {
                        let timespan = SystemTime::now().duration_since(UNIX_EPOCH).expect("time travel").as_millis() as f64;
                        Ok(timespan.into())
                    }
                }
            }
            Literal::Function { params, body, env } => {
                let old_env = Rc::clone(&self.env);
                self.env = Rc::clone(env);
                self.add_env();
                for (name, arg) in params.into_iter().zip(args.into_iter()) {
                    self.env.borrow_mut().define(&name.lexeme, arg.clone());
                }
                let result = self.execute(&body);
                self.env = old_env;
                match result {
                    Ok(()) => Ok(Literal::None.into()),
                    Err(e) => match e.ty {
                        ErrorType::Return(res) => Ok(res),
                        _ => Err(e)
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
            Expr::Variable(var) => { 
                // match self.env.borrow_mut().get(&var.name) {
                //     Some(lit) => Ok(lit.clone()),
                //     None => RloxError::new_err(ErrorType::ValueError, 0, "", format!("Variable {} is undefined", var.name))
                // }
                let distance = self.locals.get(&var);
                let val = match distance {
                    Some(d) => self.env.borrow_mut().get_at(*d, &var.name),
                    None => self.global.borrow_mut().get(&var.name),
                };
                // match &val {
                //     Some(Literal::Function { .. }) => println!(" is a function"),
                //     _ => println!(" with value {:?}", &val),
                // }
                val.ok_or(RloxError { ty: ErrorType::NameError, line: var.loc, info: "".into(), msg: format!("Variable not found: {}", var.name) })
            }
            Expr::Assign { var, value } => {
                // print!("Executing assign to {var:?}");
                let val = self.evaluate(value)?;
                // match &val {
                //     Literal::Function { .. } => println!(" with a function"),
                //     _ => println!(" with value {:?}", &val),
                // }
                let distance = self.locals.get(&var);
                match distance {
                    Some(d) => self.env.borrow_mut().assign_at(*d, var, val.clone())?,
                    None => self.global.borrow_mut().assign(var, val.clone())?,
                };
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
                    Literal::Function { ref params, .. } | Literal::NatFunc { ref params, .. } => if params.len() == args.len() {
                        self.call(&func, args)
                    } else {
                        RloxError::new_err(ErrorType::ArityError, *loc, "", 
                            format!("Function requires {} args but was given {}", params.len(), args.len()))
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
                self.add_env();
                for st in stmts {
                    let res = self.execute(st);
                    if res.is_err() {
                        self.pop_env();
                        // let outer = self.env.borrow().enclosing.clone().expect("Block scope failed to preserve outer environment");
                        // self.env = outer;
                        return res;
                    }
                }
                // let outer = self.env.borrow().enclosing.clone().expect("Block scope failed to preserve outer environment");
                // self.env = outer;
                self.pop_env();
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
            },
            Stmt::Fun { name, params, body } => {
                self.env.borrow_mut().define(&name.lexeme,
                    Literal::Function { params: params.clone(), body: body.clone(), env: Rc::clone(&self.env) }
                );
                Ok(())
            },
            Stmt::Return { value, .. } => {
                let result = self.evaluate(value)?;
                RloxError::new_err(ErrorType::Return(result), 0, "", "")
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
