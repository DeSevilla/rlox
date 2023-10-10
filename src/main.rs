use std::env;
use std::fs;
use std::io;
use rlox::Stmt;
use rlox::{Token, RloxError, Interpreter};
use itertools::Itertools;
pub mod scanner;
pub mod parser;

struct RloxRepl {
    interpreter: Interpreter,
}

impl RloxRepl {
    fn run(&mut self, source: String) {
        let results = scanner::Scanner::new(&source).scan();
        let (tokens, errors): (Vec<Token>, Vec<RloxError>) = results.into_iter().partition_result();
        if errors.len() != 0 {
            println!("Got errors while scanning: {errors:?}");
            return
        }
        let parsed = parser::Parser::new(tokens).parse();
        let (tree, errors): (Vec<Stmt>, Vec<RloxError>) = parsed.into_iter().partition_result();
        if errors.len() != 0 {
            println!("Got parse errors: {errors:?}");
            return
        }
        self.interpreter.interpret(tree);
    }

    fn run_prompt(&mut self) {
        // print!("> ");
        let stdin = io::stdin();
        for line in stdin.lines() {
            // print!("> ");
            match line {
                Ok(text) => if text == "quit" { break } else if text.len() > 0 { self.run(text) } else { break },
                Err(e) => { println!("{e}"); break }
            }
            // print!("> ")
        }
    }

    fn run_file(&mut self, name: &str) {
        let contents = fs::read_to_string(name).expect("Could not read file");
        self.run(contents);
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut repl = RloxRepl {
        interpreter: Interpreter::new()
    };
    if args.len() > 2 {
        println!("Usage: rlox [script]");
    }
    else if args.len() == 2 {
        repl.run_file(&args[1]);
    }
    else if args.len() == 1 {
        repl.run_prompt();
    }
}
