use std::env;
use std::fs;
use std::io;
use rlox::Stmt;
use rlox::{Token, RloxError};
use itertools::Itertools;
pub mod scanner;
pub mod parser;
pub mod interpreter;
pub mod resolver;

struct RloxRepl {
    interpreter: interpreter::Interpreter,
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
                Ok(text) => if text == "quit" { break } else if text.len() > 0 { self.run(text) } else { continue },
                Err(e) => { println!("{e}"); break }
            }
            // print!("> ")
        }
    }

    fn run_file(&mut self, path: &str) {
        let append_test = |p| "./test/".to_owned() + p;
        println!("{}", append_test(path));
        let one_file = fs::read_to_string(path)
            .or_else(|_| fs::read_to_string(append_test(path)));
        if one_file.is_ok() {
            return self.run(one_file.unwrap());
        }
        else {
            println!("Could not read an individual file {one_file:?}");
        }
        let many_files = fs::read_dir(path)
            .or_else(|_| fs::read_dir(append_test(path)));
        match many_files {
            Ok(filenames) => for file in filenames {
                let contents = match file {
                    Ok(entry) => { println!("{:?}", entry.path()); fs::read_to_string(entry.path()) },
                    Err(e) => { println!("Failed to open file; {e}"); continue; }
                };
                self.interpreter.clear_env();
                self.run(contents.expect("Could not read from file"));
            },
            Err(e) => println!("Could not read any matching files {e:?}")
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut repl = RloxRepl {
        interpreter: interpreter::Interpreter::new()
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
