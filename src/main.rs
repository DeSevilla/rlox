use std::env;
use std::fs;
use std::io;
use rlox::{Token, RloxError};
use itertools::Itertools;
pub mod scanner;
pub mod parser;


fn run(source: String) {
    let results = scanner::Scanner::new(&source).scan();
    // println!("{}", Expr::test());
    let (tokens, errors): (Vec<Token>, Vec<RloxError>) = results.into_iter().partition_result();
    // let tokens: Vec<_> = tokens.into_iter().filter_map(|x| x.ok()).collect();
    // let errors: Vec<_> = errors.into_iter().filter_map(|x| x.err()).collect();
    if errors.len() != 0 {
        println!("Got errors while scanning: {errors:?}");
        return
    }
    // println!("{source} scans to {tokens:?}");
    let parsed = parser::Parser::new(tokens).parse();
    if parsed.is_err() {
        println!("Got error while parsing: {parsed:?}");
        return
    }
    let tree = match parsed {
        Ok(ast) => ast,
        Err(_) => unreachable!(),
    };
    let result = tree.evaluate();
    match result {
        Ok(lit) => println!("{}", lit.to_string()),
        Err(e) => println!("Got error while evaluating: {e:?}")
    }
}

fn run_prompt() {
    // print!("> ");
    let stdin = io::stdin();
    for line in stdin.lines() {
        // print!("> ");
        match line {
            Ok(text) => if text == "quit" { break } else if text.len() > 0 { run(text) } else { break },
            Err(e) => { println!("{e}"); break }
        }
        // print!("> ")
    }
}

fn run_file(name: &str) {
    let contents = fs::read_to_string(name).expect("Could not read file");
    run(contents);
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() > 2 {
        println!("Usage: rlox [script]");
    }
    else if args.len() == 2 {
        run_file(&args[1]);
    }
    else if args.len() == 1 {
        run_prompt();
    }
}
