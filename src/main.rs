use std::env;
use std::fs;
use std::io;
use rlox::{Expr, Token, RloxError};
use itertools::Itertools;
pub mod scanner;
pub mod parser;


fn run(source: String) {
    let mut had_error = false;
    let results = scanner::Scanner::new(&source).scan();
    println!("{}", Expr::test());
    let (tokens, errors): (Vec<Token>, Vec<RloxError>) = results.into_iter().partition_result();
    // let tokens: Vec<_> = tokens.into_iter().filter_map(|x| x.ok()).collect();
    // let errors: Vec<_> = errors.into_iter().filter_map(|x| x.err()).collect();
    if errors.len() != 0 {
        had_error = true;
        println!("Got errors while scanning: {errors:?}");
    }
    println!("{source} scans to {tokens:?}");
    let tree = parser::Parser::new(tokens).parse();
    println!("Tokens parse to {tree:?}");
    if had_error {
        println!("Not running due to errors");
    }
    else {
        match tree {
            Ok(ast) => println!("Parses to: {}", ast.parenthesize()),
            Err(e) => println!("Got error while parsing: {e}"),
        }
    }
}

fn run_prompt() {
    let stdin = io::stdin();
    for line in stdin.lines() {
        match line {
            Ok(text) => if text.len() > 0 { run(text)} else { break },
            Err(e) => { println!("{e}"); break }
        }
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
    println!("Hello, world!");
}
