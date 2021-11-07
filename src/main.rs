use rox::ast;
use rox::interpreter::{interpreter, Environment};
use rox::scanner::Scanner;
use rox::token::Token;
use std::env;
use std::fs;
use std::io;
use std::io::Write;
use std::process::exit;

/// Try to parse and run an expression. This is useful for the REPL mode,
/// where we want to be able to ro evaluate an expression (and not necessarily a statement).
/// return None if the parsing failed.
fn run_expr(tokens: &Vec<Token>, env: &mut Option<Environment>) -> Option<()> {
    let mut parser = ast::parser::Parser::new(tokens.clone());
    let expr = match parser.expression() {
        Err(_) => return None,
        Ok(expr) => expr,
    };
    match interpreter::evaluate_expression_(env, &expr) {
        Err(error) => {
            println!("{:?}", error);
        }
        Ok(val) => {
            println!("{}", val);
        }
    }
    Some(())
}

fn run(content: String, env: &mut Option<Environment>, prompt_mode: bool) {
    let scanner = Scanner::new(content);
    let tokens = match scanner.scan_tokens() {
        Err(errors) => {
            for err in errors {
                println!("{:?}", err);
            }
            if !prompt_mode {
                exit(65);
            }
            return;
        }
        Ok(tokens) => tokens,
    };

    if prompt_mode {
        match run_expr(&tokens, env) {
            None => {}
            Some(()) => return,
        }
    }

    let mut parser = ast::parser::Parser::new(tokens);
    let program = match parser.parse() {
        Err(error) => {
            println!("{:?}", error);
            if !prompt_mode {
                exit(65);
            }
            return;
        }
        Ok(program) => program,
    };
    match interpreter::interpret(env, &program) {
        Ok(_) => {}
        Err(err) => {
            println!("{}\n[line {}]", err.message, err.token.line);
            if !prompt_mode {
                exit(70);
            }
            return;
        }
    }
}

fn run_file(filename: &str) {
    let contents = fs::read_to_string(filename).expect("Something went wrong reading the file");
    let mut env = None;
    run(contents, &mut env, false);
}

fn run_prompt() {
    let mut env = None;
    loop {
        print!("> ");
        io::stdout()
            .flush()
            .expect("Somethig went wrong when flushing IO");
        let mut line = String::new();
        io::stdin()
            .read_line(&mut line)
            .expect("Something went wrong when reading the line");
        if line == "\n" {
            break;
        }
        run(line, &mut env, true)
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() > 2 {
        println!("Usage: rox [script]");
        exit(64);
    } else if args.len() == 2 {
        run_file(&args[1]);
    } else {
        run_prompt();
    }
}
