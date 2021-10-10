use rox::ast;
use rox::interpreter::{interpreter, Environment};
use rox::scanner::Scanner;
use std::env;
use std::fs;
use std::io;
use std::io::Write;
use std::process::exit;

/// Try to parse and run an expression. This is useful for the REPL mode,
/// were we want to be able to ro evaluate an expression (and not necessaritly a statement).
/// return None if the parsing failed.
fn run_expr(parser: &mut ast::parser::Parser, env: &mut Environment) -> Option<()> {
    let expr = match parser.expression() {
        Err(_) => return None,
        Ok(expr) => expr,
    };
    match interpreter::evaluate_expression(env, &expr) {
        Err(error) => {
            println!("{:?}", error);
        }
        Ok(val) => {
            println!("{}", val);
        }
    }
    Some(())
}

fn run(content: String, env: &mut Environment, prompt_mode: bool) {
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
    let mut parser = ast::parser::Parser::new(tokens);
    if prompt_mode {
        match run_expr(&mut parser, env) {
            None => {}
            Some(()) => return,
        }
    }
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
    match interpreter::interpret_with_env(env, &program) {
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
    run(contents, &mut Environment::new(), false);
}

fn run_prompt() {
    let mut env = Environment::new();
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
