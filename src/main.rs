use rox::ast;
use rox::interpreter::{interpreter, Environment, FlowInterruption};
use rox::scanner::Scanner;
use rox::token::Token;
use std::cell::RefCell;
use std::env;
use std::fs;
use std::io;
use std::io::Write;
use std::process::exit;
use std::rc::Rc;

/// Try to parse and run an expression. This is useful for the REPL mode,
/// where we want to be able to ro evaluate an expression (and not necessarily a statement).
/// return None if the parsing failed.
fn run_expr(tokens: &Vec<Token>, env: Option<Rc<RefCell<Environment>>>) -> Option<()> {
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

fn run(content: String, env: Option<Rc<RefCell<Environment>>>, prompt_mode: bool) {
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
        // match run_expr(&tokens, env) {
        //     None => {}
        //     Some(()) => return,
        // }
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
        Err(FlowInterruption::RuntimeError(err)) => {
            println!("{}\n[line {}]", err.message, err.token.line);
            if !prompt_mode {
                exit(70);
            }
            return;
        }
        Err(_) => {
            // this case should not oocur
            panic!();
        }
    }
}

fn run_file(filename: &str) {
    let contents = fs::read_to_string(filename).expect("Something went wrong reading the file");
    run(contents, None, false);
}

fn run_prompt() {
    // let env = None;
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
        // run(line, Rc::clone(&env), true)
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
