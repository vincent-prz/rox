use rox::ast;
use rox::interpreter::{FlowInterruption, Interpreter};
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
fn run_expr(tokens: &Vec<Token>, interpreter: &mut Interpreter) -> Option<()> {
    let mut parser = ast::parser::Parser::new(tokens.clone());
    let expr = match parser.expression() {
        Err(_) => return None,
        Ok(expr) => expr,
    };
    match interpreter.evaluate_expression(&expr) {
        Err(error) => {
            println!("{:?}", error);
        }
        Ok(val) => {
            println!("{}", val.borrow());
        }
    }
    Some(())
}

fn run(content: String, interpreter: &mut Interpreter, prompt_mode: bool) {
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
        match run_expr(&tokens, interpreter) {
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
    match interpreter.interpret(&program) {
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
    run(contents, &mut Interpreter::new(), false);
}

fn run_prompt() {
    let mut interpreter = Interpreter::new();
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
        run(line, &mut interpreter, true)
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
