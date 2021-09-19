use rox::ast;
use rox::interpreter::Interpreter;
use rox::scanner::Scanner;
use std::env;
use std::fs;
use std::io;
use std::io::Write;
use std::process::exit;

fn run(content: String, interpreter: &mut Interpreter, exit_on_failure: bool) {
    let scanner = Scanner::new(content);
    let tokens = match scanner.scan_tokens() {
        Err(errors) => {
            for err in errors {
                println!("{:?}", err);
            }
            if exit_on_failure {
                exit(65);
            }
            return;
        }
        Ok(tokens) => tokens,
    };
    let mut parser = ast::parser::Parser::new(tokens);
    let program = match parser.parse() {
        Err(error) => {
            println!("{:?}", error);
            if exit_on_failure {
                exit(65);
            }
            return;
        }
        Ok(expr) => expr,
    };
    match interpreter.interpret(&program) {
        Ok(_) => {}
        Err(err) => {
            println!("{}\n[line {}]", err.message, err.token.line);
            if exit_on_failure {
                exit(70);
            }
            return;
        }
    }
}

fn run_file(filename: &str) {
    let contents = fs::read_to_string(filename).expect("Something went wrong reading the file");
    let mut interpreter = Interpreter::new();
    run(contents, &mut interpreter, true);
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
        run(line, &mut interpreter, false)
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
