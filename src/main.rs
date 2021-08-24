mod scanner;
mod token;
use std::env;
use std::io;
use std::io::Write;
use std::fs;
use std::process::exit;
use scanner::Scanner;

fn run(content: String) {
    let scanner = Scanner::new(content);
    match scanner.scan_tokens() {
        Err(errors) => {
            for err in errors {
                println!("{:?}", err);
            }
            exit(65);
        }
        Ok(tokens) => {
            for token in tokens {
                println!("{:?}", token);
            }
        }
    }
}

fn run_file(filename: &str) {
    let contents = fs::read_to_string(filename)
        .expect("Something went wrong reading the file");
    run(contents);
}

fn run_prompt() {
    loop {
        print!("> ");
        io::stdout().flush().expect("Somethig went wrong when flushing IO");
        let mut line = String::new();
        io::stdin().read_line(&mut line)
            .expect("Something went wrong when reading the line");
        if line == "\n" {
            break;
        }
        run(line)
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
