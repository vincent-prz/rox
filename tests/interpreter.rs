// test scanning + parsing + evaluating altogether
use rox::ast;
use rox::evaluator::{evaluate, RuntimeError, Value};
use rox::scanner::Scanner;

fn interpret(s: &str) -> Result<Value, RuntimeError> {
    let scanner = Scanner::new(s.to_string());
    let tokens = scanner
        .scan_tokens()
        .expect("Unexpected failure of scanning");
    let mut parser = ast::parser::Parser::new(tokens);
    let expr = parser.parse().expect("Unexpected failure of parsing");
    evaluate(&expr)
}

#[test]
fn eval_true() {
    assert_eq!(interpret("true"), Ok(Value::True));
}

#[test]
fn eval_false() {
    assert_eq!(interpret("false"), Ok(Value::False));
}

#[test]
fn eval_nil() {
    assert_eq!(interpret("nil"), Ok(Value::Nil));
}

#[test]
fn eval_42() {
    assert_eq!(interpret("42"), Ok(Value::Number(42.0)));
}

#[test]
fn eval_42_float() {
    assert_eq!(interpret("42.0"), Ok(Value::Number(42.0)));
}

#[test]
fn eval_string() {
    assert_eq!(interpret("\"hello\""), Ok(Value::Str("hello".to_string())));
}

#[test]
fn minus_one() {
    assert_eq!(interpret("-1"), Ok(Value::Number(-1.0)));
}

#[test]
fn not_true() {
    assert_eq!(interpret("!true"), Ok(Value::False));
}

#[test]
fn not_false() {
    assert_eq!(interpret("!false"), Ok(Value::True));
}

#[test]
fn not_one() {
    assert_eq!(interpret("!1"), Ok(Value::False));
}

#[test]
fn not_string() {
    assert_eq!(interpret("!\"\""), Ok(Value::False));
}

#[test]
fn not_nil() {
    assert_eq!(interpret("!nil"), Ok(Value::True));
}

#[test]
fn one_plus_one() {
    assert_eq!(interpret("1 + 1"), Ok(Value::Number(2.0)));
}

#[test]
fn minus_one_plus_one() {
    assert_eq!(interpret("-1 + 1"), Ok(Value::Number(0.0)));
}

#[test]
fn two_times_three() {
    assert_eq!(interpret("2 * 3"), Ok(Value::Number(6.0)));
}

#[test]
fn times_has_higher_precedence_than_plus() {
    assert_eq!(interpret("1 + 2 * 3"), Ok(Value::Number(7.0)));
}

#[test]
fn one_minus_one() {
    assert_eq!(interpret("1 - 1"), Ok(Value::Number(0.0)));
}

#[test]
fn five_div_by_two() {
    assert_eq!(interpret("5 / 2"), Ok(Value::Number(2.5)));
}

#[test]
fn computation_with_parens() {
    assert_eq!(interpret("(1 + 2) * 3"), Ok(Value::Number(9.0)));
}

#[test]
fn add_strings() {
    assert_eq!(
        interpret("\"hello \" + \"world!\""),
        Ok(Value::Str("hello world!".to_string()))
    );
}

#[test]
fn number_equality() {
    assert_eq!(interpret("2 == 2"), Ok(Value::True));
    assert_eq!(interpret("2 == 3"), Ok(Value::False));
}

#[test]
fn string_equality() {
    assert_eq!(interpret("\"hello\" == \"hello\""), Ok(Value::True));
    assert_eq!(interpret("\"hello\" == \"world\""), Ok(Value::False));
}

#[test]
fn boolean_equality() {
    assert_eq!(interpret("true == true"), Ok(Value::True));
    assert_eq!(interpret("true == false"), Ok(Value::False));
}

#[test]
fn nil_equality() {
    assert_eq!(interpret("nil == nil"), Ok(Value::True));
}

#[test]
fn number_inequality() {
    assert_eq!(interpret("2 != 2"), Ok(Value::False));
    assert_eq!(interpret("2 != 3"), Ok(Value::True));
}

#[test]
fn string_inequality() {
    assert_eq!(interpret("\"hello\" != \"hello\""), Ok(Value::False));
    assert_eq!(interpret("\"hello\" != \"world\""), Ok(Value::True));
}

#[test]
fn boolean_inequality() {
    assert_eq!(interpret("true != true"), Ok(Value::False));
    assert_eq!(interpret("true != false"), Ok(Value::True));
}

#[test]
fn nil_inequality() {
    assert_eq!(interpret("nil != nil"), Ok(Value::False));
}
