use crate::ast::{Binary, Expr, Literal, Unary};
use crate::token::{Token, TokenType};
use std::fmt;

#[derive(Debug, PartialEq)]
pub enum Value {
    Nil,
    True,
    False,
    Str(String),
    Number(f64),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::True => write!(f, "true"),
            Value::False => write!(f, "false"),
            Value::Str(s) => write!(f, "{}", s),
            Value::Number(n) => write!(f, "{}", n),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct RuntimeError {
    token: Token,
    message: String,
}

impl RuntimeError {
    fn new(token: Token, message: String) -> Self {
        RuntimeError { token, message }
    }
}

pub fn evaluate(expr: &Expr) -> Result<Value, RuntimeError> {
    match expr {
        Expr::Literal(lit) => evaluate_literal(lit),
        Expr::Unary(unary) => evaluate_unary(unary),
        Expr::Binary(binary) => evaluate_binary(binary),
        Expr::Grouping(group) => evaluate(&group.expression),
    }
}

fn evaluate_literal(lit: &Literal) -> Result<Value, RuntimeError> {
    Ok(match lit {
        Literal::Nil => Value::Nil,
        Literal::True => Value::True,
        Literal::False => Value::False,
        Literal::Str(s) => Value::Str(s.clone()),
        Literal::Number(n) => Value::Number(*n),
    })
}

fn evaluate_unary(unary: &Unary) -> Result<Value, RuntimeError> {
    let right_val = evaluate(&unary.right)?;
    match unary.operator.typ {
        TokenType::Minus => match right_val {
            Value::Number(n) => Ok(Value::Number(-n)),
            _ => panic!(),
        },
        TokenType::Bang => Ok(bool_to_val(!is_truthy(&right_val))),
        _ => panic!(),
    }
}

fn evaluate_binary(binary: &Binary) -> Result<Value, RuntimeError> {
    let left_val = evaluate(&binary.left)?;
    let right_val = evaluate(&binary.right)?;
    match binary.operator.typ {
        TokenType::Plus => match (left_val, right_val) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x + y)),
            (Value::Str(x), Value::Str(y)) => Ok(Value::Str(format!("{}{}", x, y))),
            _ => panic!(),
        },
        TokenType::Minus => match (left_val, right_val) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x - y)),
            _ => panic!(),
        },
        TokenType::Star => match (left_val, right_val) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x * y)),
            _ => panic!(),
        },
        TokenType::Slash => match (left_val, right_val) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x / y)),
            _ => panic!(),
        },
        TokenType::EqualEqual => Ok(bool_to_val(left_val == right_val)),
        TokenType::BangEqual => Ok(bool_to_val(left_val != right_val)),
        TokenType::LessEqual => match (left_val, right_val) {
            (Value::Number(x), Value::Number(y)) => Ok(bool_to_val(x <= y)),
            _ => panic!(),
        },
        TokenType::Less => match (left_val, right_val) {
            (Value::Number(x), Value::Number(y)) => Ok(bool_to_val(x < y)),
            _ => panic!(),
        },
        TokenType::GreaterEqual => match (left_val, right_val) {
            (Value::Number(x), Value::Number(y)) => Ok(bool_to_val(x >= y)),
            _ => panic!(),
        },
        TokenType::Greater => match (left_val, right_val) {
            (Value::Number(x), Value::Number(y)) => Ok(bool_to_val(x > y)),
            _ => panic!(),
        },
        _ => panic!(),
    }
}

fn is_truthy(value: &Value) -> bool {
    match value {
        Value::False => false,
        Value::Nil => false,
        _ => true,
    }
}

fn bool_to_val(b: bool) -> Value {
    match b {
        true => Value::True,
        false => Value::False,
    }
}
