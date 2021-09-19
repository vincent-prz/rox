use crate::ast::{Binary, Declaration, Expr, Literal, Program, Statement, Unary, VarDecl};
use crate::token::{Token, TokenType};
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
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
    pub token: Token,
    pub message: String,
}

impl RuntimeError {
    fn new(token: Token, message: String) -> Self {
        RuntimeError { token, message }
    }
}

struct Environment {
    values: HashMap<String, Value>,
}

impl Environment {
    fn new() -> Self {
        Environment {
            values: HashMap::new(),
        }
    }

    fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    fn get(&self, name: &Token) -> Result<Value, RuntimeError> {
        match self.values.get(&name.lexeme) {
            Some(value) => Ok(value.clone()),
            None => Err(RuntimeError::new(
                name.clone(),
                format!("Undefined variable {}.", name.lexeme),
            )),
        }
    }
}

pub struct Interpreter {
    environnment: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            environnment: Environment::new(),
        }
    }

    pub fn interpret(&mut self, program: &Program) -> Result<(), RuntimeError> {
        self.execute_program(program)
    }

    fn execute_program(&mut self, program: &Program) -> Result<(), RuntimeError> {
        for decl in &program.declarations {
            self.execute_declaration(&decl)?;
        }
        Ok(())
    }

    fn execute_declaration(&mut self, decl: &Declaration) -> Result<(), RuntimeError> {
        match decl {
            Declaration::VarDecl(var_decl) => self.execute_var_decl(var_decl),
            Declaration::Statement(stmt) => self.execute_statement(stmt),
        }
    }

    fn execute_var_decl(&mut self, decl: &VarDecl) -> Result<(), RuntimeError> {
        let varname = decl.identifier.lexeme.clone();
        let value = match &decl.initializer {
            None => Value::Nil,
            Some(expr) => self.evaluate_expression(&expr)?,
        };
        self.environnment.define(varname, value);
        Ok(())
    }

    fn execute_statement(&self, stmt: &Statement) -> Result<(), RuntimeError> {
        match stmt {
            Statement::PrintStmt(expr) => {
                let value = self.evaluate_expression(&expr)?;
                println!("{}", value);
            }
            Statement::ExprStmt(expr) => {
                self.evaluate_expression(&expr)?;
            }
        };
        Ok(())
    }

    // NOTE - letting this function public to allow unit testing of expression parsing and evaluation.
    pub fn evaluate_expression(&self, expr: &Expr) -> Result<Value, RuntimeError> {
        match expr {
            Expr::Literal(lit) => self.evaluate_literal(lit),
            Expr::Unary(unary) => self.evaluate_unary(unary),
            Expr::Binary(binary) => self.evaluate_binary(binary),
            Expr::Grouping(group) => self.evaluate_expression(&group.expression),
            Expr::Variable(name) => self.environnment.get(name),
        }
    }

    fn evaluate_literal(&self, lit: &Literal) -> Result<Value, RuntimeError> {
        Ok(match lit {
            Literal::Nil => Value::Nil,
            Literal::True => Value::True,
            Literal::False => Value::False,
            Literal::Str(s) => Value::Str(s.clone()),
            Literal::Number(n) => Value::Number(*n),
        })
    }

    fn evaluate_unary(&self, unary: &Unary) -> Result<Value, RuntimeError> {
        let right_val = self.evaluate_expression(&unary.right)?;
        match unary.operator.typ {
            TokenType::Minus => match right_val {
                Value::Number(n) => Ok(Value::Number(-n)),
                _ => Err(RuntimeError::new(
                    unary.operator.clone(),
                    "Operand must be a number.".to_string(),
                )),
            },
            TokenType::Bang => Ok(bool_to_val(!is_truthy(&right_val))),
            _ => panic!(),
        }
    }

    fn evaluate_binary(&self, binary: &Binary) -> Result<Value, RuntimeError> {
        let left_val = self.evaluate_expression(&binary.left)?;
        let right_val = self.evaluate_expression(&binary.right)?;
        match binary.operator.typ {
            TokenType::Plus => match (left_val, right_val) {
                (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x + y)),
                (Value::Str(x), Value::Str(y)) => Ok(Value::Str(format!("{}{}", x, y))),
                _ => Err(RuntimeError::new(
                    binary.operator.clone(),
                    "Operands must be two numbers or two strings.".to_string(),
                )),
            },
            TokenType::Minus => match (left_val, right_val) {
                (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x - y)),
                _ => Err(make_numbers_operand_error(&binary.operator)),
            },
            TokenType::Star => match (left_val, right_val) {
                (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x * y)),
                _ => Err(make_numbers_operand_error(&binary.operator)),
            },
            TokenType::Slash => match (left_val, right_val) {
                (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x / y)),
                _ => Err(make_numbers_operand_error(&binary.operator)),
            },
            TokenType::EqualEqual => Ok(bool_to_val(left_val == right_val)),
            TokenType::BangEqual => Ok(bool_to_val(left_val != right_val)),
            TokenType::LessEqual => match (left_val, right_val) {
                (Value::Number(x), Value::Number(y)) => Ok(bool_to_val(x <= y)),
                _ => Err(make_numbers_operand_error(&binary.operator)),
            },
            TokenType::Less => match (left_val, right_val) {
                (Value::Number(x), Value::Number(y)) => Ok(bool_to_val(x < y)),
                _ => Err(make_numbers_operand_error(&binary.operator)),
            },
            TokenType::GreaterEqual => match (left_val, right_val) {
                (Value::Number(x), Value::Number(y)) => Ok(bool_to_val(x >= y)),
                _ => Err(make_numbers_operand_error(&binary.operator)),
            },
            TokenType::Greater => match (left_val, right_val) {
                (Value::Number(x), Value::Number(y)) => Ok(bool_to_val(x > y)),
                _ => Err(make_numbers_operand_error(&binary.operator)),
            },
            _ => panic!(),
        }
    }
}

fn make_numbers_operand_error(operator: &Token) -> RuntimeError {
    RuntimeError::new(operator.clone(), "Operands must be a number.".to_string())
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
