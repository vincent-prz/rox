use crate::ast::{
    Assignment, Binary, Declaration, Expr, IfStmt, Literal, Logical, Program, Statement, Unary,
    VarDecl, WhileStmt,
};
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

#[derive(Clone, Debug)]
pub struct Environment {
    values: HashMap<String, Value>,
    enclosing: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
            enclosing: None,
        }
    }

    fn new_with_enclosing(env: Environment) -> Self {
        Environment {
            values: HashMap::new(),
            enclosing: Some(Box::new(env)),
        }
    }

    fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    fn assign(&mut self, name: &Token, value: Value) -> Result<(), RuntimeError> {
        if self.values.contains_key(&name.lexeme) {
            return Ok(self.define(name.lexeme.clone(), value));
        }
        match &mut self.enclosing {
            Some(enclosing) => enclosing.assign(name, value),
            None => Err(RuntimeError::new(
                name.clone(),
                format!("Cannot assign undefined variable {}.", name.lexeme),
            )),
        }
    }

    fn get(&self, name: &Token) -> Result<Value, RuntimeError> {
        match self.values.get(&name.lexeme) {
            Some(value) => Ok(value.clone()),
            None => match &self.enclosing {
                Some(enclosing) => enclosing.get(name),
                None => Err(RuntimeError::new(
                    name.clone(),
                    format!("Undefined variable {}.", name.lexeme),
                )),
            },
        }
    }

    fn copy_from(&mut self, src: Environment) {
        self.values = src.values;
        self.enclosing = src.enclosing;
    }
}

pub mod interpreter {

    use super::*;

    pub fn interpret(program: &Program) -> Result<(), RuntimeError> {
        let mut env = Environment::new();
        execute_program(&mut env, program)
    }

    // This variant of `interpret` is useful for the REPL
    pub fn interpret_with_env(
        env: &mut Environment,
        program: &Program,
    ) -> Result<(), RuntimeError> {
        execute_program(env, program)
    }

    fn execute_program(env: &mut Environment, program: &Program) -> Result<(), RuntimeError> {
        for decl in &program.declarations {
            execute_declaration(env, &decl)?;
        }
        Ok(())
    }

    fn execute_declaration(env: &mut Environment, decl: &Declaration) -> Result<(), RuntimeError> {
        match decl {
            Declaration::VarDecl(var_decl) => execute_var_decl(env, var_decl),
            Declaration::Statement(stmt) => execute_statement(env, stmt),
        }
    }

    fn execute_var_decl(env: &mut Environment, decl: &VarDecl) -> Result<(), RuntimeError> {
        let varname = decl.identifier.lexeme.clone();
        let value = match &decl.initializer {
            None => Value::Nil,
            Some(expr) => evaluate_expression(env, &expr)?,
        };
        env.define(varname, value);
        Ok(())
    }

    fn execute_statement(env: &mut Environment, stmt: &Statement) -> Result<(), RuntimeError> {
        match stmt {
            Statement::PrintStmt(expr) => {
                let value = evaluate_expression(env, &expr)?;
                println!("{}", value);
            }
            Statement::ExprStmt(expr) => {
                evaluate_expression(env, &expr)?;
            }
            Statement::Block(declarations) => execute_block(env, declarations)?,
            Statement::IfStmt(IfStmt {
                condition,
                then_branch,
                else_branch,
            }) => {
                if is_truthy(&evaluate_expression(env, condition)?) {
                    execute_statement(env, then_branch)?;
                } else {
                    match else_branch {
                        None => {}
                        Some(statement) => execute_statement(env, statement)?,
                    }
                }
            }
            Statement::WhileStmt(while_stmt) => execute_while_statement(env, while_stmt)?,
        }
        Ok(())
    }

    fn execute_block(
        env: &mut Environment,
        declarations: &Vec<Declaration>,
    ) -> Result<(), RuntimeError> {
        // FIXME: ugly clone here, because I didn't manage to make enclosing behind a ref
        // Indeed, I had lifetimes compiler issues I could not solve.
        let parent_env = env.clone();
        let mut new_env = Environment::new_with_enclosing(parent_env);
        for decl in declarations {
            execute_declaration(&mut new_env, decl)?;
        }
        env.copy_from(*new_env.enclosing.unwrap());
        Ok(())
    }

    fn execute_while_statement(
        env: &mut Environment,
        while_stmt: &WhileStmt,
    ) -> Result<(), RuntimeError> {
        let condition = &while_stmt.condition;
        let body = &while_stmt.body;
        while is_truthy(&evaluate_expression(env, &condition)?) {
            execute_statement(env, &body)?;
        }
        Ok(())
    }

    // NOTE - public function for REPL
    pub fn evaluate_expression(env: &mut Environment, expr: &Expr) -> Result<Value, RuntimeError> {
        match expr {
            Expr::Literal(lit) => evaluate_literal(env, lit),
            Expr::Unary(unary) => evaluate_unary(env, unary),
            Expr::Binary(binary) => evaluate_binary(env, binary),
            Expr::Grouping(group) => evaluate_expression(env, &group.expression),
            Expr::Variable(name) => env.get(name),
            Expr::Assignment(assignment) => evaluate_assignment(env, assignment),
            Expr::Logical(logical) => evaluate_logical(env, logical),
        }
    }

    fn evaluate_literal(env: &Environment, lit: &Literal) -> Result<Value, RuntimeError> {
        Ok(match lit {
            Literal::Nil => Value::Nil,
            Literal::True => Value::True,
            Literal::False => Value::False,
            Literal::Str(s) => Value::Str(s.clone()),
            Literal::Number(n) => Value::Number(*n),
        })
    }

    fn evaluate_unary(env: &mut Environment, unary: &Unary) -> Result<Value, RuntimeError> {
        let right_val = evaluate_expression(env, &unary.right)?;
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

    fn evaluate_binary(env: &mut Environment, binary: &Binary) -> Result<Value, RuntimeError> {
        let left_val = evaluate_expression(env, &binary.left)?;
        let right_val = evaluate_expression(env, &binary.right)?;
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
            // FIXME: this
            _ => panic!(),
        }
    }

    fn evaluate_assignment(
        env: &mut Environment,
        assignment: &Assignment,
    ) -> Result<Value, RuntimeError> {
        let value = evaluate_expression(env, &assignment.value)?;
        env.assign(&assignment.name, value.clone())?;
        Ok(value)
    }

    fn evaluate_logical(env: &mut Environment, logical: &Logical) -> Result<Value, RuntimeError> {
        let left_val = evaluate_expression(env, &logical.left)?;
        match logical.operator.typ {
            TokenType::Or => {
                if is_truthy(&left_val) {
                    return Ok(left_val);
                }
                let right_val = evaluate_expression(env, &logical.right)?;
                return Ok(right_val);
            }
            TokenType::And => {
                if !is_truthy(&left_val) {
                    return Ok(left_val);
                }
                let right_val = evaluate_expression(env, &logical.right)?;
                return Ok(right_val);
            }
            // This case should not occur if parsing was done correctly
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
