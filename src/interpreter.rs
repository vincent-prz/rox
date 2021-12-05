use crate::ast::{
    Assignment, Binary, Call, Declaration, Expr, FunDecl, IfStmt, Literal, Logical, Program,
    Statement, Unary, VarDecl, WhileStmt,
};
use crate::token::{Token, TokenType};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

#[derive(Debug, Clone)]
pub enum Value {
    Nil,
    True,
    False,
    Str(String),
    Number(f64),
    Callable(Callable),
}

#[derive(Debug, Clone)]
pub enum Callable {
    NativeClock,
    Function(Function),
}

#[derive(Debug, Clone)]
pub struct Function {
    decl: FunDecl,
    closure: Rc<RefCell<Environment>>,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::True => write!(f, "true"),
            Value::False => write!(f, "false"),
            Value::Str(s) => write!(f, "{}", s),
            Value::Number(n) => write!(f, "{}", n),
            Value::Callable(callable) => write!(f, "{}", callable),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Nil, Value::Nil) => true,
            (Value::True, Value::True) => true,
            (Value::False, Value::False) => true,
            (Value::Str(s1), Value::Str(s2)) => s1 == s2,
            (Value::Number(n1), Value::Number(n2)) => n1 == n2,
            _ => false, // functions are never equal to anything
        }
    }
}

impl fmt::Display for Callable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Callable::NativeClock => write!(f, "<native fn>"),
            Callable::Function(func) => write!(f, "<fn {}>", func.decl.name.lexeme),
        }
    }
}

#[derive(Debug)]
pub enum FlowInterruption {
    ReturnValue(Value),
    RuntimeError(RuntimeError),
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
    enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
            enclosing: None,
        }
    }

    fn new_with_enclosing(env: Rc<RefCell<Environment>>) -> Self {
        Environment {
            values: HashMap::new(),
            enclosing: Some(env),
        }
    }

    fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    fn assign(&mut self, name: &Token, value: Value) -> Result<(), FlowInterruption> {
        if self.values.contains_key(&name.lexeme) {
            return Ok(self.define(name.lexeme.clone(), value));
        }
        match &mut self.enclosing {
            Some(enclosing) => enclosing.borrow_mut().assign(name, value),
            None => Err(FlowInterruption::RuntimeError(RuntimeError::new(
                name.clone(),
                format!("Cannot assign undefined variable {}.", name.lexeme),
            ))),
        }
    }

    fn get(&self, name: &Token) -> Result<Value, FlowInterruption> {
        match self.values.get(&name.lexeme) {
            Some(value) => Ok(value.clone()),
            None => match &self.enclosing {
                Some(enclosing) => enclosing.borrow().get(name),
                None => Err(FlowInterruption::RuntimeError(RuntimeError::new(
                    name.clone(),
                    format!("Undefined variable {}.", name.lexeme),
                ))),
            },
        }
    }
}

pub mod interpreter {

    use super::*;

    fn get_globals_env() -> Environment {
        let mut globals = Environment::new();
        globals.define("clock".to_string(), Value::Callable(Callable::NativeClock));
        globals
    }

    pub fn interpret(
        env: Option<Rc<RefCell<Environment>>>,
        program: &Program,
    ) -> Result<(), FlowInterruption> {
        let globals = get_globals_env();
        match env {
            None => execute_program(Rc::new(RefCell::new(globals)), program),
            Some(e) => execute_program(e, program),
        }
    }

    fn execute_program(
        env: Rc<RefCell<Environment>>,
        program: &Program,
    ) -> Result<(), FlowInterruption> {
        for decl in &program.declarations {
            execute_declaration(Rc::clone(&env), &decl)?;
        }
        Ok(())
    }

    fn execute_declaration(
        env: Rc<RefCell<Environment>>,
        decl: &Declaration,
    ) -> Result<(), FlowInterruption> {
        match decl {
            Declaration::FunDecl(fun_decl) => execute_fun_decl(env, fun_decl),
            Declaration::VarDecl(var_decl) => execute_var_decl(env, var_decl),
            Declaration::Statement(stmt) => execute_statement(env, stmt),
        }
    }
    fn execute_fun_decl(
        env: Rc<RefCell<Environment>>,
        decl: &FunDecl,
    ) -> Result<(), FlowInterruption> {
        env.borrow_mut().define(
            decl.name.lexeme.clone(),
            Value::Callable(Callable::Function(Function {
                decl: decl.clone(),
                closure: Rc::clone(&env),
            })),
        );
        Ok(())
    }

    fn execute_var_decl(
        env: Rc<RefCell<Environment>>,
        decl: &VarDecl,
    ) -> Result<(), FlowInterruption> {
        let varname = decl.identifier.lexeme.clone();
        let value = match &decl.initializer {
            None => Value::Nil,
            Some(expr) => evaluate_expression(Rc::clone(&env), &expr)?,
        };
        env.borrow_mut().define(varname, value);
        Ok(())
    }

    fn execute_statement(
        env: Rc<RefCell<Environment>>,
        stmt: &Statement,
    ) -> Result<(), FlowInterruption> {
        match stmt {
            Statement::PrintStmt(expr) => {
                let value = evaluate_expression(env, &expr)?;
                println!("{}", value);
            }
            Statement::ReturnStmt(option_expr) => {
                let return_value = match option_expr {
                    None => Value::Nil,
                    Some(expr) => evaluate_expression(env, expr)?,
                };
                // propagating return value as error to make sure the `return` statement
                // interrupts the function flow
                return Err(FlowInterruption::ReturnValue(return_value));
            }
            Statement::ExprStmt(expr) => {
                evaluate_expression(env, &expr)?;
            }
            Statement::Block(declarations) => {
                let block_env = Rc::new(RefCell::new(Environment::new_with_enclosing(env)));
                execute_block(block_env, declarations)?;
            }
            Statement::IfStmt(IfStmt {
                condition,
                then_branch,
                else_branch,
            }) => {
                if is_truthy(&evaluate_expression(Rc::clone(&env), condition)?) {
                    execute_statement(Rc::clone(&env), then_branch)?;
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
        env: Rc<RefCell<Environment>>,
        declarations: &Vec<Declaration>,
    ) -> Result<(), FlowInterruption> {
        for decl in declarations {
            execute_declaration(Rc::clone(&env), decl)?;
        }
        Ok(())
    }

    fn execute_while_statement(
        env: Rc<RefCell<Environment>>,
        while_stmt: &WhileStmt,
    ) -> Result<(), FlowInterruption> {
        let condition = &while_stmt.condition;
        let body = &while_stmt.body;
        while is_truthy(&evaluate_expression(Rc::clone(&env), &condition)?) {
            execute_statement(Rc::clone(&env), &body)?;
        }
        Ok(())
    }

    // NOTE - public variant of evaluate_expression for REPL
    pub fn evaluate_expression_(
        env: Option<Rc<RefCell<Environment>>>,
        expr: &Expr,
    ) -> Result<Value, FlowInterruption> {
        match env {
            None => evaluate_expression(Rc::new(RefCell::new(get_globals_env())), expr),
            Some(e) => evaluate_expression(e, expr),
        }
    }

    fn evaluate_expression(
        env: Rc<RefCell<Environment>>,
        expr: &Expr,
    ) -> Result<Value, FlowInterruption> {
        match expr {
            Expr::Literal(lit) => evaluate_literal(lit),
            Expr::Unary(unary) => evaluate_unary(env, unary),
            Expr::Binary(binary) => evaluate_binary(env, binary),
            Expr::Grouping(group) => evaluate_expression(env, &group.expression),
            Expr::Variable(name) => env.borrow().get(name),
            Expr::Assignment(assignment) => evaluate_assignment(env, assignment),
            Expr::Logical(logical) => evaluate_logical(env, logical),
            Expr::Call(call) => evaluate_call(env, call),
        }
    }

    fn evaluate_literal(lit: &Literal) -> Result<Value, FlowInterruption> {
        Ok(match lit {
            Literal::Nil => Value::Nil,
            Literal::True => Value::True,
            Literal::False => Value::False,
            Literal::Str(s) => Value::Str(s.clone()),
            Literal::Number(n) => Value::Number(*n),
        })
    }

    fn evaluate_unary(
        env: Rc<RefCell<Environment>>,
        unary: &Unary,
    ) -> Result<Value, FlowInterruption> {
        let right_val = evaluate_expression(env, &unary.right)?;
        match unary.operator.typ {
            TokenType::Minus => match right_val {
                Value::Number(n) => Ok(Value::Number(-n)),
                _ => Err(FlowInterruption::RuntimeError(RuntimeError::new(
                    unary.operator.clone(),
                    "Operand must be a number.".to_string(),
                ))),
            },
            TokenType::Bang => Ok(bool_to_val(!is_truthy(&right_val))),
            _ => panic!(),
        }
    }

    fn evaluate_binary(
        env: Rc<RefCell<Environment>>,
        binary: &Binary,
    ) -> Result<Value, FlowInterruption> {
        let left_val = evaluate_expression(Rc::clone(&env), &binary.left)?;
        let right_val = evaluate_expression(Rc::clone(&env), &binary.right)?;
        match binary.operator.typ {
            TokenType::Plus => match (left_val, right_val) {
                (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x + y)),
                (Value::Str(x), Value::Str(y)) => Ok(Value::Str(format!("{}{}", x, y))),
                _ => Err(FlowInterruption::RuntimeError(RuntimeError::new(
                    binary.operator.clone(),
                    "Operands must be two numbers or two strings.".to_string(),
                ))),
            },
            TokenType::Minus => match (left_val, right_val) {
                (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x - y)),
                _ => Err(FlowInterruption::RuntimeError(make_numbers_operand_error(
                    &binary.operator,
                ))),
            },
            TokenType::Star => match (left_val, right_val) {
                (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x * y)),
                _ => Err(FlowInterruption::RuntimeError(make_numbers_operand_error(
                    &binary.operator,
                ))),
            },
            TokenType::Slash => match (left_val, right_val) {
                (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x / y)),
                _ => Err(FlowInterruption::RuntimeError(make_numbers_operand_error(
                    &binary.operator,
                ))),
            },
            TokenType::EqualEqual => Ok(bool_to_val(left_val == right_val)),
            TokenType::BangEqual => Ok(bool_to_val(left_val != right_val)),
            TokenType::LessEqual => match (left_val, right_val) {
                (Value::Number(x), Value::Number(y)) => Ok(bool_to_val(x <= y)),
                _ => Err(FlowInterruption::RuntimeError(make_numbers_operand_error(
                    &binary.operator,
                ))),
            },
            TokenType::Less => match (left_val, right_val) {
                (Value::Number(x), Value::Number(y)) => Ok(bool_to_val(x < y)),
                _ => Err(FlowInterruption::RuntimeError(make_numbers_operand_error(
                    &binary.operator,
                ))),
            },
            TokenType::GreaterEqual => match (left_val, right_val) {
                (Value::Number(x), Value::Number(y)) => Ok(bool_to_val(x >= y)),
                _ => Err(FlowInterruption::RuntimeError(make_numbers_operand_error(
                    &binary.operator,
                ))),
            },
            TokenType::Greater => match (left_val, right_val) {
                (Value::Number(x), Value::Number(y)) => Ok(bool_to_val(x > y)),
                _ => Err(FlowInterruption::RuntimeError(make_numbers_operand_error(
                    &binary.operator,
                ))),
            },
            // FIXME: this
            _ => panic!(),
        }
    }

    fn evaluate_assignment(
        env: Rc<RefCell<Environment>>,
        assignment: &Assignment,
    ) -> Result<Value, FlowInterruption> {
        let value = evaluate_expression(Rc::clone(&env), &assignment.value)?;
        Rc::clone(&env)
            .borrow_mut()
            .assign(&assignment.name, value.clone())?;
        Ok(value)
    }

    fn evaluate_logical(
        env: Rc<RefCell<Environment>>,
        logical: &Logical,
    ) -> Result<Value, FlowInterruption> {
        let left_val = evaluate_expression(Rc::clone(&env), &logical.left)?;
        match logical.operator.typ {
            TokenType::Or => {
                if is_truthy(&left_val) {
                    return Ok(left_val);
                }
                let right_val = evaluate_expression(Rc::clone(&env), &logical.right)?;
                return Ok(right_val);
            }
            TokenType::And => {
                if !is_truthy(&left_val) {
                    return Ok(left_val);
                }
                let right_val = evaluate_expression(Rc::clone(&env), &logical.right)?;
                return Ok(right_val);
            }
            // This case should not occur if parsing was done correctly
            _ => panic!(),
        }
    }
    fn evaluate_call(
        env: Rc<RefCell<Environment>>,
        call: &Call,
    ) -> Result<Value, FlowInterruption> {
        let callee = evaluate_expression(Rc::clone(&env), &call.callee)?;
        let mut arguments = vec![];
        for ast_arg in &call.arguments {
            let current_arg = evaluate_expression(Rc::clone(&env), &ast_arg)?;
            arguments.push(current_arg);
        }
        match callee {
            Value::Callable(callable) => {
                if arguments.len() != callable.arity() {
                    return Err(FlowInterruption::RuntimeError(RuntimeError::new(
                        call.paren.clone(),
                        format!(
                            "Expected {} arguments but got {}.",
                            callable.arity(),
                            arguments.len()
                        )
                        .to_string(),
                    )));
                }
                match callable.call(arguments) {
                    Err(FlowInterruption::ReturnValue(val)) => Ok(val),
                    x => x,
                }
            }
            _ => Err(FlowInterruption::RuntimeError(RuntimeError::new(
                call.paren.clone(),
                "Can only call functions and classes".to_string(),
            ))),
        }
    }

    impl Callable {
        fn arity(&self) -> usize {
            match &self {
                Callable::NativeClock => 0,
                Callable::Function(function) => function.decl.params.len(),
            }
        }
        fn call(&self, arguments: Vec<Value>) -> Result<Value, FlowInterruption> {
            match &self {
                Callable::NativeClock => {
                    let start = SystemTime::now();
                    let since_the_epoch = start
                        .duration_since(UNIX_EPOCH)
                        .expect("Time went backwards");
                    Ok(Value::Number(since_the_epoch.as_secs() as f64))
                }
                Callable::Function(function) => {
                    let call_env = Rc::new(RefCell::new(Environment::new_with_enclosing(
                        Rc::clone(&function.closure),
                    )));
                    // FIXME: use enumerate here
                    let mut index = 0;
                    for arg in arguments {
                        // NOTE - arg.len == params.len() should be checked by the caller
                        call_env
                            .borrow_mut()
                            .define(function.decl.params[index].lexeme.clone(), arg);
                        index += 1;
                    }
                    execute_block(call_env, &function.decl.body)?;
                    Ok(Value::Nil)
                }
            }
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
