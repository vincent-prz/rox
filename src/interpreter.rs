use crate::ast::{
    Assignment, Binary, Call, ClassDecl, Declaration, Expr, FunDecl, Get, IfStmt, Literal, Logical,
    Program, Set, Statement, Unary, VarDecl, WhileStmt,
};
use crate::token::{Token, TokenType};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

// TODO: split this file
#[derive(Debug, Clone)]
pub enum Value {
    Nil,
    True,
    False,
    Str(String),
    Number(f64),
    Callable(Callable),
    ClassInstance(Rc<RefCell<ClassInstance>>),
}

#[derive(Debug, Clone)]
pub enum Callable {
    NativeClock,
    Function(Function),
    Class(Class),
}

#[derive(Debug, Clone)]
pub struct Class {
    name: String,
    methods: HashMap<String, Function>,
}

impl Class {
    fn make_new_instance(&self) -> ClassInstance {
        ClassInstance::new(self.clone())
    }
}

#[derive(Debug, Clone)]
pub struct ClassInstance {
    class: Class,
    fields: HashMap<String, Rc<RefCell<Value>>>,
}

// NOTE - can not make this a method of ClassInstance, because we need self to be a Rc RefCell
// Indeed method calls can mutate the instance.
fn get_from_class_instance(
    instance: Rc<RefCell<ClassInstance>>,
    name: &Token,
) -> Result<Rc<RefCell<Value>>, FlowInterruption> {
    if let Some(val) = instance.borrow().fields.get(&name.lexeme) {
        return Ok(Rc::clone(val));
    }
    if let Some(method) = instance.borrow().class.methods.get(&name.lexeme) {
        return Ok(Rc::new(RefCell::new(Value::Callable(Callable::Function(
            method.clone().bind(Rc::clone(&instance)),
        )))));
    }
    Err(FlowInterruption::RuntimeError(RuntimeError::new(
        name.clone(),
        format!("Undefined property {}.", name.lexeme),
    )))
}

impl ClassInstance {
    fn new(class: Class) -> Self {
        Self {
            class,
            fields: HashMap::new(),
        }
    }

    fn set(&mut self, name: &Token, value: Rc<RefCell<Value>>) {
        self.fields.insert(name.lexeme.clone(), value);
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    decl: FunDecl,
    closure: Rc<RefCell<Environment>>,
    is_initializer: bool,
}

impl Function {
    fn arity(&self) -> usize {
        self.decl.params.len()
    }

    fn bind(&self, instance: Rc<RefCell<ClassInstance>>) -> Self {
        let mut env = Environment::new_with_enclosing(Rc::clone(&self.closure));
        env.define(
            "this".to_string(),
            Rc::new(RefCell::new(Value::ClassInstance(instance))),
        );
        Function {
            decl: self.decl.clone(),
            closure: Rc::new(RefCell::new(env)),
            is_initializer: self.decl.name.lexeme == "init",
        }
    }
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
            Value::ClassInstance(instance) => {
                write!(f, "{} instance", instance.borrow().class.name)
            }
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
            Callable::Class(class) => write!(f, "{}", class.name),
        }
    }
}

#[derive(Debug)]
pub enum FlowInterruption {
    ReturnValue(Rc<RefCell<Value>>),
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
struct Environment {
    values: HashMap<String, Rc<RefCell<Value>>>,
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

    fn define(&mut self, name: String, value: Rc<RefCell<Value>>) {
        self.values.insert(name, value);
    }

    fn assign(&mut self, name: &Token, value: Rc<RefCell<Value>>) -> Result<(), FlowInterruption> {
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

    fn get(&self, name: &Token) -> Result<Rc<RefCell<Value>>, FlowInterruption> {
        match self.values.get(&name.lexeme) {
            Some(value) => Ok(Rc::clone(value)),
            None => match &self.enclosing {
                Some(enclosing) => enclosing.borrow().get(name),
                None => Err(FlowInterruption::RuntimeError(RuntimeError::new(
                    name.clone(),
                    format!("Undefined variable {}.", name.lexeme),
                ))),
            },
        }
    }

    fn get_at(&self, name: &str) -> Option<Rc<RefCell<Value>>> {
        self.values.get(name).map(|val| Rc::clone(val))
    }
}

fn get_default_globals() -> Environment {
    let mut globals = Environment::new();
    globals.define(
        "clock".to_string(),
        Rc::new(RefCell::new(Value::Callable(Callable::NativeClock))),
    );
    globals
}

pub struct Interpreter {
    environment: Rc<RefCell<Environment>>,
}

impl Interpreter {
    pub fn new() -> Self {
        let globals = Rc::new(RefCell::new(get_default_globals()));
        let environment = Rc::clone(&globals);
        Self { environment }
    }

    pub fn interpret(&mut self, program: &Program) -> Result<(), FlowInterruption> {
        self.execute_program(program)
    }

    fn execute_program(&mut self, program: &Program) -> Result<(), FlowInterruption> {
        for decl in &program.declarations {
            self.execute_declaration(&decl)?;
        }
        Ok(())
    }

    fn execute_declaration(&mut self, decl: &Declaration) -> Result<(), FlowInterruption> {
        match decl {
            Declaration::ClassDecl(class_decl) => self.execute_class_decl(class_decl),
            Declaration::FunDecl(fun_decl) => self.execute_fun_decl(fun_decl),
            Declaration::VarDecl(var_decl) => self.execute_var_decl(var_decl),
            Declaration::Statement(stmt) => self.execute_statement(stmt),
        }
    }

    fn execute_class_decl(&mut self, decl: &ClassDecl) -> Result<(), FlowInterruption> {
        let name = decl.name.lexeme.clone();
        let mut methods = HashMap::new();
        for ast_method in &decl.methods {
            methods.insert(
                ast_method.name.lexeme.clone(),
                self.parse_fun_decl(ast_method),
            );
        }
        self.environment.borrow_mut().define(
            name.clone(),
            Rc::new(RefCell::new(Value::Callable(Callable::Class(Class {
                name,
                methods,
            })))),
        );
        Ok(())
    }

    fn execute_fun_decl(&mut self, decl: &FunDecl) -> Result<(), FlowInterruption> {
        self.environment.borrow_mut().define(
            decl.name.lexeme.clone(),
            Rc::new(RefCell::new(Value::Callable(Callable::Function(
                self.parse_fun_decl(decl),
            )))),
        );
        Ok(())
    }

    fn parse_fun_decl(&self, decl: &FunDecl) -> Function {
        Function {
            decl: decl.clone(),
            closure: Rc::clone(&self.environment),
            is_initializer: false,
        }
    }

    fn execute_var_decl(&mut self, decl: &VarDecl) -> Result<(), FlowInterruption> {
        let varname = decl.identifier.lexeme.clone();
        let value = match &decl.initializer {
            None => Rc::new(RefCell::new(Value::Nil)),
            Some(expr) => self.evaluate_expression(&expr)?,
        };
        self.environment.borrow_mut().define(varname, value);
        Ok(())
    }

    fn execute_statement(&mut self, stmt: &Statement) -> Result<(), FlowInterruption> {
        match stmt {
            Statement::PrintStmt(expr) => {
                let value = self.evaluate_expression(&expr)?;
                println!("{}", value.borrow());
            }
            Statement::ReturnStmt(option_expr) => {
                let return_value = match option_expr {
                    None => Rc::new(RefCell::new(Value::Nil)),
                    Some(expr) => self.evaluate_expression(expr)?,
                };
                // propagating return value as error to make sure the `return` statement
                // interrupts the function flow
                return Err(FlowInterruption::ReturnValue(return_value));
            }
            Statement::ExprStmt(expr) => {
                self.evaluate_expression(&expr)?;
            }
            Statement::Block(declarations) => {
                let block_env = Rc::new(RefCell::new(Environment::new_with_enclosing(Rc::clone(
                    &self.environment,
                ))));
                self.execute_block(declarations, block_env)?;
            }
            Statement::IfStmt(IfStmt {
                condition,
                then_branch,
                else_branch,
            }) => {
                if is_truthy(&self.evaluate_expression(condition)?.borrow()) {
                    self.execute_statement(then_branch)?;
                } else {
                    match else_branch {
                        None => {}
                        Some(statement) => self.execute_statement(statement)?,
                    }
                }
            }
            Statement::WhileStmt(while_stmt) => self.execute_while_statement(while_stmt)?,
        }
        Ok(())
    }

    fn execute_block(
        &mut self,
        declarations: &Vec<Declaration>,
        environment: Rc<RefCell<Environment>>,
    ) -> Result<(), FlowInterruption> {
        let previous = Rc::clone(&self.environment);
        self.environment = environment;
        for decl in declarations {
            match self.execute_declaration(decl) {
                Ok(()) => {}
                Err(err) => {
                    // make sure previous env is restored if a decclaration returns an error
                    self.environment = previous;
                    return Err(err);
                }
            }
        }
        self.environment = previous;
        Ok(())
    }

    fn execute_while_statement(&mut self, while_stmt: &WhileStmt) -> Result<(), FlowInterruption> {
        let condition = &while_stmt.condition;
        let body = &while_stmt.body;
        while is_truthy(&self.evaluate_expression(&condition)?.borrow()) {
            self.execute_statement(&body)?;
        }
        Ok(())
    }

    // NOTE - public REPL
    pub fn evaluate_expression(
        &mut self,
        expr: &Expr,
    ) -> Result<Rc<RefCell<Value>>, FlowInterruption> {
        match expr {
            Expr::Literal(lit) => self.evaluate_literal(lit),
            Expr::Unary(unary) => self.evaluate_unary(unary),
            Expr::Binary(binary) => self.evaluate_binary(binary),
            Expr::Grouping(group) => self.evaluate_expression(&group.expression),
            Expr::Variable(name) => self.environment.borrow().get(name),
            Expr::Assignment(assignment) => self.evaluate_assignment(assignment),
            Expr::Logical(logical) => self.evaluate_logical(logical),
            Expr::Call(call) => self.evaluate_call(call),
            Expr::Get(get) => self.evaluate_get(get),
            Expr::Set(set) => self.evaluate_set(set),
            Expr::This(name) => self.environment.borrow().get(name),
        }
    }

    fn evaluate_literal(&self, lit: &Literal) -> Result<Rc<RefCell<Value>>, FlowInterruption> {
        Ok(Rc::new(RefCell::new(match lit {
            Literal::Nil => Value::Nil,
            Literal::True => Value::True,
            Literal::False => Value::False,
            Literal::Str(s) => Value::Str(s.clone()),
            Literal::Number(n) => Value::Number(*n),
        })))
    }

    fn evaluate_unary(&mut self, unary: &Unary) -> Result<Rc<RefCell<Value>>, FlowInterruption> {
        let right_val = self.evaluate_expression(&unary.right)?;
        match unary.operator.typ {
            TokenType::Minus => match *right_val.borrow() {
                Value::Number(n) => Ok(Rc::new(RefCell::new(Value::Number(-n)))),
                _ => Err(FlowInterruption::RuntimeError(RuntimeError::new(
                    unary.operator.clone(),
                    "Operand must be a number.".to_string(),
                ))),
            },
            TokenType::Bang => Ok(Rc::new(RefCell::new(bool_to_val(!is_truthy(
                &right_val.borrow(),
            ))))),
            _ => panic!(),
        }
    }

    fn evaluate_binary(&mut self, binary: &Binary) -> Result<Rc<RefCell<Value>>, FlowInterruption> {
        let left_tmp = self.evaluate_expression(&binary.left)?;
        let left_val = left_tmp.borrow();
        let right_tmp = self.evaluate_expression(&binary.right)?;
        let right_val = right_tmp.borrow();
        match binary.operator.typ {
            TokenType::Plus => match (&*left_val, &*right_val) {
                (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x + y)),
                (Value::Str(x), Value::Str(y)) => Ok(Value::Str(format!("{}{}", x, y))),
                _ => Err(FlowInterruption::RuntimeError(RuntimeError::new(
                    binary.operator.clone(),
                    "Operands must be two numbers or two strings.".to_string(),
                ))),
            },
            TokenType::Minus => match (&*left_val, &*right_val) {
                (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x - y)),
                _ => Err(FlowInterruption::RuntimeError(make_numbers_operand_error(
                    &binary.operator,
                ))),
            },
            TokenType::Star => match (&*left_val, &*right_val) {
                (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x * y)),
                _ => Err(FlowInterruption::RuntimeError(make_numbers_operand_error(
                    &binary.operator,
                ))),
            },
            TokenType::Slash => match (&*left_val, &*right_val) {
                (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x / y)),
                _ => Err(FlowInterruption::RuntimeError(make_numbers_operand_error(
                    &binary.operator,
                ))),
            },
            TokenType::EqualEqual => Ok(bool_to_val(&*left_val == &*right_val)),
            TokenType::BangEqual => Ok(bool_to_val(&*left_val != &*right_val)),
            TokenType::LessEqual => match (&*left_val, &*right_val) {
                (Value::Number(x), Value::Number(y)) => Ok(bool_to_val(x <= y)),
                _ => Err(FlowInterruption::RuntimeError(make_numbers_operand_error(
                    &binary.operator,
                ))),
            },
            TokenType::Less => match (&*left_val, &*right_val) {
                (Value::Number(x), Value::Number(y)) => Ok(bool_to_val(x < y)),
                _ => Err(FlowInterruption::RuntimeError(make_numbers_operand_error(
                    &binary.operator,
                ))),
            },
            TokenType::GreaterEqual => match (&*left_val, &*right_val) {
                (Value::Number(x), Value::Number(y)) => Ok(bool_to_val(x >= y)),
                _ => Err(FlowInterruption::RuntimeError(make_numbers_operand_error(
                    &binary.operator,
                ))),
            },
            TokenType::Greater => match (&*left_val, &*right_val) {
                (Value::Number(x), Value::Number(y)) => Ok(bool_to_val(x > y)),
                _ => Err(FlowInterruption::RuntimeError(make_numbers_operand_error(
                    &binary.operator,
                ))),
            },
            // FIXME: this
            _ => panic!(),
        }
        .map(|result| Rc::new(RefCell::new(result)))
    }

    fn evaluate_assignment(
        &mut self,
        assignment: &Assignment,
    ) -> Result<Rc<RefCell<Value>>, FlowInterruption> {
        let value = self.evaluate_expression(&assignment.value)?;
        Rc::clone(&self.environment)
            .borrow_mut()
            .assign(&assignment.name, value.clone())?;
        Ok(value)
    }

    fn evaluate_logical(
        &mut self,
        logical: &Logical,
    ) -> Result<Rc<RefCell<Value>>, FlowInterruption> {
        let left_val = self.evaluate_expression(&logical.left)?;
        match logical.operator.typ {
            TokenType::Or => {
                if is_truthy(&left_val.borrow()) {
                    return Ok(left_val);
                }
                let right_val = self.evaluate_expression(&logical.right)?;
                return Ok(right_val);
            }
            TokenType::And => {
                if !is_truthy(&left_val.borrow()) {
                    return Ok(left_val);
                }
                let right_val = self.evaluate_expression(&logical.right)?;
                return Ok(right_val);
            }
            // This case should not occur if parsing was done correctly
            _ => panic!(),
        }
    }

    fn evaluate_call(&mut self, call: &Call) -> Result<Rc<RefCell<Value>>, FlowInterruption> {
        let tmp = self.evaluate_expression(&call.callee)?;
        let callee = tmp.borrow();
        let mut arguments = vec![];
        for ast_arg in &call.arguments {
            let current_arg = self.evaluate_expression(&ast_arg)?;
            arguments.push(current_arg);
        }
        match &*callee {
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
                match self.perform_call(&callable, arguments) {
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

    fn perform_call(
        &mut self,
        callable: &Callable,
        arguments: Vec<Rc<RefCell<Value>>>,
    ) -> Result<Rc<RefCell<Value>>, FlowInterruption> {
        match callable {
            Callable::NativeClock => {
                let start = SystemTime::now();
                let since_the_epoch = start
                    .duration_since(UNIX_EPOCH)
                    .expect("Time went backwards");
                let value = Value::Number(since_the_epoch.as_secs() as f64);
                Ok(Rc::new(RefCell::new(value)))
            }
            Callable::Function(function) => self.perform_function_call(function, arguments),
            Callable::Class(class) => {
                let new_instance = Rc::new(RefCell::new(class.make_new_instance()));
                if let Some(initializer) = class.methods.get("init") {
                    let bound_initializer = initializer.bind(Rc::clone(&new_instance));
                    self.perform_function_call(&bound_initializer, arguments)?;
                }
                let value = Value::ClassInstance(new_instance);
                Ok(Rc::new(RefCell::new(value)))
            }
        }
    }

    fn perform_function_call(
        &mut self,
        function: &Function,
        arguments: Vec<Rc<RefCell<Value>>>,
    ) -> Result<Rc<RefCell<Value>>, FlowInterruption> {
        let call_env = Rc::new(RefCell::new(Environment::new_with_enclosing(Rc::clone(
            &function.closure,
        ))));
        // FIXME: use enumerate here
        let mut index = 0;
        for arg in arguments {
            // NOTE - arg.len == params.len() should be checked by the caller
            call_env
                .borrow_mut()
                .define(function.decl.params[index].lexeme.clone(), arg);
            index += 1;
        }
        self.execute_block(&function.decl.body, call_env)?;
        if function.is_initializer {
            // if in constructor, we implicitely return this, if not done by the user
            match function.closure.borrow().get_at("this") {
                Some(val) => return Ok(val),
                None => panic!("Could not find this in constructor!"),
            }
        }
        Ok(Rc::new(RefCell::new(Value::Nil)))
    }

    fn evaluate_get(&mut self, get: &Get) -> Result<Rc<RefCell<Value>>, FlowInterruption> {
        let tmp = self.evaluate_expression(&get.object)?;
        let object = tmp.borrow();
        match &*object {
            Value::ClassInstance(instance) => {
                get_from_class_instance(Rc::clone(instance), &get.name)
            }
            _ => Err(FlowInterruption::RuntimeError(RuntimeError::new(
                get.name.clone(),
                "Only instances have properties.".to_string(),
            ))),
        }
    }

    fn evaluate_set(&mut self, set: &Set) -> Result<Rc<RefCell<Value>>, FlowInterruption> {
        let tmp = self.evaluate_expression(&set.object)?;
        let object = tmp.borrow();
        match &*object {
            Value::ClassInstance(instance) => {
                let value = self.evaluate_expression(&set.value)?;
                instance.borrow_mut().set(&set.name, Rc::clone(&value));
                return Ok(value);
            }
            _ => Err(FlowInterruption::RuntimeError(RuntimeError::new(
                set.name.clone(),
                "Only instances have fields.".to_string(),
            ))),
        }
    }
}

impl Callable {
    fn arity(&self) -> usize {
        match &self {
            Callable::NativeClock => 0,
            Callable::Function(function) => function.arity(),
            Callable::Class(class) => match class.methods.get("init") {
                Some(function) => function.arity(),
                None => 0,
            },
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
