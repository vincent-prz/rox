use crate::token::{Token, TokenType, TokenType::*};

#[derive(Debug, PartialEq, Clone)]
pub struct Program {
    pub declarations: Vec<Declaration>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Declaration {
    ClassDecl(ClassDecl),
    FunDecl(FunDecl),
    VarDecl(VarDecl),
    Statement(Statement),
}
#[derive(Debug, PartialEq, Clone)]
pub struct ClassDecl {
    pub name: Token,
    pub methods: Vec<FunDecl>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunDecl {
    pub name: Token,
    pub params: Vec<Token>,
    pub body: Vec<Declaration>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct VarDecl {
    pub identifier: Token,
    pub initializer: Option<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    ExprStmt(Expr),
    IfStmt(IfStmt),
    PrintStmt(Expr),
    ReturnStmt(ReturnStmt),
    WhileStmt(WhileStmt),
    Block(Vec<Declaration>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Literal(Literal),
    Unary(Unary),
    Binary(Binary),
    Call(Call),
    Grouping(Grouping),
    Variable(Token),
    Assignment(Assignment),
    Logical(Logical),
    Get(Get),
    Set(Set),
    This(Token),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Number(f64),
    Str(String),
    True,
    False,
    Nil,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Call {
    pub callee: Box<Expr>,
    pub paren: Token,
    pub arguments: Vec<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Grouping {
    pub expression: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Unary {
    pub operator: Token,
    pub right: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Binary {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Logical {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Get {
    pub object: Box<Expr>,
    pub name: Token,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Set {
    pub object: Box<Expr>,
    pub name: Token,
    pub value: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Assignment {
    pub name: Token,
    pub value: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfStmt {
    pub condition: Expr,
    pub then_branch: Box<Statement>,
    pub else_branch: Option<Box<Statement>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct WhileStmt {
    pub condition: Expr,
    pub body: Box<Statement>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnStmt {
    pub token: Token,
    pub expr: Option<Expr>,
}

pub mod printer {
    use super::*;

    pub fn pretty_print(expr: &Expr) -> String {
        match expr {
            Expr::Literal(lit) => pretty_print_litteral(lit),
            Expr::Grouping(group) => pretty_print_grouping(group),
            Expr::Unary(unary) => pretty_print_unary(unary),
            Expr::Binary(binary) => pretty_print_binary(binary),
            Expr::Variable(token) => token.lexeme.clone(),
            Expr::Assignment(assignment) => pretty_print_assignment(assignment),
            Expr::Logical(logical) => pretty_print_logical(logical),
            Expr::Call(call) => pretty_print_call(call),
            Expr::Get(get) => pretty_print_get(get),
            Expr::Set(set) => pretty_print_set(set),
            Expr::This(_) => "this".to_string(),
        }
    }

    fn pretty_print_litteral(literal: &Literal) -> String {
        match literal {
            Literal::Number(n) => n.to_string(),
            Literal::Str(s) => s.clone(),
            Literal::True => "true".to_string(),
            Literal::False => "false".to_string(),
            Literal::Nil => "nil".to_string(),
        }
    }

    fn pretty_print_grouping(group: &Grouping) -> String {
        format!("(group {})", pretty_print(&group.expression))
    }

    fn pretty_print_unary(unary: &Unary) -> String {
        format!("({} {})", unary.operator.lexeme, pretty_print(&unary.right))
    }

    fn pretty_print_binary(binary: &Binary) -> String {
        format!(
            "({} {} {})",
            binary.operator.lexeme,
            pretty_print(&binary.left),
            pretty_print(&binary.right)
        )
    }

    fn pretty_print_logical(logical: &Logical) -> String {
        format!(
            "({} {} {})",
            logical.operator.lexeme,
            pretty_print(&logical.left),
            pretty_print(&logical.right)
        )
    }

    fn pretty_print_assignment(assignment: &Assignment) -> String {
        format!(
            "(= {} {})",
            assignment.name.lexeme,
            pretty_print(&assignment.value)
        )
    }

    fn pretty_print_call(call: &Call) -> String {
        // FIXME: arguments are not displayed
        format!("(call {})", pretty_print(&call.callee))
    }

    fn pretty_print_get(get: &Get) -> String {
        format!("(get {} {})", pretty_print(&get.object), get.name.lexeme)
    }

    fn pretty_print_set(set: &Set) -> String {
        format!(
            "(set {} {} {})",
            pretty_print(&set.object),
            set.name.lexeme,
            pretty_print(&set.value)
        )
    }
}

#[test]
fn test_pretty_printer() {
    let minus_op = Token {
        typ: TokenType::Minus,
        lexeme: "-".to_string(),
        line: 1,
    };
    let star_op = Token {
        typ: TokenType::Star,
        lexeme: "*".to_string(),
        line: 1,
    };
    let expression = Expr::Binary(Binary {
        left: Box::new(Expr::Unary(Unary {
            operator: minus_op,
            right: Box::new(Expr::Literal(Literal::Number(123.0))),
        })),
        operator: star_op,
        right: Box::new(Expr::Grouping(Grouping {
            expression: Box::new(Expr::Literal(Literal::Number(45.67))),
        })),
    });
    let result = printer::pretty_print(&expression);
    assert_eq!(result, "(* (- 123) (group 45.67))");
}

pub mod parser {
    use super::*;

    /*
    program        → declaration* EOF ;
    declaration    → classDecl | funDecl | varDecl | statement ;
    classDecl      → "class" IDENTIFIER "{" function* "}" ;
    funDecl        → "fun" function ;
    function       → IDENTIFIER "(" parameters? ")" block ;
    varDecl        → "var" IDENTIFIER ( "=" expression )? ";" ;
    parameters     → IDENTIFIER ( "," IDENTIFIER )* ;
    statement      → exprStmt
                   | ifStmt
                   | printStmt
                   | returnStmt
                   | whileStmt
                   | block
    block          → "{" declaration* "}"
    exprStmt       → expression ";" ;
    ifStmt         → "if" "(" expression ")" statement
                   ( "else" statement )? ;
    whileStmt      → "while" "(" expression ")" statement;
    printStmt      → "print" expression ";" ;
    returnStmt      → "return" expression? ";" ;

    expression     → assignment ;
    assignment     → ( call "." )? IDENTIFIER "=" assignment
                   | logic_or ;
    logic_or       → logic_and ( "or" logic_and )* ;
    logic_and      → equality ( "and" equality )* ;
    equality       → comparison ( ( "!=" | "==" ) comparison )* ;
    comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    term           → factor ( ( "-" | "+" ) factor )* ;
    factor         → unary ( ( "/" | "*" ) unary )* ;
    unary          → ( "!" | "-" ) unary | call
    call           → primary ( "(" arguments? ")" | "." IDENTIFIER )* ;
    primary        → NUMBER | STRING | "true" | "false" | "nil"
                   | "(" expression ")" | IDENTIFIER ;
    arguments      → expression ( "," expression )* ;

    */
    pub struct Parser {
        tokens: Vec<Token>,
        current: usize,
    }

    #[derive(Debug)]
    pub struct ParseError {
        pub message: String,
        pub token: Token,
    }

    impl Parser {
        pub fn new(tokens: Vec<Token>) -> Self {
            Self { tokens, current: 0 }
        }

        pub fn parse(&mut self) -> Result<Program, ParseError> {
            self.program()
        }

        fn peek(&self) -> &Token {
            &self.tokens[self.current]
        }

        fn is_at_end(&self) -> bool {
            self.peek().typ == TokenType::Eof
        }

        fn check(&self, typ: &TokenType) -> bool {
            if self.is_at_end() {
                return false;
            }
            match &self.peek().typ {
                // discard content of identifier for check
                Identifier(_) => match typ {
                    Identifier(_) => true,
                    _ => false,
                },
                t => t == typ,
            }
        }

        fn advance(&mut self) -> Token {
            if !self.is_at_end() {
                self.current += 1;
            }
            self.previous()
        }

        fn matches(&mut self, types: &Vec<TokenType>) -> bool {
            for typ in types {
                if self.check(typ) {
                    self.advance();
                    return true;
                }
            }
            false
        }

        fn previous(&mut self) -> Token {
            self.tokens[self.current - 1].clone()
        }

        fn consume(&mut self, typ: &TokenType, message: &str) -> Result<Token, ParseError> {
            if self.check(typ) {
                return Ok(self.advance());
            }
            Err(ParseError {
                token: self.peek().clone(),
                message: message.to_string(),
            })
        }

        fn program(&mut self) -> Result<Program, ParseError> {
            let mut declarations: Vec<Declaration> = Vec::new();
            while !self.is_at_end() {
                let decl = self.declaration()?;
                declarations.push(decl);
            }
            Ok(Program { declarations })
        }

        fn declaration(&mut self) -> Result<Declaration, ParseError> {
            let token = self.peek();
            match token.typ {
                Class => self.class_decl().map(Declaration::ClassDecl),
                Fun => self.fun_decl("function").map(Declaration::FunDecl),
                Var => self.var_decl().map(Declaration::VarDecl),
                _ => Ok(Declaration::Statement(self.statement()?)),
            }
        }

        fn class_decl(&mut self) -> Result<ClassDecl, ParseError> {
            self.advance(); // discard class token
            let lexeme = self.peek().lexeme.clone();
            // FIXME: need to copy lexeme to check Identifier type -> ugly
            let name = self.consume(&Identifier(lexeme), "Expect class name.")?;
            self.consume(&LeftBrace, &format!("Expect '{{' before class body."))?;
            let mut methods = vec![];
            while self.peek().typ != RightBrace && !self.is_at_end() {
                let meth = self.fun_decl("method")?;
                methods.push(meth);
            }
            self.consume(&RightBrace, &format!("Expect '}}' after class body."))?;
            Ok(ClassDecl { name, methods })
        }

        fn fun_decl(&mut self, kind: &str) -> Result<FunDecl, ParseError> {
            if kind == "function" {
                self.advance(); // discard fun token
            }
            // FIXME: need to create empty string to consume identifier
            let name = self.consume(
                &Identifier("".to_string()),
                &format!("Expect {} name.", kind),
            )?;
            self.consume(&LeftParen, &format!("Expect '(' after {} name.", kind))?;
            let params = self.parameters()?;
            self.consume(&LeftBrace, &format!("Expect '{{' before {} body.", kind))?;
            let body = self.block()?;
            Ok(FunDecl { name, params, body })
        }

        fn parameters(&mut self) -> Result<Vec<Token>, ParseError> {
            let mut params = vec![];
            if self.peek().typ != RightParen {
                loop {
                    let identifier =
                        self.consume(&Identifier("".to_string()), "Expect parameter name.")?;
                    params.push(identifier);
                    if !self.matches(&vec![Comma]) {
                        break;
                    }
                }
            }
            if params.len() >= 255 {
                // FIXME: we don't want the parser to enter panic mode here
                return Err(ParseError {
                    token: self.peek().clone(),
                    message: "Can't have more than 255 parameters.".to_string(),
                });
            }
            self.consume(&RightParen, "Expect ')' after parameters.");
            Ok(params)
        }

        fn var_decl(&mut self) -> Result<VarDecl, ParseError> {
            self.advance(); // discard var token
            let lexeme = self.peek().lexeme.clone();
            // FIXME: need to copy lexeme to check Identifier type -> ugly
            let identifier = self.consume(&Identifier(lexeme), "Expect variable name.")?;
            let initializer = if self.matches(&vec![Equal]) {
                Some(self.expression()?)
            } else {
                None
            };
            self.consume(&Semicolon, "Expect ';' after declaration.")?;
            Ok(VarDecl {
                identifier,
                initializer,
            })
        }

        fn statement(&mut self) -> Result<Statement, ParseError> {
            let token = self.peek();
            match token.typ {
                If => Ok(Statement::IfStmt(self.if_stmt()?)),
                While => Ok(Statement::WhileStmt(self.while_stmt()?)),
                // desugaring a for statement into while
                For => self.for_stmt(),
                Print => {
                    self.advance(); // discard print token
                    let expr = self.expression()?;
                    self.consume(&Semicolon, "Expect ';' after value.")?;
                    Ok(Statement::PrintStmt(expr))
                }
                Return => {
                    let token = self.advance(); // take return token
                    let expr = if self.peek().typ == Semicolon {
                        None
                    } else {
                        Some(self.expression()?)
                    };
                    self.consume(&Semicolon, "Expect ';' after return value.")?;
                    Ok(Statement::ReturnStmt(ReturnStmt { token, expr }))
                }
                LeftBrace => {
                    self.advance(); // discard left brace
                    Ok(Statement::Block(self.block()?))
                }
                _ => self.expr_statement(),
            }
        }

        fn expr_statement(&mut self) -> Result<Statement, ParseError> {
            let expr = self.expression()?;
            self.consume(&Semicolon, "Expect ';' after expression.")?;
            Ok(Statement::ExprStmt(expr))
        }

        fn if_stmt(&mut self) -> Result<IfStmt, ParseError> {
            self.advance(); // discard print token
            self.consume(&LeftParen, "Expect '(' after if.")?;
            let condition = self.expression()?;
            self.consume(&RightParen, "Expect ')' after if condition.")?;
            let then_branch = self.statement()?;
            let next_token = self.peek();
            let else_branch = if next_token.typ == Else {
                self.advance(); // discard else token
                Some(Box::new(self.statement()?))
            } else {
                None
            };
            Ok(IfStmt {
                condition,
                then_branch: Box::new(then_branch),
                else_branch,
            })
        }

        fn while_stmt(&mut self) -> Result<WhileStmt, ParseError> {
            self.advance(); // discard while token
            self.consume(&LeftParen, "Expect '(' after while.")?;
            let condition = self.expression()?;
            self.consume(&RightParen, "Expect ')' after while condition.")?;

            let body = self.statement()?;
            Ok(WhileStmt {
                condition,
                body: Box::new(body),
            })
        }

        fn for_stmt(&mut self) -> Result<Statement, ParseError> {
            self.advance(); // discard for token
            self.consume(&LeftParen, "Expect '(' after for.")?;
            let token = self.peek();
            let initializer = match token.typ {
                Semicolon => {
                    self.advance(); // discard semi colon
                    None
                }
                Var => Some(self.var_decl()?).map(Declaration::VarDecl),
                _ => Some(Declaration::Statement(self.expr_statement()?)),
            };
            let condition = match self.peek().typ {
                Semicolon => None,
                _ => Some(self.expression()?),
            };
            self.consume(&Semicolon, "Expect ';' after loop condition.")?;
            let increment = match self.peek().typ {
                RightParen => None,
                _ => Some(self.expression()?),
            };
            self.consume(&RightParen, "Expect ')' after for clauses.")?;
            let body = self.statement()?;

            let full_body = match increment {
                None => body,
                Some(incr) => Statement::Block(vec![
                    Declaration::Statement(body),
                    Declaration::Statement(Statement::ExprStmt(incr)),
                ]),
            };

            let while_stmt = WhileStmt {
                condition: match condition {
                    Some(cond) => cond,
                    None => Expr::Literal(Literal::True),
                },
                body: Box::new(full_body),
            };
            Ok(match initializer {
                None => Statement::WhileStmt(while_stmt),
                Some(var_decl) => Statement::Block(vec![
                    var_decl,
                    Declaration::Statement(Statement::WhileStmt(while_stmt)),
                ]),
            })
        }

        // FIXME disagreeing with the book here - it seems we want to return Declaration
        // and not Statements here ? See page 130
        fn block(&mut self) -> Result<Vec<Declaration>, ParseError> {
            // assumption: left brace has already been consumed
            let mut result = vec![];
            while self.peek().typ != RightBrace && !self.is_at_end() {
                let decl = self.declaration()?;
                result.push(decl);
            }
            self.consume(&RightBrace, "Expect '}' after block.")?;
            Ok(result)
        }

        // NOTE - letting this function public to allow unit testing of expression parsing and evaluation.
        pub fn expression(&mut self) -> Result<Expr, ParseError> {
            self.assignment()
        }

        fn assignment(&mut self) -> Result<Expr, ParseError> {
            let expr = self.logic_or()?;
            if self.matches(&vec![Equal]) {
                let equals = self.previous();
                let value = self.assignment()?;
                return match expr {
                    Expr::Variable(name) => Ok(Expr::Assignment(Assignment {
                        name,
                        value: Box::new(value),
                    })),
                    Expr::Get(Get { object, name }) => Ok(Expr::Set(Set {
                        object,
                        name,
                        value: Box::new(value),
                    })),
                    // FIXME: should keep parsing here
                    _ => Err(ParseError {
                        token: equals,
                        message: "Invalid assignment target.".to_string(),
                    }),
                };
            }
            Ok(expr)
        }

        fn logic_or(&mut self) -> Result<Expr, ParseError> {
            let mut expr = self.logic_and()?;
            while self.matches(&vec![Or]) {
                let operator = self.previous();
                let right = self.logic_and()?;
                expr = Expr::Logical(Logical {
                    left: Box::new(expr),
                    operator,
                    right: Box::new(right),
                });
            }
            Ok(expr)
        }

        fn logic_and(&mut self) -> Result<Expr, ParseError> {
            let mut expr = self.equality()?;
            while self.matches(&vec![And]) {
                let operator = self.previous();
                let right = self.equality()?;
                expr = Expr::Logical(Logical {
                    left: Box::new(expr),
                    operator,
                    right: Box::new(right),
                });
            }
            Ok(expr)
        }

        /*
         * Parse something of the form `<rule>((<operators>)<rule>)*`
         */
        fn parse_left_associative_binary_op(
            &mut self,
            rule: &dyn Fn(&mut Self) -> Result<Expr, ParseError>,
            operators: &Vec<TokenType>,
        ) -> Result<Expr, ParseError> {
            let mut expr = rule(self)?;
            while self.matches(operators) {
                let operator = self.previous();
                let right = rule(self)?;
                expr = Expr::Binary(Binary {
                    left: Box::new(expr),
                    operator,
                    right: Box::new(right),
                });
            }
            Ok(expr)
        }

        fn equality(&mut self) -> Result<Expr, ParseError> {
            self.parse_left_associative_binary_op(&Parser::comparison, &vec![BangEqual, EqualEqual])
        }

        fn comparison(&mut self) -> Result<Expr, ParseError> {
            self.parse_left_associative_binary_op(
                &Parser::term,
                &vec![Less, LessEqual, Greater, GreaterEqual],
            )
        }

        fn term(&mut self) -> Result<Expr, ParseError> {
            self.parse_left_associative_binary_op(&Parser::factor, &vec![Minus, Plus])
        }

        fn factor(&mut self) -> Result<Expr, ParseError> {
            self.parse_left_associative_binary_op(&Parser::unary, &vec![Slash, Star])
        }

        fn unary(&mut self) -> Result<Expr, ParseError> {
            if self.matches(&vec![Minus, Bang]) {
                let operator = self.previous();
                let right = self.unary()?;
                return Ok(Expr::Unary(Unary {
                    operator,
                    right: Box::new(right),
                }));
            }
            self.call()
        }

        fn call(&mut self) -> Result<Expr, ParseError> {
            let mut result = self.primary()?;
            loop {
                if self.peek().typ == LeftParen {
                    let paren = self.advance();
                    let arguments = self.arguments()?;
                    result = Expr::Call(Call {
                        callee: Box::new(result),
                        paren,
                        arguments,
                    });
                    self.consume(&RightParen, "Expect ')' after arguments.")?;
                } else if self.peek().typ == Dot {
                    self.advance(); // discard dot
                    let name = self.consume(
                        &Identifier("".to_string()),
                        "Expect property name after '.'.",
                    )?;
                    result = Expr::Get(Get {
                        object: Box::new(result),
                        name,
                    });
                } else {
                    break;
                }
            }
            Ok(result)
        }

        fn primary(&mut self) -> Result<Expr, ParseError> {
            let token = self.advance();
            match token.typ {
                Str(s) => Ok(Expr::Literal(Literal::Str(s))),
                Number(x) => Ok(Expr::Literal(Literal::Number(x))),
                True => Ok(Expr::Literal(Literal::True)),
                False => Ok(Expr::Literal(Literal::False)),
                Nil => Ok(Expr::Literal(Literal::Nil)),
                LeftParen => {
                    let expr = self.expression()?;
                    let next_token = self.advance();
                    if next_token.typ != RightParen {
                        return Err(ParseError {
                            message: "Expect ')' after expression.".to_string(),
                            token: next_token,
                        });
                    }
                    Ok(Expr::Grouping(Grouping {
                        expression: (Box::new(expr)),
                    }))
                }
                Identifier(_) => Ok(Expr::Variable(token)),
                This => Ok(Expr::This(token)),
                _ => Err(ParseError {
                    message: "Expect expression".to_string(),
                    token,
                }),
            }
        }

        fn arguments(&mut self) -> Result<Vec<Expr>, ParseError> {
            let mut arguments = vec![];
            if self.peek().typ != RightParen {
                loop {
                    let expr = self.expression()?;
                    arguments.push(expr);
                    if !self.matches(&vec![Comma]) {
                        break;
                    }
                }
            }
            if arguments.len() >= 255 {
                // FIXME: we don't want the parser to enter panic mode here
                return Err(ParseError {
                    token: self.peek().clone(),
                    message: "Can't have more than 255 arguments.".to_string(),
                });
            }
            Ok(arguments)
        }
    }
}
