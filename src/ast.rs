use crate::token::{Token, TokenType, TokenType::*};

pub struct Program {
    pub declarations: Vec<Declaration>,
}

pub enum Declaration {
    VarDecl(VarDecl),
    Statement(Statement),
}

pub struct VarDecl {
    pub identifier: Token,
    pub initializer: Option<Expr>,
}

pub enum Statement {
    ExprStmt(Expr),
    IfStmt(IfStmt),
    PrintStmt(Expr),
    WhileStmt(WhileStmt),
    Block(Vec<Declaration>),
}

pub enum Expr {
    Literal(Literal),
    Unary(Unary),
    Binary(Binary),
    Grouping(Grouping),
    Variable(Token),
    Assignment(Assignment),
    Logical(Logical),
}

pub enum Literal {
    Number(f64),
    Str(String),
    True,
    False,
    Nil,
}

pub struct Grouping {
    pub expression: Box<Expr>,
}

pub struct Unary {
    pub operator: Token,
    pub right: Box<Expr>,
}

pub struct Binary {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
}

pub struct Logical {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
}

pub struct Assignment {
    pub name: Token,
    pub value: Box<Expr>,
}

pub struct IfStmt {
    pub condition: Expr,
    pub then_branch: Box<Statement>,
    pub else_branch: Option<Box<Statement>>,
}

pub struct WhileStmt {
    pub condition: Expr,
    pub body: Box<Statement>,
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
    declaration    → varDecl
                   | statement ;
    varDecl        → "var" IDENTIFIER ( "=" expression )? ";" ;
    statement      → exprStmt
                   | ifStmt
                   | printStmt
                   | whileStmt
                   | block
    block          → "{" declaration* "}"
    exprStmt       → expression ";" ;
    ifStmt         → "if" "(" expression ")" statement
                   ( "else" statement )? ;
    whileStmt      → "while" "(" expression ")" statement;
    printStmt      → "print" expression ";" ;

    expression     → assignment ;
    assignment     → IDENTIFIER "=" assignment
                   | logic_or ;
    logic_or       → logic_and ( "or" logic_and )* ;
    logic_and      → equality ( "and" equality )* ;
    equality       → comparison ( ( "!=" | "==" ) comparison )* ;
    comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    term           → factor ( ( "-" | "+" ) factor )* ;
    factor         → unary ( ( "/" | "*" ) unary )* ;
    unary          → ( "!" | "-" ) unary
                   | primary ;
    primary        → NUMBER | STRING | "true" | "false" | "nil"
                   | "(" expression ")" | IDENTIFIER ;

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
            &self.peek().typ == typ
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
                Var => self.var_decl(),
                _ => Ok(Declaration::Statement(self.statement()?)),
            }
        }

        fn var_decl(&mut self) -> Result<Declaration, ParseError> {
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
            Ok(Declaration::VarDecl(VarDecl {
                identifier,
                initializer,
            }))
        }

        fn statement(&mut self) -> Result<Statement, ParseError> {
            let token = self.peek();
            match token.typ {
                If => Ok(Statement::IfStmt(self.if_stmt()?)),
                While => Ok(Statement::WhileStmt(self.while_stmt()?)),
                Print => {
                    self.advance(); // discard print token
                    let expr = self.expression()?;
                    self.consume(&Semicolon, "Expect ';' after value.")?;
                    Ok(Statement::PrintStmt(expr))
                }
                LeftBrace => Ok(Statement::Block(self.block()?)),
                _ => {
                    let expr = self.expression()?;
                    self.consume(&Semicolon, "Expect ';' after expression.")?;
                    Ok(Statement::ExprStmt(expr))
                }
            }
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

        fn block(&mut self) -> Result<Vec<Declaration>, ParseError> {
            self.advance(); // discard left brace
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
            self.primary()
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
                _ => Err(ParseError {
                    message: "Expect expression".to_string(),
                    token,
                }),
            }
        }
    }
}
