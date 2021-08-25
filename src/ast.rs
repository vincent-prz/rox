use crate::token::{Token, TokenType};

enum Expr {
    Literal(Literal),
    Unary(Unary),
    Binary(Binary),
    Grouping(Grouping),
}

enum Literal {
    Number(f64),
    String(String),
    True,
    False,
    Nil,
}

struct Grouping {
    expression: Box<Expr>,
}

struct Unary {
    operator: Token,
    right: Box<Expr>,
}

struct Binary {
    left: Box<Expr>,
    operator: Token,
    right: Box<Expr>,
}

fn pretty_print(expr: &Expr) -> String {
    match expr {
        Expr::Literal(lit) => pretty_print_litteral(lit),
        Expr::Grouping(group) => pretty_print_grouping(group),
        Expr::Unary(unary) => pretty_print_unary(unary),
        Expr::Binary(binary) => pretty_print_binary(binary),
    }
}

fn pretty_print_litteral(literal: &Literal) -> String {
    match literal {
        Literal::Number(n) => n.to_string(),
        Literal::String(s) => s.clone(),
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
    format!("({} {} {})", binary.operator.lexeme, pretty_print(&binary.left), pretty_print(&binary.right))
}

#[test]
fn test_pretty_printer() {
    let minus_op = Token { typ: TokenType::Minus, lexeme: "-".to_string(), line: 1 };
    let star_op = Token { typ: TokenType::Star, lexeme: "*".to_string(), line: 1 };
    let expression = Expr::Binary(
        Binary {
            left: Box::new(Expr::Unary(Unary { operator: minus_op, right: Box::new(Expr::Literal(Literal::Number(123.0)))})),
            operator: star_op,
            right: Box::new(Expr::Grouping(Grouping { expression: Box::new(Expr::Literal(Literal::Number(45.67)))})),
        }
    );
    let result = pretty_print(&expression);
    assert_eq!(result, "(* (- 123) (group 45.67))");
}