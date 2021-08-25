use crate::token::{Token, TokenType};

pub struct Scanner {
    source: String,
    tokens: Vec<Token>,
    errors: Vec<ScannerError>,
    start: u32,
    current: u32,
    line: u32,
}

impl Scanner {
    pub fn new(source: String) -> Self {
        Self {
            source,
            tokens: vec![],
            errors: vec![],
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_tokens(mut self) -> Result<Vec<Token>, Vec<ScannerError>> {
        while !self.is_at_end() {
            // we are at the beginning of the next lexeme
            self.start = self.current;
            self.scan_token();
        }
        if self.errors.len() > 0 {
            return Err(self.errors);
        }
        self.tokens.push(Token {typ: TokenType::Eof, lexeme: String::from(""), line: self.line});
        Ok(self.tokens)
    }

    fn scan_token(&mut self) {
        let c: char = self.advance();
        match c {
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            '{' => self.add_token(TokenType::LeftBrace),
            '}' => self.add_token(TokenType::RightBrace),
            ',' => self.add_token(TokenType::Comma),
            '.' => self.add_token(TokenType::Dot),
            '-' => self.add_token(TokenType::Minus),
            '+' => self.add_token(TokenType::Plus),
            ';' => self.add_token(TokenType::Semicolon),
            '*' => self.add_token(TokenType::Star),
            '!' => {
                let token_type = if self.current_match('=') { TokenType::BangEqual } else { TokenType::Bang };
                self.add_token(token_type);
            },
            '=' => {
                let token_type = if self.current_match('=') { TokenType::EqualEqual } else { TokenType::Equal };
                self.add_token(token_type);
            },
            '>' => {
                let token_type = if self.current_match('=') { TokenType::GreaterEqual } else { TokenType::Greater };
                self.add_token(token_type);
            },
            '<' => {
                let token_type = if self.current_match('=') { TokenType::LessEqual } else { TokenType::Less };
                self.add_token(token_type);
            },
            '/' => {
                // handling comments
                if self.current_match('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(TokenType::Slash);
                }
            },
            '"' => self.string(),
            ' ' => (),
            '\t' => (),
            '\r' => (),
            '\n' => self.increment_line(),
            _ => {
                if c.is_digit(10) {
                    self.number();
                } else if c.is_alphabetic() {
                    self.identifier();
                } else {
                    self.add_error(format!("Unexpected character: {}.", c))
                }
            }
        }
    }

    fn add_token(&mut self, typ: TokenType) {
        let text = &self.source[self.start as usize..self.current as usize];
        self.tokens.push(Token {typ, lexeme: text.to_string(), line: self.line});
    }

    fn peek(&self) -> char {
        // FIXME: write a test for end of file case
        if self.is_at_end() {
            return '\0';
        }
        self.source.chars().nth(self.current as usize).expect("Couldn't peek character from source")
    }

    fn peek_next(&self) -> char {
        // NOTE: not great, duplicating code from peek
        // one alternative is to do `current++, peek, current--`, but it would require to declare self as mutable
        if self.current as usize + 1 >= self.source.len() {
            return '\0';
        }
        self.source.chars().nth(self.current as usize + 1).expect("Couldn't peek character from source")
    }

    fn advance(&mut self) -> char {
        let c = self.peek();
        self.current += 1;
        c
    }

    fn is_at_end(&self) -> bool {
        self.current as usize >= self.source.len()
    }

    fn add_error(&mut self, message: String) {
        self.errors.push(ScannerError { message, line: self.line});
    }

    fn increment_line(&mut self) {
        self.line += 1;
    }

    fn current_match(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        let c = self.peek();
        if c != expected {
            return false
        }
        self.current += 1;
        true
    }

    fn string(&mut self) {
        let mut c = self.advance();
        let mut nb_lines = 0;
        while c != '"' {
            if self.is_at_end() {
                self.add_error(String::from("Unterminated string."));
                break;
            }
            if c == '\n' {
                nb_lines += 1;
            }
            c = self.advance();
        }

        let string_literal = &self.source[self.start as usize + 1..self.current as usize - 1];
        self.add_token(TokenType::String(string_literal.to_string()));
        // multi line string: need to increment the lines after recording the token, because we want the string to be recorded
        // with line where it started.
        for _ in 0..nb_lines {
            self.increment_line();
        }
    }

    fn number(&mut self) {
        while self.peek().is_digit(10) {
            self.advance();
        }
        // if there is a dot followed by a digit, then we have a float
        // else it's an int followed by a dot.
        if self.peek() == '.' && self.peek_next().is_digit(10) {
            self.advance();
            while self.peek().is_digit(10) {
                self.advance();
            }
        }
        let string_number = &self.source[self.start as usize..self.current as usize];
        let literal_value = string_number.parse::<f64>().expect(&format!("Could not parse float: {}", string_number));
        self.add_token(TokenType::Number(literal_value));
    }

    fn identifier(&mut self) {
        while self.peek().is_alphanumeric() {
            self.advance();
        }
        // FIXME: duplicated with code in add_token
        let identifier = &self.source[self.start as usize..self.current as usize];
        match identifier {
            // reserved keywords
            "and" => self.add_token(TokenType::And),
            "class" => self.add_token(TokenType::Class),
            "else" => self.add_token(TokenType::Else),
            "false" => self.add_token(TokenType::False),
            "fun" => self.add_token(TokenType::Fun),
            "for" => self.add_token(TokenType::For),
            "if" => self.add_token(TokenType::If),
            "nil" => self.add_token(TokenType::Nil),
            "or" => self.add_token(TokenType::Or),
            "print" => self.add_token(TokenType::Print),
            "return" => self.add_token(TokenType::Return),
            "super" => self.add_token(TokenType::Super),
            "this" => self.add_token(TokenType::This),
            "true" => self.add_token(TokenType::True),
            "var" => self.add_token(TokenType::Var),
            "while" => self.add_token(TokenType::While),
            // bare identifier
            _ => self.add_token(TokenType::Identifier(identifier.to_string()))

        }
    }
}

#[derive(Debug, PartialEq)]
pub struct ScannerError {
    message: String,
    line: u32
}

#[cfg(test)]
mod scanner_tests {
    use super::*;

    #[test]
    fn empty_program() {
        let scanner = Scanner::new(String::from(""));
        let result = scanner.scan_tokens().unwrap();
        let expected = vec![Token { typ: TokenType::Eof, lexeme: String::from(""), line: 1 }];
        assert_eq!(result, expected);
    }

    #[test]
    fn simple_parens() {
        let scanner = Scanner::new(String::from("()"));
        let result = scanner.scan_tokens().unwrap();
        let expected = vec![
            Token { typ: TokenType::LeftParen, lexeme: String::from("("), line: 1 },
            Token { typ: TokenType::RightParen, lexeme: String::from(")"), line: 1 },
            Token { typ: TokenType::Eof, lexeme: String::from(""), line: 1 },
            ];
        assert_eq!(result, expected);
    }

    #[test]
    fn multi_line_parens() {
        let scanner = Scanner::new(String::from("()\n()"));
        let result = scanner.scan_tokens().unwrap();
        let expected = vec![
            Token { typ: TokenType::LeftParen, lexeme: String::from("("), line: 1 },
            Token { typ: TokenType::RightParen, lexeme: String::from(")"), line: 1 },
            Token { typ: TokenType::LeftParen, lexeme: String::from("("), line: 2 },
            Token { typ: TokenType::RightParen, lexeme: String::from(")"), line: 2 },
            Token { typ: TokenType::Eof, lexeme: String::from(""), line: 2 },
            ];
        assert_eq!(result, expected);
    }

    #[test]
    fn simple_comment() {
        let scanner = Scanner::new(String::from("// this is a comment\n()"));
        let result = scanner.scan_tokens().unwrap();
        let expected = vec![
            Token { typ: TokenType::LeftParen, lexeme: String::from("("), line: 2 },
            Token { typ: TokenType::RightParen, lexeme: String::from(")"), line: 2 },
            Token { typ: TokenType::Eof, lexeme: String::from(""), line: 2 },
            ];
        assert_eq!(result, expected);
    }

    #[test]
    fn bang_vs_bang_equal() {
        let scanner = Scanner::new(String::from("!()!="));
        let result = scanner.scan_tokens().unwrap();
        let expected = vec![
            Token { typ: TokenType::Bang, lexeme: String::from("!"), line: 1 },
            Token { typ: TokenType::LeftParen, lexeme: String::from("("), line: 1 },
            Token { typ: TokenType::RightParen, lexeme: String::from(")"), line: 1 },
            Token { typ: TokenType::BangEqual, lexeme: String::from("!="), line: 1 },
            Token { typ: TokenType::Eof, lexeme: String::from(""), line: 1 },
            ];
        assert_eq!(result, expected);
    }

    #[test]
    fn ignore_whitespace() {
        let scanner = Scanner::new(String::from("( \t\r)"));
        let result = scanner.scan_tokens().unwrap();
        let expected = vec![
            Token { typ: TokenType::LeftParen, lexeme: String::from("("), line: 1 },
            Token { typ: TokenType::RightParen, lexeme: String::from(")"), line: 1 },
            Token { typ: TokenType::Eof, lexeme: String::from(""), line: 1 },
            ];
        assert_eq!(result, expected);
    }

    #[test]
    fn empty_string() {
        let scanner = Scanner::new(String::from("\"\""));
        let result = scanner.scan_tokens().unwrap();
        let expected = vec![
            Token { typ: TokenType::String(String::from("")), lexeme: String::from("\"\""), line: 1 },
            Token { typ: TokenType::Eof, lexeme: String::from(""), line: 1 },
            ];
        assert_eq!(result, expected);
    }

    #[test]
    fn string() {
        let scanner = Scanner::new(String::from("\"hello world\""));
        let result = scanner.scan_tokens().unwrap();
        let expected = vec![
            Token { typ: TokenType::String(String::from("hello world")), lexeme: String::from("\"hello world\""), line: 1 },
            Token { typ: TokenType::Eof, lexeme: String::from(""), line: 1 },
            ];
        assert_eq!(result, expected);
    }

    #[test]
    fn unterminated_string_should_fail() {
        let scanner = Scanner::new(String::from("\"hello world"));
        let result = scanner.scan_tokens();
        let expected = Err(vec![ScannerError { message: String::from("Unterminated string."), line: 1}]);
        assert_eq!(result, expected);
    }

    #[test]
    fn string_with_parens() {
        let scanner = Scanner::new(String::from("(\"hello world\")"));
        let result = scanner.scan_tokens().unwrap();
        let expected = vec![
            Token { typ: TokenType::LeftParen, lexeme: String::from("("), line: 1 },
            Token { typ: TokenType::String(String::from("hello world")), lexeme: String::from("\"hello world\""), line: 1 },
            Token { typ: TokenType::RightParen, lexeme: String::from(")"), line: 1 },
            Token { typ: TokenType::Eof, lexeme: String::from(""), line: 1 },
            ];
        assert_eq!(result, expected);
    }

    #[test]
    fn multi_line_string() {
        let scanner = Scanner::new(String::from("(\"hello \nworld\")"));
        let result = scanner.scan_tokens().unwrap();
        let expected = vec![
            Token { typ: TokenType::LeftParen, lexeme: String::from("("), line: 1 },
            Token { typ: TokenType::String(String::from("hello \nworld")), lexeme: String::from("\"hello \nworld\""), line: 1 },
            Token { typ: TokenType::RightParen, lexeme: String::from(")"), line: 2 },
            Token { typ: TokenType::Eof, lexeme: String::from(""), line: 2 },
            ];
        assert_eq!(result, expected);
    }

    #[test]
    fn integer() {
        let scanner = Scanner::new(String::from("12"));
        let result = scanner.scan_tokens().unwrap();
        let expected = vec![
            Token { typ: TokenType::Number(12.0), lexeme: String::from("12"), line: 1 },
            Token { typ: TokenType::Eof, lexeme: String::from(""), line: 1 },
            ];
        assert_eq!(result, expected);
    }

    #[test]
    fn float() {
        let scanner = Scanner::new(String::from("7.8"));
        let result = scanner.scan_tokens().unwrap();
        let expected = vec![
            Token { typ: TokenType::Number(7.8), lexeme: String::from("7.8"), line: 1 },
            Token { typ: TokenType::Eof, lexeme: String::from(""), line: 1 },
            ];
        assert_eq!(result, expected);
    }

    #[test]
    fn float_then_dot() {
        let scanner = Scanner::new(String::from("7.8.()"));
        let result = scanner.scan_tokens().unwrap();
        let expected = vec![
            Token { typ: TokenType::Number(7.8), lexeme: String::from("7.8"), line: 1 },
            Token { typ: TokenType::Dot, lexeme: String::from("."), line: 1 },
            Token { typ: TokenType::LeftParen, lexeme: String::from("("), line: 1 },
            Token { typ: TokenType::RightParen, lexeme: String::from(")"), line: 1 },
            Token { typ: TokenType::Eof, lexeme: String::from(""), line: 1 },
            ];
        assert_eq!(result, expected);
    }

    #[test]
    fn int_then_dot() {
        let scanner = Scanner::new(String::from("14."));
        let result = scanner.scan_tokens().unwrap();
        let expected = vec![
            Token { typ: TokenType::Number(14.0), lexeme: String::from("14"), line: 1 },
            Token { typ: TokenType::Dot, lexeme: String::from("."), line: 1 },
            Token { typ: TokenType::Eof, lexeme: String::from(""), line: 1 },
            ];
        assert_eq!(result, expected);
    }

    #[test]
    fn identifier() {
        let scanner = Scanner::new(String::from("orchid"));
        let result = scanner.scan_tokens().unwrap();
        let expected = vec![
            Token { typ: TokenType::Identifier(String::from("orchid")), lexeme: String::from("orchid"), line: 1 },
            Token { typ: TokenType::Eof, lexeme: String::from(""), line: 1 },
            ];
        assert_eq!(result, expected);
    }

    #[test]
    fn alphanum_identifier() {
        let scanner = Scanner::new(String::from("orchid7"));
        let result = scanner.scan_tokens().unwrap();
        let expected = vec![
            Token { typ: TokenType::Identifier(String::from("orchid7")), lexeme: String::from("orchid7"), line: 1 },
            Token { typ: TokenType::Eof, lexeme: String::from(""), line: 1 },
            ];
        assert_eq!(result, expected);
    }

    #[test]
    fn keywords() {
        let scanner = Scanner::new(String::from("or nil"));
        let result = scanner.scan_tokens().unwrap();
        let expected = vec![
            Token { typ: TokenType::Or, lexeme: String::from("or"), line: 1 },
            Token { typ: TokenType::Nil, lexeme: String::from("nil"), line: 1 },
            Token { typ: TokenType::Eof, lexeme: String::from(""), line: 1 },
            ];
        assert_eq!(result, expected);
    }
}
