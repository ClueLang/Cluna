use crate::{error::Diagnostic, number::Number};
use collect_into_rc_slice::CollectIntoRcStr;
use std::{fmt, ops::Range, rc::Rc};

#[derive(Debug,Clone, Copy, PartialEq)]
#[rustfmt::skip]
pub enum TokenType {
    // tokens
    Plus, Minus, Star, Slash, FloorDiv, Percent, Caret, Hash, Tilde,
    Equals, DoubleEquals, NotEquals, LessThan, LessThanOrEqual,
    GreaterThan, GreaterThanOrEqual, Dot, Colon, Semicolon, Comma,
    LeftParen, RightParen, LeftBrace, RightBrace, LeftBracket, RightBracket,
    DoubleDot, TripleDot,

    // bitwise ops
    BitAnd, BitOr, BitShiftLeft, BitShiftRight,

    // literals
    Number, String, MultilineString, Identifier,

    // keywords
    And, Break, Do, If, Else, ElseIf, End, True, False, Function,
    In, Local, Nil, Not, Or, Repeat, Return, Then, Until, While, For,

    // Unsupported: Goto, Labels

    Eof
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lexeme {
    Symbol(Rc<str>),
    Number(Number),
}

impl fmt::Display for Lexeme {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Lexeme::Symbol(s) => write!(f, "{}", s),
            Lexeme::Number(n) => write!(f, "{}", n),
        }
    }
}

impl Lexeme {
    pub fn as_symbol(&self) -> Rc<str> {
        match self {
            Lexeme::Symbol(s) => s.clone(),
            _ => panic!("lexeme is not a symbol"),
        }
    }

    pub fn as_number(&self) -> &Number {
        match self {
            Lexeme::Number(n) => n,
            _ => panic!("lexeme is not a number"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Position {
    pub line: usize,
    pub column: usize,
    pub span: Range<usize>,
}

impl Position {
    pub const fn new(line: usize, column: usize, span: Range<usize>) -> Self {
        Self { line, column, span }
    }

    pub const fn with_span(&self, span: Range<usize>) -> Self {
        Self {
            line: self.line,
            column: self.column,
            span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub(crate) kind: TokenType,
    lexeme: Lexeme,
    position: Position,
}

impl Token {
    pub const fn new(kind: TokenType, lexeme: Rc<str>, position: Position) -> Self {
        Self {
            kind,
            lexeme: Lexeme::Symbol(lexeme),
            position,
        }
    }

    pub const fn new_number(kind: TokenType, lexeme: Number, position: Position) -> Self {
        Self {
            kind,
            lexeme: Lexeme::Number(lexeme),
            position,
        }
    }

    pub const fn kind(&self) -> TokenType {
        self.kind
    }

    pub const fn lexeme(&self) -> &Lexeme {
        &self.lexeme
    }

    pub fn position(&self) -> Position {
        self.position.clone()
    }

    pub const fn line(&self) -> usize {
        self.position.line
    }

    pub const fn column(&self) -> usize {
        self.position.column
    }

    pub fn span(&self) -> Range<usize> {
        self.position.span.clone()
    }
}

pub struct Lexer {
    pub(crate) source: Vec<char>,
    tokens: Vec<Token>,
    pub(crate) path: Option<String>,
    pub(crate) current: usize,
    pub(crate) position: Position,
}

impl Lexer {
    pub fn new(source: String, path: Option<String>) -> Self {
        Self {
            source: source.chars().collect(),
            tokens: Vec::new(),
            path,
            current: 0,
            position: Position::new(1, 0, 0..0),
        }
    }

    pub(crate) const fn line(&self) -> usize {
        self.position.line
    }

    pub(crate) const fn column(&self) -> usize {
        self.position.column
    }

    fn done(&self) -> bool {
        self.current >= self.source.len()
    }

    pub(crate) fn advance(&mut self) -> Option<char> {
        if self.done() {
            None
        } else {
            if self.source[self.current] == '\n' {
                self.position.line += 1;
                self.position.column = 0;
            }
            let result = Some(self.source[self.current]);
            self.current += 1;
            self.position.column += 1;

            result
        }
    }

    fn advance_to(&mut self, offset: usize) -> Option<char> {
        if self.done() {
            None
        } else {
            if self.source[self.current + offset - 1] == '\n' {
                self.position.line += 1;
                self.position.column = 0;
            }
            let result = Some(self.source[self.current + offset - 1]);
            self.current += offset;
            self.position.column += offset;
            result
        }
    }

    pub(crate) fn go_back(&mut self) -> Option<char> {
        if self.current == 0 {
            None
        } else {
            if self.source[self.current - 1] == '\n' {
                self.position.line -= 1;
                self.position.column = 0;
            } else {
                self.position.column -= 1;
            }
            self.current -= 1;

            Some(self.source[self.current])
        }
    }

    pub(crate) fn peek(&self) -> Option<char> {
        if self.done() {
            None
        } else {
            self.source.get(self.current).copied()
        }
    }

    fn peek_at(&self, offset: usize) -> Option<char> {
        if self.done() {
            None
        } else {
            self.source.get(self.current + offset - 1).copied()
        }
    }

    pub(crate) fn look_back(&self) -> Option<char> {
        (self.current != 0).then_some(self.source[self.current - 2])
    }

    fn add_token(&mut self, token_type: TokenType, len: usize) {
        let span = self.current - len..self.current;
        let lexeme = self.source[span.clone()]
            .iter()
            .copied()
            .collect_into_rc_str();
        self.tokens.push(Token::new(
            token_type,
            lexeme,
            Position::new(self.position.line, self.position.column, span),
        ));
    }

    fn add_token_front(&mut self, token_type: TokenType, len: usize) {
        let span = self.current - 1..self.current + len - 1;
        let lexeme = self.source[span.clone()]
            .iter()
            .copied()
            .collect_into_rc_str();
        self.advance_to(len - 1);
        self.tokens.push(Token::new(
            token_type,
            lexeme,
            Position::new(self.position.line, self.position.column, span),
        ));
    }

    fn read_string(&mut self, quote: char) -> Result<(), Diagnostic> {
        let start = self.current;
        while self.peek() != Some(quote) && !self.done() {
            if self.peek() == Some('\\') {
                self.advance();
                match self.peek() {
                    Some(_) => {
                        self.advance();
                    }
                    None => {
                        return Err(Diagnostic::new(
                            "unterminated escape sequence".to_owned(),
                            self.path.clone(),
                            self.position.clone(),
                        ));
                    }
                }
            } else {
                self.advance();
            }
        }

        if self.done() {
            return Err(Diagnostic::new(
                "unterminated string".to_owned(),
                self.path.clone(),
                self.position.clone(),
            ));
        }
        self.advance();
        let span = start - 1..self.current;
        let lexeme = self.source[span.clone()]
            .iter()
            .copied()
            .collect_into_rc_str();
        self.tokens.push(Token::new(
            TokenType::String,
            lexeme,
            self.position.with_span(span),
        ));
        Ok(())
    }

    fn read_multiline_string(&mut self) -> Result<(), Diagnostic> {
        let start = self.current;
        let mut equals_count = 0;

        while let Some(c) = self.advance() {
            if self.done() {
                return Err(Diagnostic::new(
                    "unterminated multiline string".to_owned(),
                    self.path.clone(),
                    self.position.with_span(start..self.current),
                ));
            }

            if c == '[' {
                break;
            }

            if c == '=' {
                equals_count += 1;
            }
        }

        while let Some(c) = self.advance() {
            if self.done() {
                return Err(Diagnostic::new(
                    "unterminated multiline string".to_owned(),
                    self.path.clone(),
                    self.position.with_span(start..self.current),
                ));
            }

            if c == ']' {
                let mut equals_encountered = 0;
                while let Some(c) = self.peek() {
                    if c == '=' {
                        equals_encountered += 1;
                        self.advance();
                    } else {
                        break;
                    }
                }

                if equals_encountered == equals_count && self.peek() == Some(']') {
                    break;
                }
            }
        }

        self.advance();
        let span = start - 1..self.current;
        let lexeme = self.source[span.clone()]
            .iter()
            .copied()
            .collect_into_rc_str();

        self.tokens.push(Token::new(
            TokenType::MultilineString,
            lexeme,
            self.position.with_span(span),
        ));
        Ok(())
    }

    fn read_token(&mut self) -> Result<(), Diagnostic> {
        let start = self.current;

        if let Some(c) = self.peek() {
            if !(c.is_alphabetic() || c == '_') {
                return Err(Diagnostic::new(
                    "invalid identifier".to_owned(),
                    self.path.clone(),
                    self.position.with_span(start..self.current),
                ));
            }
            while let Some(c) = self.advance() {
                if !(c.is_alphanumeric() || c == '_') {
                    self.go_back();
                    break;
                }
            }
            let span = start..self.current;
            let lexeme = self.source[span.clone()]
                .iter()
                .copied()
                .collect_into_rc_str();

            let kind = match &*lexeme {
                "and" => TokenType::And,
                "break" => TokenType::Break,
                "do" => TokenType::Do,
                "else" => TokenType::Else,
                "elseif" => TokenType::ElseIf,
                "end" => TokenType::End,
                "false" => TokenType::False,
                "for" => TokenType::For,
                "function" => TokenType::Function,
                "if" => TokenType::If,
                "in" => TokenType::In,
                "local" => TokenType::Local,
                "nil" => TokenType::Nil,
                "not" => TokenType::Not,
                "or" => TokenType::Or,
                "repeat" => TokenType::Repeat,
                "return" => TokenType::Return,
                "then" => TokenType::Then,
                "true" => TokenType::True,
                "until" => TokenType::Until,
                "while" => TokenType::While,
                "goto" => {
                    return Err(Diagnostic::new(
                        "goto is not supported in clue".to_owned(),
                        self.path.clone(),
                        self.position.with_span(span),
                    ))
                }
                _ => TokenType::Identifier,
            };

            self.tokens
                .push(Token::new(kind, lexeme, self.position.with_span(span)));

            Ok(())
        } else {
            Err(Diagnostic::new(
                "invalid identifier".to_owned(),
                self.path.clone(),
                self.position.with_span(start..self.current),
            ))
        }
    }

    fn read_number(&mut self) -> Result<(), Diagnostic> {
        let start = self.current;
        let number = Number::from_source(self)?;
        let span = start - 1..self.current;

        self.tokens.push(Token::new_number(
            TokenType::Number,
            number,
            self.position.with_span(span),
        ));
        Ok(())
    }

    fn read_multiline_comment(&mut self) -> Result<(), Diagnostic> {
        let mut equals_count = 0;
        let start = self.current;

        while let Some(c) = self.advance() {
            if self.done() {
                return Err(Diagnostic::new(
                    "unterminated block comment".to_owned(),
                    self.path.clone(),
                    self.position.with_span(start..self.current),
                ));
            }

            if c == '[' {
                break;
            }

            if c == '=' {
                equals_count += 1;
            }
        }

        while let Some(c) = self.advance() {
            if self.done() {
                return Err(Diagnostic::new(
                    "unterminated block comment".to_owned(),
                    self.path.clone(),
                    self.position.with_span(start..self.current),
                ));
            }

            if c == '-' && self.peek() == Some('-') && self.peek_at(2) == Some(']') {
                self.advance_to(2);
            }

            let mut equals_encountered = 0;
            while let Some(c) = self.peek() {
                if c == '=' {
                    equals_encountered += 1;
                    self.advance();
                } else {
                    break;
                }
            }

            if equals_encountered == equals_count && self.peek() == Some(']') {
                self.advance();
                break;
            }
        }

        self.advance();
        Ok(())
    }
}

pub fn scan_code(code: String, path: Option<String>) -> Result<Vec<Token>, Diagnostic> {
    let mut lexer = Lexer::new(code, path);

    while let Some(c) = lexer.advance() {
        match c {
            ' ' | '\r' | '\t' | '\n' => {}
            '(' => lexer.add_token(TokenType::LeftParen, 1),
            ')' => lexer.add_token(TokenType::RightParen, 1),
            '{' => lexer.add_token(TokenType::LeftBrace, 1),
            '}' => lexer.add_token(TokenType::RightBrace, 1),
            '[' => {
                if lexer.peek().map_or(false, |c| c == '[' || c == '=') {
                    lexer.read_multiline_string()?;
                    continue;
                }
                lexer.add_token(TokenType::LeftBracket, 1)
            }
            ']' => lexer.add_token(TokenType::RightBracket, 1),
            '+' => lexer.add_token(TokenType::Plus, 1),
            '-' => match (lexer.peek(), lexer.peek_at(2)) {
                (Some('-'), Some('[')) => {
                    if matches!(lexer.peek_at(3), Some('[' | '=')) {
                        lexer.advance_to(2);
                        lexer.read_multiline_comment()?;
                        continue;
                    } else {
                        lexer.advance_to(2);
                        while let Some(c) = lexer.advance() {
                            if c == '\n' {
                                break;
                            }
                        }
                    }
                }
                (Some('-'), _) => {
                    lexer.advance();
                    while let Some(c) = lexer.advance() {
                        if c == '\n' {
                            break;
                        }
                    }
                }
                _ => lexer.add_token(TokenType::Minus, 1),
            },
            '#' => {
                if lexer.line() == 1 && lexer.column() == 1 {
                    while let Some(c) = lexer.advance() {
                        if c == '\n' {
                            break;
                        }
                    }
                    continue;
                }
                lexer.add_token(TokenType::Hash, 1)
            }
            '*' => lexer.add_token(TokenType::Star, 1),
            '/' => {
                if let Some('/') = lexer.peek() {
                    lexer.advance();
                    lexer.add_token(TokenType::FloorDiv, 2)
                } else {
                    lexer.add_token(TokenType::Slash, 1)
                }
            }
            '%' => lexer.add_token(TokenType::Percent, 1),
            '^' => lexer.add_token(TokenType::Caret, 1),
            '=' => {
                if let Some('=') = lexer.peek() {
                    lexer.advance();
                    lexer.add_token(TokenType::DoubleEquals, 2);
                } else {
                    lexer.add_token(TokenType::Equals, 1);
                }
            }
            '~' => {
                if let Some('=') = lexer.peek() {
                    lexer.advance();
                    lexer.add_token(TokenType::NotEquals, 2);
                } else {
                    lexer.add_token(TokenType::Tilde, 1);
                }
            }
            '<' => match lexer.peek() {
                Some('=') => {
                    lexer.advance();
                    lexer.add_token(TokenType::LessThanOrEqual, 2);
                }
                Some('<') => {
                    lexer.advance();
                    lexer.add_token(TokenType::BitShiftLeft, 2);
                }
                _ => lexer.add_token(TokenType::LessThan, 1),
            },
            '>' => match lexer.peek() {
                Some('=') => {
                    lexer.advance();
                    lexer.add_token(TokenType::GreaterThanOrEqual, 2);
                }
                Some('>') => {
                    lexer.advance();
                    lexer.add_token(TokenType::BitShiftRight, 2);
                }
                _ => lexer.add_token(TokenType::GreaterThan, 1),
            },
            '.' => match (lexer.peek(), lexer.peek_at(2)) {
                (Some('.'), Some('.')) => lexer.add_token_front(TokenType::TripleDot, 3),
                (Some('0'..='9'), _) => {
                    lexer.current -= 1;
                    lexer.read_number()?
                }
                (Some('.'), _) => lexer.add_token_front(TokenType::DoubleDot, 2),
                _ => lexer.add_token(TokenType::Dot, 1),
            },
            ':' => {
                if let Some(':') = lexer.peek() {
                    return Err(Diagnostic::new(
                        "labels are not supported".to_owned(),
                        lexer.path.clone(),
                        lexer.position.with_span(lexer.current - 1..lexer.current),
                    ));
                }
                lexer.add_token(TokenType::Colon, 1)
            }
            ';' => lexer.add_token(TokenType::Semicolon, 1),
            ',' => lexer.add_token(TokenType::Comma, 1),
            '&' => lexer.add_token(TokenType::BitAnd, 1),
            '|' => lexer.add_token(TokenType::BitOr, 1),
            '"' | '\'' => {
                lexer.read_string(c)?;
            }
            _ => {
                lexer.go_back();
                if c.is_ascii_digit() {
                    lexer.read_number()?;
                } else if c.is_ascii_alphabetic() || c == '_' {
                    lexer.read_token()?;
                } else {
                    panic!(
                        "{}",
                        Diagnostic::unexpected(c.to_string(), lexer.path, lexer.position)
                    );
                }
            }
        }
    }
    Ok(lexer.tokens)
}
