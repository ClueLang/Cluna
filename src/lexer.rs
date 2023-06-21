#[derive(Debug,Clone, Copy,PartialEq)]
#[rustfmt::skip]
pub enum TokenType {
    // tokens
    Plus, Minus, Star, Slash, Percent, Caret, Hash, Tilde,
    Equals, DoubleEquals, NotEquals, LessThan, LessThanOrEqual,
    GreaterThan, GreaterThanOrEqual, Dot, Colon, Semicolon, Comma,
    LeftParen, RightParen, LeftBrace, RightBrace, LeftBracket, RightBracket,
    DoubleDot, TripleDot,

    // literals
    Number, String, MultilineString,Identifier,

    // keywords
    And, Break, Do, If, Else, ElseIf, End, True, False, Function,
    In, Local, Nil, Not, Or, Repeat, Return, Then, Until, While, For,

    // Unsupported: Goto, Labels

    Eof
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    kind: TokenType,
    lexeme: String,
    line: usize,
}

impl Token {
    pub fn new(kind: TokenType, lexeme: String, line: usize) -> Self {
        Self { kind, lexeme, line }
    }
    pub fn kind(&self) -> TokenType {
        self.kind
    }

    pub fn lexeme(&self) -> String {
        self.lexeme.clone()
    }

    pub fn line(&self) -> usize {
        self.line
    }
}

pub struct Lexer {
    source: Vec<char>,
    pub tokens: Vec<Token>,
    column: usize,
    current: usize,
    line: usize,
}

impl Lexer {
    pub fn new(source: String) -> Self {
        Self {
            source: source.chars().collect(),
            tokens: Vec::new(),
            column: 0,
            current: 0,
            line: 1,
        }
    }

    fn done(&self) -> bool {
        self.current >= self.source.len()
    }

    fn advance(&mut self) -> Option<char> {
        if self.done() {
            None
        } else {
            if self.source[self.current] == '\n' {
                self.line += 1;
                self.column = 0;
            }
            let result = Some(self.source[self.current]);
            self.current += 1;
            self.column += 1;
            result
        }
    }

    fn advance_to(&mut self, offset: usize) -> Option<char> {
        if self.done() {
            None
        } else {
            if self.source[self.current + offset - 1] == '\n' {
                self.line += 1;
                self.column = 0;
            }
            let result = Some(self.source[self.current + offset - 1]);
            self.current += offset;
            self.column += offset;
            result
        }
    }

    fn go_back(&mut self) -> Option<char> {
        if self.current == 0 {
            None
        } else {
            if self.source[self.current - 1] == '\n' {
                self.line -= 1;
                self.column = 0;
            } else {
                self.column -= 1;
            }
            self.current -= 1;

            Some(self.source[self.current])
        }
    }

    fn peek(&self) -> Option<char> {
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

    fn look_back(&self) -> Option<char> {
        (self.current != 0).then_some(self.source[self.current - 2])
    }

    fn add_token(&mut self, token_type: TokenType, len: usize) {
        let lexeme = self.source[self.current - len..self.current]
            .iter()
            .collect();
        self.tokens.push(Token::new(token_type, lexeme, self.line));
    }

    fn add_token_front(&mut self, token_type: TokenType, len: usize) {
        let lexeme = self.source[self.current - 1..self.current + len - 1]
            .iter()
            .collect();
        self.advance_to(len - 1);
        self.tokens.push(Token::new(token_type, lexeme, self.line));
    }

    fn read_string(&mut self, quote: char) -> Result<(), String> {
        let start = self.current;
        while self.peek() != Some(quote) && !self.done() {
            if self.peek() == Some('\\') {
                self.advance();
                match self.peek() {
                    Some(_) => {
                        self.advance();
                    }
                    None => {
                        return Err(format!(
                            "Error: Unterminated escape sequence at {}:{}",
                            self.line, self.column
                        ))
                    }
                }
            } else {
                self.advance();
            }
        }

        if self.done() {
            return Err(format!(
                "Error: Unterminated string at {}:{}",
                self.line, self.column
            ));
        }
        self.advance();
        let lexeme = self.source[start - 1..self.current].iter().collect();
        self.tokens
            .push(Token::new(TokenType::String, lexeme, self.line));
        Ok(())
    }

    fn read_multiline_string(&mut self) -> Result<(), String> {
        let start = self.current;
        let mut equals_count = 0;

        while let Some(c) = self.advance() {
            if self.done() {
                return Err(format!(
                    "Error: Unterminated multiline string at {}:{}",
                    self.line, self.column
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
                return Err(format!(
                    "Error: Unterminated multiline string at {}:{}",
                    self.line, self.column
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

        let lexeme = self.source[start - 1..self.current].iter().collect();
        self.tokens
            .push(Token::new(TokenType::MultilineString, lexeme, self.line));
        Ok(())
    }

    fn read_token(&mut self) -> Result<(), String> {
        let start = self.current;

        if let Some(c) = self.peek() {
            if !(c.is_alphabetic() || c == '_') {
                return Err(format!(
                    "Error: Invalid identifier at {}:{}",
                    self.line, self.column
                ));
            }
            while let Some(c) = self.advance() {
                if !(c.is_alphanumeric() || c == '_') {
                    break;
                }
            }
            self.go_back();
            let lexeme: String = self.source[start..self.current].iter().collect();

            match &*lexeme {
                "and" => self
                    .tokens
                    .push(Token::new(TokenType::And, lexeme, self.line)),
                "break" => self
                    .tokens
                    .push(Token::new(TokenType::Break, lexeme, self.line)),
                "do" => self
                    .tokens
                    .push(Token::new(TokenType::Do, lexeme, self.line)),
                "else" => self
                    .tokens
                    .push(Token::new(TokenType::Else, lexeme, self.line)),
                "elseif" => self
                    .tokens
                    .push(Token::new(TokenType::ElseIf, lexeme, self.line)),
                "end" => self
                    .tokens
                    .push(Token::new(TokenType::End, lexeme, self.line)),
                "false" => self
                    .tokens
                    .push(Token::new(TokenType::False, lexeme, self.line)),
                "for" => self
                    .tokens
                    .push(Token::new(TokenType::For, lexeme, self.line)),
                "function" => self
                    .tokens
                    .push(Token::new(TokenType::Function, lexeme, self.line)),
                "if" => self
                    .tokens
                    .push(Token::new(TokenType::If, lexeme, self.line)),
                "in" => self
                    .tokens
                    .push(Token::new(TokenType::In, lexeme, self.line)),
                "local" => self
                    .tokens
                    .push(Token::new(TokenType::Local, lexeme, self.line)),

                "nil" => self
                    .tokens
                    .push(Token::new(TokenType::Nil, lexeme, self.line)),
                "not" => self
                    .tokens
                    .push(Token::new(TokenType::Not, lexeme, self.line)),
                "or" => self
                    .tokens
                    .push(Token::new(TokenType::Or, lexeme, self.line)),
                "repeat" => self
                    .tokens
                    .push(Token::new(TokenType::Repeat, lexeme, self.line)),
                "return" => self
                    .tokens
                    .push(Token::new(TokenType::Return, lexeme, self.line)),
                "then" => self
                    .tokens
                    .push(Token::new(TokenType::Then, lexeme, self.line)),
                "true" => self
                    .tokens
                    .push(Token::new(TokenType::True, lexeme, self.line)),
                "until" => self
                    .tokens
                    .push(Token::new(TokenType::Until, lexeme, self.line)),
                "while" => self
                    .tokens
                    .push(Token::new(TokenType::While, lexeme, self.line)),
                "goto" => {
                    return Err(format!(
                        "Goto is unsupported in clue at {}:{}",
                        self.line, self.column
                    ))
                }
                _ => self
                    .tokens
                    .push(Token::new(TokenType::Identifier, lexeme, self.line)),
            }
            Ok(())
        } else {
            Err(format!(
                "Error: Invalid identifier at {}:{}",
                self.line, self.column
            ))
        }
    }

    fn read_number(&mut self) -> Result<(), String> {
        let start = self.current;
        let mut digit_encountered = false;
        let mut e_encountered = false;
        let mut hex_encountered = false;
        let mut sign_encountered = false;

        while let Some(c) = self.advance() {
            match c {
                '0'..='9' => {
                    digit_encountered = true;
                }
                '.' => {
                    if !digit_encountered && !self.peek().map_or(false, |c| c.is_ascii_digit()) {
                        return Err(format!(
                            "Error: Malformed number at {}:{}",
                            self.line, self.column
                        ));
                    }
                }
                'x' | 'X' => {
                    if !self.look_back().map_or(false, |c| c == '0') {
                        return Err(format!(
                            "Error: Malformed number at {}:{}",
                            self.line, self.column
                        ));
                    }
                    hex_encountered = true;
                }
                'e' | 'E' => {
                    if hex_encountered {
                        continue;
                    }

                    if !digit_encountered
                        || !self
                            .peek()
                            .map_or(false, |c| c.is_ascii_digit() || c == '+' || c == '-')
                        || e_encountered
                    {
                        return Err(format!(
                            "Error: Malformed number at {}:{}",
                            self.line, self.column
                        ));
                    }
                    if self.peek().map_or(false, |c| c == '+' || c == '-') {
                        self.advance();
                    }
                    e_encountered = true;
                }
                'a'..='f' | 'A'..='F' => {
                    if !hex_encountered {
                        return Err(format!(
                            "Error: Malformed number at {}:{}",
                            self.line, self.column
                        ));
                    }
                }
                '+' | '-' => {
                    if !e_encountered {
                        self.go_back();
                        break;
                    }

                    if sign_encountered {
                        return Err(format!(
                            "Error: Malformed number at {}:{}",
                            self.line, self.column
                        ));
                    }
                    sign_encountered = true;
                }
                _ => {
                    self.go_back();
                    break;
                }
            }
        }

        if e_encountered
            && !(self.source[self.current - 1].is_ascii_digit()
                || self.peek().map_or(false, |c| c.is_ascii_digit()))
        {
            return Err(format!(
                "Error: Malformed number at {}:{}",
                self.line, self.column
            ));
        }

        if !digit_encountered {
            return Err(format!(
                "Error: Malformed number at {}:{}",
                self.line, self.column
            ));
        }

        self.add_token(TokenType::Number, self.current - start);

        Ok(())
    }

    fn read_multiline_comment(&mut self) -> Result<(), String> {
        let mut equals_count = 0;

        while let Some(c) = self.advance() {
            if self.done() {
                return Err(format!(
                    "Error: Unterminated block comment at {}:{}",
                    self.line, self.column
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
                return Err(format!(
                    "Error: Unterminated block comment at {}:{}",
                    self.line, self.column
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

pub fn scan_code(code: String) -> Result<Vec<Token>, String> {
    let mut lexer = Lexer::new(code);
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
            '#' => lexer.add_token(TokenType::Hash, 1),
            '*' => lexer.add_token(TokenType::Star, 1),
            '/' => lexer.add_token(TokenType::Slash, 1),
            '%' => lexer.add_token(TokenType::Percent, 1),
            '^' => lexer.add_token(TokenType::Caret, 1),
            '=' => {
                if lexer.peek().map_or(false, |c| c == '=') {
                    lexer.advance();
                    lexer.add_token(TokenType::DoubleEquals, 2);
                } else {
                    lexer.add_token(TokenType::Equals, 1);
                }
            }
            '~' => {
                if lexer.peek().map_or(false, |c| c == '=') {
                    lexer.advance();
                    lexer.add_token(TokenType::NotEquals, 2);
                } else {
                    lexer.add_token(TokenType::Not, 1);
                }
            }
            '<' => {
                if lexer.peek().map_or(false, |c| c == '=') {
                    lexer.advance();
                    lexer.add_token(TokenType::LessThanOrEqual, 2);
                } else {
                    lexer.add_token(TokenType::LessThan, 1);
                }
            }
            '>' => {
                if lexer.peek().map_or(false, |c| c == '=') {
                    lexer.advance();
                    lexer.add_token(TokenType::GreaterThanOrEqual, 2);
                } else {
                    lexer.add_token(TokenType::GreaterThan, 1);
                }
            }
            '.' => match (lexer.peek(), lexer.peek_at(2)) {
                (Some('.'), Some('.')) => lexer.add_token_front(TokenType::TripleDot, 3),
                (Some('1'..='9'), _) => {
                    lexer.current -= 1;
                    lexer.read_number()?
                }
                (Some('.'), _) => lexer.add_token_front(TokenType::DoubleDot, 2),
                _ => lexer.add_token(TokenType::Dot, 1),
            },
            ':' => {
                if lexer.peek().map_or(false, |c| c == ':') {
                    return Err(format!(
                        "Labels are not supported at {}:{}",
                        lexer.line, lexer.column
                    ));
                }
                lexer.add_token(TokenType::Colon, 1)
            }
            ';' => lexer.add_token(TokenType::Semicolon, 1),
            ',' => lexer.add_token(TokenType::Comma, 1),
            '"' | '\'' => {
                lexer.read_string(c)?;
            }
            _ => {
                lexer.current -= 1;
                if c.is_ascii_digit() {
                    lexer.read_number()?;
                } else if c.is_ascii_alphabetic() || c == '_' {
                    lexer.read_token()?;
                } else {
                    panic!(
                        "Error: Unexpected character {c} at {}:{}",
                        lexer.line, lexer.column
                    );
                }
            }
        }
    }
    Ok(lexer.tokens)
}
