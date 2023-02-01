// list of keywords in lua
const KEYWORDS: &[&str] = &[
    "and", "break", "do", "else", "elseif", "end", "false", "for", "function", "if", "in", "local",
    "nil", "not", "or", "repeat", "return", "then", "true", "until", "while",
];

#[derive(Debug)]
#[rustfmt::skip]
pub enum TokenType {
    // tokens
    Plus, Minus, Star, Slash, Modulo, Caret, Hash, Tilde,
    Equals, DoubleEquals, NotEquals, LessThan, LessThanEquals, 
    GreaterThan, GreaterThanEquals, Dot, Colon, Semicolon, Comma,
    LeftParen, RightParen, LeftBrace, RightBrace, LeftBracket, RightBracket, 
    DoubleDot, TripleDot,

    // literals
    Number, String, Identifier,

    // keywords
    And, Break, Do, If, Else, ElseIf, End, True, False, Function, 
    In, Local, Nil, Not, Or, Repeat, Return, Then, Until, While,

    Eof
}

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: usize,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String, line: usize) -> Self {
        Self {
            token_type,
            lexeme,
            line,
        }
    }
}

pub struct Lexer {
    source: Vec<char>,
    pub tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
}

impl Lexer {
    pub fn new(source: String) -> Self {
        Self {
            source: source.chars().collect(),
            tokens: Vec::new(),
            start: 0,
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
            }
            let result = Some(self.source[self.current]);
            self.current += 1;
            result
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
        let lexeme = self.source[self.current..len].iter().collect();
        self.tokens.push(Token::new(token_type, lexeme, self.line));
    }

    fn read_string(&mut self, quote: char) {
        let start = self.current;
        while self.peek() != Some(quote) && !self.done() {
            if self.peek() == Some(quote) {
                self.current += 1;
            }
        }

        let lexeme = self.source[start..self.current].iter().collect();
        self.tokens
            .push(Token::new(TokenType::String, lexeme, self.line));
    }

    fn read_multiline_string(&mut self) {
        let start = self.current;

        while self.peek() != Some(']') && self.peek_at(2) == Some(']') && !self.done() {
            self.current += 1;
        }

        let lexeme = self.source[start..self.current].iter().collect();
        self.tokens
            .push(Token::new(TokenType::String, lexeme, self.line));
    }

    fn read_identifier(&mut self) -> Result<(), String> {
        let start = self.current;

        if let Some(c) = self.advance() {
            if !(c.is_alphabetic() || c == '_') {
                return Err("Error: Invalid identifier".to_owned());
            }
            while let Some(c) = self.advance() {
                if !c.is_alphanumeric() || c == '_' {
                    break;
                }
            }

            let lexeme: String = self.source[start..self.current].iter().collect();
            if KEYWORDS.contains(&&*lexeme) {
                Err("Error: Identifier is a keyword".to_owned())
            } else {
                self.tokens
                    .push(Token::new(TokenType::Identifier, lexeme, self.line));
                Ok(())
            }
        } else {
            Err("Error: Invalid identifier".to_owned())
        }
    }

    pub fn read_number(&mut self) -> Result<(), String> {
        let start = self.current;
        let mut digit_encountered = false;
        let mut e_encountered = false;
        let mut hex_encountered = false;

        while let Some(c) = self.advance() {
            match c {
                '0'..='9' => {
                    digit_encountered = true;
                }
                '.' => {
                    if !self.peek().map_or(false, |c| c.is_ascii_digit()) {
                        return Err("Error: Malformed number".to_owned());
                    }
                }
                'x' => {
                    if !self.look_back().map_or(false, |c| c == '0') {
                        return Err("Error: Malformed number".to_owned());
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
                        return Err("Error: Malformed number".to_owned());
                    }
                    if self.peek().map_or(false, |c| c == '+' || c == '-') {
                        self.advance();
                    }
                    e_encountered = true;
                }
                'a'..='f' | 'A'..='F' => {
                    if !hex_encountered {
                        return Err("Error: Malformed number".to_owned());
                    }
                }
                '+' => {
                    if !self.peek().map_or(false, |c| c.is_ascii_digit()) && c == '+' {
                        return Err("Error: Malformed number".to_owned());
                    }
                }
                '-' => {}
                _ => return Err("Error: Malformed number".to_owned()),
            }
        }

        if e_encountered
            && !(self.source[self.current - 1].is_ascii_digit()
                || self.peek().map_or(false, |c| c.is_ascii_digit()))
        {
            return Err("Error: Malformed number".to_owned());
        }

        Ok(())
    }
}

pub fn scan_code(code: String) -> Vec<Token> {
    let mut lexer = Lexer::new(code);
    todo!()
}
