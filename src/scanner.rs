#![allow(non_camel_case_types)]

use self::CommentType::*;
use self::TokenType::*;

#[rustfmt::skip]
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum TokenType {
	//symbols
	ROUND_BRACKET_OPEN, ROUND_BRACKET_CLOSED,
	SQUARE_BRACKET_OPEN, SQUARE_BRACKET_CLOSED,
	CURLY_BRACKET_OPEN, CURLY_BRACKET_CLOSED,
	COMMA, SEMICOLON, NOT, AND, OR, DOLLAR,
	PLUS, MINUS, STAR, SLASH, PERCENTUAL, CARET, HASHTAG,
	COLON, DOT, TWODOTS, THREEDOTS, 
    BIT_AND, BIT_OR, TILDE, LEFT_SHIFT, RIGHT_SHIFT,
	
	//definition and comparison
	DEFINE, EQUAL, NOT_EQUAL, BIGGER, BIGGER_EQUAL, SMALLER, SMALLER_EQUAL,
	
	//literals
	IDENTIFIER, NUMBER, STRING, MULTILINE_STRING,
	
	//keywords
	IF, ELSEIF, ELSE, FOR, IN, WHILE, UNTIL, LOCAL, FN, METHOD, 
    RETURN, TRUE, FALSE, NIL, LOOP, BREAK, TRY, THEN, DO, END, REPEAT,

    NONE_BLOCK,

	EOF
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum CommentType {
    COMMENT,
    MULTILINE_COMMENT,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenType,
    pub lexeme: String,
    pub line: usize,
}
#[derive(Clone, Debug)]
pub struct Comment {
    pub kind: CommentType,
    pub lexeme: String,
    pub line: usize,
}

impl Comment {
    pub fn new(kind: CommentType, lexeme: String, line: usize) -> Self {
        Self {
            kind,
            lexeme: String::from(lexeme),
            line,
        }
    }
}

impl Token {
    pub fn new(kind: TokenType, lexeme: String, line: usize) -> Self {
        Self {
            kind,
            lexeme: String::from(lexeme),
            line,
        }
    }
}

struct CodeInfo {
    line: usize,
    start: usize,
    current: usize,
    size: usize,
    code: Vec<char>,
    filename: String,
    tokens: Vec<Token>,
    errored: bool,
}

impl CodeInfo {
    fn new(code: String, filename: String) -> CodeInfo {
        let chars = code.chars();
        Self {
            line: 1,
            start: 0,
            current: 0,
            size: chars.clone().count(),
            code: chars.collect(),
            filename,
            tokens: Vec::new(),
            errored: false,
        }
    }

    fn ended(&self) -> bool {
        self.current >= self.size
    }

    fn at(&self, pos: usize) -> char {
        if pos >= self.size {
            return 0 as char;
        }
        self.code[pos]
    }

    fn read_next(&mut self) -> char {
        let prev: char = self.at(self.current);
        self.current += 1;
        prev
    }

    fn compare(&mut self, expected: char) -> bool {
        if self.ended() {
            return false;
        }
        if self.at(self.current) != expected {
            return false;
        }
        self.current = self.current + 1;
        true
    }

    fn peek(&self, pos: usize) -> char {
        let pos: usize = self.current + pos;
        if pos >= self.size {
            return 0 as char;
        }
        self.at(pos)
    }

    //isNumber: c.is_ascii_digit()
    //isChar: c.is_ascii_alphabetic()
    //isCharOrNumber: c.is_ascii_alphanumeric()

    fn substr(&self, start: usize, end: usize) -> String {
        let mut result: String = String::new();
        for i in start..end {
            if i >= self.size {
                break;
            }
            result.push(self.at(i));
        }
        result
    }

    fn add_literal_token(&mut self, kind: TokenType, literal: String) {
        self.tokens.push(Token::new(kind, literal, self.line));
    }

    fn add_token(&mut self, kind: TokenType) {
        let lexeme: String = self.substr(self.start, self.current);
        self.tokens.push(Token::new(kind, lexeme, self.line));
    }

    fn compare_and_add(&mut self, c: char, kt: TokenType, kf: TokenType) {
        let kind: TokenType = match self.compare(c) {
            true => kt,
            false => kf,
        };
        self.add_token(kind);
    }

    fn match_and_add(&mut self, c1: char, k1: TokenType, c2: char, k2: TokenType, kd: TokenType) {
        let kind: TokenType = match self.compare(c1) {
            true => k1,
            false => match self.compare(c2) {
                true => k2,
                false => kd,
            },
        };
        self.add_token(kind);
    }

    fn warning(&mut self, message: &str) {
        println!(
            "Error in file \"{}\" at line {}!\nError: \"{}\"\n",
            self.filename, self.line, message
        );
        self.errored = true;
    }

    fn read_number(&mut self, check: impl Fn(&char) -> bool, simple: bool) {
        let start = self.current;
        while check(&self.peek(0)) {
            self.current += 1
        }
        if self.peek(0) == '.' && check(&self.peek(1)) {
            self.current += 1;
            while check(&self.peek(0)) {
                self.current += 1
            }
        }
        if simple {
            let c = self.peek(0);
            if c == 'e' || c == 'E' {
                let c = self.peek(1);
                if !c.is_ascii_digit() {
                    if c == '-' && self.peek(2).is_ascii_digit() {
                        self.current += 1;
                    } else {
                        self.warning("Malformed number");
                    }
                }
                self.current += 1;
                while self.peek(0).is_ascii_digit() {
                    self.current += 1
                }
            }
        } else if self.current == start {
            self.warning("Malformed number");
        }
        let llcheck = self.substr(self.current, self.current + 2);
        if llcheck == "LL" {
            self.current += 2;
        } else if llcheck == "UL" {
            if self.peek(2) == 'L' {
                self.current += 3;
            } else {
                self.warning("Malformed number");
            }
        }
        self.add_literal_token(NUMBER, self.substr(self.start, self.current));
    }

    fn read_string(&mut self, strend: char) {
        while !self.ended() && self.peek(0) != strend {
            if self.peek(0) == '\n' {
                self.warning("Unterminated string");
                break;
            };
            self.current += 1;
        }
        if self.ended() {
            self.warning("Unterminated string");
        } else {
            self.current += 1;
            let literal: String = self.substr(self.start + 1, self.current - 1);
            self.add_literal_token(STRING, literal);
        }
    }

    fn read_multiline_string(&mut self) {
        let mut aline = self.line;
        while !self.ended() && self.peek(0) != ']' && self.peek(1) != ']' {
            if self.peek(0) == '\n' {
                aline += 1
            };
            self.current += 1;
        }
        if self.ended() {
            self.warning("Unterminated string");
        } else {
            self.current += 1;
            let literal: String = self.substr(self.start + 2, self.current);
            self.add_literal_token(MULTILINE_STRING, literal);
        }
        self.current += 2;
        self.line = aline
    }
}

struct CommentInfo {
    line: usize,
    start: usize,
    current: usize,
    size: usize,
    code: Vec<char>,
    filename: String,
    tokens: Vec<Comment>,
    errored: bool,
}

impl CommentInfo {
    fn new(code: String, filename: String) -> Self {
        let chars = code.chars();
        Self {
            line: 1,
            start: 0,
            current: 0,
            size: chars.clone().count(),
            code: chars.collect(),
            filename,
            tokens: Vec::new(),
            errored: false,
        }
    }

    fn ended(&self) -> bool {
        self.current >= self.size
    }

    fn at(&self, pos: usize) -> char {
        if pos >= self.size {
            return 0 as char;
        }
        self.code[pos]
    }

    fn peek(&self, pos: usize) -> char {
        let pos: usize = self.current + pos;
        if pos >= self.size {
            return 0 as char;
        }
        self.at(pos)
    }

    //isNumber: c.is_ascii_digit()
    //isChar: c.is_ascii_alphabetic()
    //isCharOrNumber: c.is_ascii_alphanumeric()

    fn substr(&self, start: usize, end: usize) -> String {
        let mut result: String = String::new();
        for i in start..end {
            if i >= self.size {
                break;
            }
            result.push(self.at(i));
        }
        result
    }

    fn add_literal_token(&mut self, kind: CommentType, literal: String) {
        self.tokens.push(Comment::new(kind, literal, self.line));
    }
    fn warning(&mut self, message: &str) {
        println!(
            "Error in file \"{}\" at line {}!\nError: \"{}\"\n",
            self.filename, self.line, message
        );
        self.errored = true;
    }
    fn read_comment(&mut self) {
        while !self.ended() && self.peek(0) != '\n' {
            self.current += 1;
        }
        if self.ended() {
            self.warning("Unterminated string");
        } else {
            self.current += 1;
            let literal: String = self.substr(self.start, self.current);
            self.add_literal_token(COMMENT, literal);
        }
    }

    fn read_multiline_comment(&mut self) {
        let mut aline = self.line;
        while !self.ended() {
            if self.peek(0) == '\n' {
                aline += 1
            };
            if self.peek(0) == ']' && self.peek(1) == ']' {
                self.current += 2;
                break;
            }
            self.current += 1;
        }
        if self.ended() {
            self.warning("Unterminated multiline comment");
        } else {
            self.current += 1;
            let literal: String = self.substr(self.start, self.current);
            self.add_literal_token(MULTILINE_COMMENT, literal);
        }
        self.line = aline
    }
}

pub fn scan_code(code: String, filename: String) -> Result<(Vec<Token>, Vec<Comment>), String> {
    let mut i = CodeInfo::new(code.clone(), filename.clone());
    let mut comments = CommentInfo::new(code.clone(), filename.clone());
    while !i.ended() {
        i.start = i.current;
        let c: char = i.read_next();
        match c {
            ',' => i.add_token(COMMA),
            '.' => {
                if i.peek(0) == '.' {
                    i.current += 1;
                    let f: char = i.peek(0);
                    if f == '.' {
                        i.current += 1;
                        i.add_token(THREEDOTS);
                    } else {
                        i.add_token(TWODOTS);
                    }
                } else {
                    i.add_token(DOT);
                }
            }
            ';' => i.add_token(SEMICOLON),
            '+' => i.add_token(PLUS),
            '-' => match i.peek(0) {
                '-' => match i.peek(1) {
                    '[' => {
                        if i.peek(2) == '[' {
                            comments.read_multiline_comment();
                            i.current = comments.current + 1;
                        }
                    }
                    _ => {
                        comments.read_comment();
                        i.current = comments.current + 1;
                    }
                },
                _ => i.add_token(MINUS),
            },
            '*' => i.add_token(STAR),
            '^' => i.add_token(CARET),
            '#' => i.add_token(HASHTAG),
            '/' => i.add_token(SLASH),
            '%' => i.add_token(PERCENTUAL),
            '~' => {
                if i.peek(0) == '=' {
                    i.current += 1;
                    i.add_token(NOT_EQUAL);
                } else {
                    i.add_token(TILDE);
                }
            }
            '=' => i.compare_and_add('=', EQUAL, DEFINE),

            '<' => i.match_and_add('=', SMALLER_EQUAL, '<', LEFT_SHIFT, SMALLER),
            '>' => i.match_and_add('=', BIGGER_EQUAL, '>', RIGHT_SHIFT, BIGGER),
            ':' => i.add_token(COLON),
            '$' => i.add_token(DOLLAR),
            ' ' | '\r' | '\t' => {}
            '\n' => i.line += 1,
            '"' | '\'' => i.read_string(c),

            '(' => i.add_token(ROUND_BRACKET_OPEN),
            ')' => i.add_token(ROUND_BRACKET_CLOSED),
            '[' => {
                if i.peek(0) == '[' {
                    i.read_multiline_string();
                } else {
                    i.add_token(SQUARE_BRACKET_OPEN)
                }
            }
            ']' => i.add_token(SQUARE_BRACKET_CLOSED),
            '{' => i.add_token(CURLY_BRACKET_OPEN),
            '}' => i.add_token(CURLY_BRACKET_CLOSED),

            '&' => i.add_token(BIT_AND),
            '|' => i.add_token(BIT_OR),

            _ => {
                if c.is_ascii_digit() {
                    if c == '0' {
                        match i.peek(0) {
                            'x' | 'X' => {
                                i.current += 1;
                                i.read_number(
                                    |c| {
                                        let c = *c;
                                        c.is_ascii_digit()
                                            || (c >= 'a' && c <= 'f')
                                            || (c >= 'A' && c <= 'F')
                                    },
                                    false,
                                );
                            }
                            'b' | 'B' => {
                                i.current += 1;
                                i.read_number(
                                    |c| {
                                        let c = *c;
                                        c == '0' || c == '1'
                                    },
                                    false,
                                );
                            }
                            _ => i.read_number(char::is_ascii_digit, true),
                        }
                    } else {
                        i.read_number(char::is_ascii_digit, true);
                    }
                } else if c.is_ascii_alphabetic() || c == '_' {
                    while {
                        let c = i.peek(0);
                        c.is_ascii_alphanumeric() || c == '_'
                    } {
                        i.current += 1
                    }
                    let string: String = i.substr(i.start, i.current);
                    let kind: TokenType = match string.as_str() {
                        "if" => IF,
                        "elseif" => ELSEIF,
                        "else" => ELSE,
                        "for" => FOR,
                        "in" => IN,
                        "while" => WHILE,
                        "until" => UNTIL,
                        "local" => LOCAL,
                        "function" => FN,
                        "method" => METHOD,
                        "return" => RETURN,
                        "true" => TRUE,
                        "false" => FALSE,
                        "nil" => NIL,
                        "loop" => LOOP,
                        "break" => BREAK,
                        "try" => TRY,
                        "then" => THEN,
                        "do" => DO,
                        "end" => END,
                        "and" => AND,
                        "or" => OR,
                        "not" => NOT,
                        "repeat" => REPEAT,
                        _ => IDENTIFIER,
                    };
                    i.add_token(kind);
                } else {
                    i.warning(format!("Unexpected character '{}'", c).as_str());
                }
            }
        }
    }
    if i.errored {
        return Err(String::from(
            "Cannot continue until the above errors are fixed",
        ));
    }
    i.add_literal_token(EOF, String::from("<end>"));
    Ok((i.tokens, comments.tokens))
}
