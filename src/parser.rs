use std::collections::VecDeque;

use crate::lexer::{Token, TokenType};

macro_rules! vec_deque {
    ($($elem:expr),*) => {
      VecDeque::from([$(($elem)),*])
    };
}

pub type Expression = VecDeque<ComplexToken>;
pub type FunctionArgs = Vec<(String, Option<(Expression, usize)>)>;
type OptionalEnd = Option<(TokenType, &'static str)>;

#[derive(Debug, Clone, PartialEq)]
pub enum ComplexToken {
    Variable {
        names: Vec<String>,
        values: Vec<Expression>,
        line: usize,
        column: usize,
    },
    Alter {
        names: Vec<String>,
        values: Vec<Expression>,
        line: usize,
        column: usize,
    },
    Table {
        names: Vec<String>,
        values: Vec<(Option<Expression>, Expression)>,
        line: usize,
        column: usize,
    },
    Function {
        local: bool,
        name: Expression,
        args: FunctionArgs,
        body: CodeBlock,
        line: usize,
        column: usize,
    },
    Lambda {
        args: FunctionArgs,
        body: CodeBlock,
        line: usize,
        column: usize,
    },
    IfStatement {
        condition: Expression,
        body: CodeBlock,
        next: Option<Box<ComplexToken>>,
        line: usize,
        column: usize,
    },
    WhileLoop {
        condition: Expression,
        body: CodeBlock,
        line: usize,
        column: usize,
    },
    RepeatLoop {
        condition: Expression,
        body: CodeBlock,
        line: usize,
        column: usize,
    },
    ForLoop {
        iterator: String,
        start: Expression,
        end: Expression,
        alter: Expression,
        code: CodeBlock,
        line: usize,
        column: usize,
    },
    ForFuncLoop {
        iterator: String,
        start: Expression,
        end: Expression,
        alter: Expression,
        code: CodeBlock,
        line: usize,
        column: usize,
    },
    Ident {
        expr: Expression,
        line: usize,
    },
    Symbol(String),
    Call(Vec<Expression>),
    Expr(Expression),
    DoBlock(CodeBlock),
    Return(Option<Vec<Expression>>),
    Break,
}

#[derive(Clone, Debug, PartialEq)]
pub struct CodeBlock {
    pub start: usize,
    pub code: Expression,
    pub end: usize,
}

struct Parser<'a> {
    tokens: &'a [Token],
    expr: Expression,
    current: usize,
    line: usize,
    column: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens,
            column: 0,
            current: 0,
            line: 1,
            expr: Expression::with_capacity(16),
        }
    }

    fn done(&self) -> bool {
        self.current >= self.tokens.len()
    }

    fn advance(&mut self) -> Option<&Token> {
        if self.done() {
            None
        } else {
            if self.tokens[self.current].line() != self.line {
                self.line = self.tokens[self.current].line();
                self.column = 0;
            }
            let result = Some(&self.tokens[self.current]);
            self.current += 1;
            self.column += 1;
            result
        }
    }

    fn advance_to(&mut self, offset: usize) -> Option<&Token> {
        if self.done() {
            None
        } else {
            if self.tokens[self.current + offset - 1].line() != self.line {
                self.line = self.tokens[self.current + offset - 1].line();
                self.column = 0;
            }
            let result = Some(&self.tokens[self.current + offset - 1]);
            self.current += offset;
            self.column += offset;
            result
        }
    }

    fn advance_if(&mut self, token: TokenType) -> bool {
        if self.done() {
            false
        } else if self.tokens[self.current].kind() == token {
            self.advance();
            true
        } else {
            false
        }
    }

    fn assert_end<T>(&mut self, tocheck: &Token, end: OptionalEnd, iftrue: T) -> Result<T, String> {
        if let Some((kind, lexeme)) = end {
            if tocheck.kind() != kind {
                return Err(format!(
                    "Error: Expected '{}' got '{} at line {}'",
                    lexeme,
                    &tocheck.lexeme(),
                    tocheck.line(),
                ));
            }
        }
        Ok(iftrue)
    }

    fn assert(&mut self, expected: TokenType, error: &str) -> Result<(), String> {
        if !self.advance_if(expected) {
            let t = self.peek().ok_or("Unexpected end of file")?;
            return Err(format!(
                "Expected: {} got {} at line {}",
                error,
                &t.lexeme(),
                t.line()
            ));
        }
        Ok(())
    }

    fn assert_advance(&mut self, expected: TokenType, error: &str) -> Result<&Token, String> {
        if !self.advance_if(expected) {
            let t = self.peek().ok_or("Unexpected end of file")?;
            return Err(format!(
                "Expected: {} got {} at line {}",
                error,
                &t.lexeme(),
                t.line()
            ));
        }
        Ok(self.current())
    }

    fn go_back(&mut self) -> Option<&Token> {
        if self.current == 0 {
            None
        } else {
            if self.tokens[self.current - 1].line() != self.line {
                self.line = self.tokens[self.current].line();
                self.column = 0;
            } else {
                self.column -= 1;
            }
            self.current -= 1;

            Some(&self.tokens[self.current])
        }
    }

    fn peek(&self) -> Option<Token> {
        if self.done() {
            None
        } else {
            self.tokens.get(self.current).cloned()
        }
    }

    fn peek_at(&self, offset: usize) -> Option<&Token> {
        if self.done() {
            None
        } else {
            self.tokens.get(self.current + offset - 1)
        }
    }

    fn current(&self) -> &Token {
        &self.tokens[self.current.saturating_sub(1)]
    }

    fn look_back(&self) -> Option<&Token> {
        (self.current > 2).then(|| &self.tokens[self.current - 2])
    }

    fn find_expressions(&mut self, end: OptionalEnd) -> Result<Vec<Expression>, String> {
        let mut exprs = vec![];
        loop {
            let expr = self.parse_expression(None)?;
            let t = self.current().clone();
            exprs.push(expr);
            if t.kind() != TokenType::Comma {
                return self.assert_end(&t, end, exprs);
            }
        }
    }

    fn is_arg(&self, kind: TokenType) -> bool {
        use TokenType::*;
        matches!(kind, String | MultilineString | LeftBrace)
    }

    fn parse_call(&mut self) -> Result<Vec<Expression>, String> {
        let is_implicit = self
            .look_back()
            .map(|t| t.kind() != TokenType::LeftParen)
            .unwrap_or(false);
        if is_implicit {
            let mut expr = Expression::with_capacity(8);
            if self.current().kind() == TokenType::RightBrace {
                expr.push_back(self.parse_table()?);
            } else {
                expr.push_back(ComplexToken::Symbol(self.current().lexeme()));
            }
            Ok(vec![expr])
        } else if self.advance_if(TokenType::RightParen) {
            Ok(vec![])
        } else {
            self.find_expressions(Some((TokenType::RightParen, ")")))
        }
    }

    fn parse_identifier(&mut self) -> Result<ComplexToken, String> {
        let line = self.current().line();
        let mut expr = Expression::with_capacity(8);
        loop {
            use TokenType::*;
            if let Some(t) = self.advance() {
                match t.kind() {
                    Identifier => expr.push_back(ComplexToken::Symbol(t.lexeme())),
                    LeftParen => {
                        let call = self.parse_call()?;
                        expr.push_back(ComplexToken::Call(call));
                    }
                    Dot => {
                        let t = self.advance().unwrap();
                        if t.kind() != Identifier {
                            return Err(format!(
                                "Expected identifier got {} at line {}",
                                t.lexeme(),
                                t.line()
                            ));
                        }
                        expr.push_back(ComplexToken::Symbol(".".to_owned()));
                        expr.push_back(ComplexToken::Symbol(t.lexeme()));
                    }
                    Colon => {
                        let t = self.advance().unwrap().clone();
                        if t.kind() != Identifier {
                            return Err(format!(
                                "Expected identifier got {} at line {}",
                                t.lexeme(),
                                t.line()
                            ));
                        }

                        if self.peek().unwrap().kind() != TokenType::LeftParen
                            && !self.is_arg(self.peek().unwrap().kind())
                        {
                            return Err(format!(
                                "Expected '(' or a single argument got {} at line {}",
                                t.lexeme(),
                                t.line()
                            ));
                        }
                        expr.push_back(ComplexToken::Symbol(":".to_owned()));
                        expr.push_back(ComplexToken::Symbol(t.lexeme()));
                    }
                    String | MultilineString | LeftBrace => {
                        // implicit function calls in lua such require "os"
                        // should be handled here
                        if !self
                            .look_back()
                            .map_or(false, |t| t.kind() == TokenType::Identifier)
                        {
                            break;
                        } else {
                            self.parse_call()?;
                        }
                    }
                    _ => {
                        break;
                    }
                }
            } else {
                break;
            }
        }
        Ok(ComplexToken::Ident { expr, line })
    }

    fn check_val(&mut self) -> bool {
        use TokenType::*;
        match self.peek().unwrap().kind(){
            //literals
            Number
                | String
                | MultilineString
                | Nil
                | Identifier
                | True
                | False
                | LeftBrace
                | LeftParen
                | LeftBracket
                | TripleDot
            // unary operators
                | Minus
                | Hash
                | Not=>{
                    self.advance();
                    true
                }
            _=>{
                false
            }
        }
    }

    fn parse_table(&mut self) -> Result<ComplexToken, String> {
        todo!()
    }

    fn parse_code_block(&mut self) -> Result<CodeBlock, String> {
        let start = self.current;
        let mut scope = 1;

        while let Some(t) = self.advance() {
            use TokenType::*;

            match t.kind() {
                Function | While | For | Repeat => {
                    scope += 1;
                }
                End => {
                    if scope == 0 {
                        return Ok(CodeBlock {
                            code: parse_tokens(&self.tokens[start..self.current])?,
                            start,
                            end: self.current,
                        });
                    } else {
                        scope -= 1;
                    }
                }
                Until => {
                    if scope == 0 {
                        return Err(format!("Error: Unexpected 'until' at line {}", self.line));
                    } else {
                        scope -= 1;
                    }
                }
                _ => {}
            }
        }

        Err(format!("Expected 'end' at line {}", self.line))
    }

    fn parse_repeat_block(&mut self) -> Result<CodeBlock, String> {
        let start = self.current;
        let mut scope = 1;

        while let Some(t) = self.advance() {
            use TokenType::*;

            match t.kind() {
                Function | While | For | Repeat => {
                    scope += 1;
                }
                End => {
                    if scope == 0 {
                        return Err(format!("Error: Unterminated repeat at line {}", self.line));
                    } else {
                        scope -= 1;
                    }
                }
                Until => {
                    if scope == 0 {
                        return Ok(CodeBlock {
                            code: parse_tokens(&self.tokens[start..self.current])?,
                            start,
                            end: self.current,
                        });
                    } else {
                        scope -= 1;
                    }
                }
                _ => {}
            }
        }

        Err(format!("Expected 'end' at line {}", self.line))
    }

    fn parse_expression(&mut self, end: OptionalEnd) -> Result<Expression, String> {
        let mut expr = Expression::with_capacity(16);
        let start = self.current;

        let last = loop {
            use TokenType::*;
            let t = self.advance().unwrap().clone();
            match t.kind() {
                Identifier => {
                    self.go_back();
                    let ident = self.parse_identifier()?;
                    expr.push_back(ident);

                    self.go_back();
                    if self.check_val() {
                        break t;
                    }
                }
                TripleDot | Number | True | False | String | MultilineString | Nil => {
                    expr.push_back(ComplexToken::Symbol(t.lexeme()));
                    if self.check_val() {
                        break t;
                    }
                }
                Minus => {
                    if self
                        .look_back()
                        .map(|t| t.kind() == TokenType::Number)
                        .unwrap_or(false)
                    {
                        // this is a binary operator
                        todo!()
                    } else {
                        // this is a unary operator

                        todo!()
                    }
                }

                _ => break t,
            }
        };

        if expr.is_empty() {
            return Err(format!(
                "Expected expression got {} at line {}",
                &last.lexeme(),
                last.line()
            ));
        }
        let current = self.current().clone();

        self.assert_end(&current, end, expr)
    }

    fn parse_variable(&mut self, local: bool) -> Result<ComplexToken, String> {
        let mut names = vec![];

        self.go_back();

        loop {
            let name = self.assert_advance(TokenType::Identifier, "<name>")?;
            names.push(name.lexeme());
            if !self.advance_if(TokenType::Comma) {
                break;
            }
        }

        let values = if self.advance_if(TokenType::Equals) {
            if !local {
                return Err(format!(
                    "Expected '=' at line {}",
                    self.peek().unwrap().lexeme(),
                ));
            }
            self.find_expressions(None)?
        } else {
            vec![]
        };

        if local {
            Ok(ComplexToken::Variable {
                names,
                values,
                line: self.line,
                column: self.column,
            })
        } else {
            Ok(ComplexToken::Alter {
                names,
                values,
                line: self.line,
                column: self.column,
            })
        }
    }

    fn parse_function(&mut self, named: bool, local: bool) -> Result<(), String> {
        todo!()
    }

    fn parse_local(&mut self) -> Result<(), String> {
        match self.peek().map(|t| t.kind()) {
            Some(TokenType::Function) => {
                self.advance();
                todo!("Local function")
            }
            Some(TokenType::Identifier) => {
                self.advance();
                let variable = self.parse_variable(true)?;
                self.expr.push_back(variable);
            }
            _ => {
                return Err(format!(
                    "Expected 'function' or identifier got {} at line {}",
                    self.peek().unwrap().lexeme(),
                    self.peek().unwrap().line()
                ))
            }
        }
        Ok(())
    }
}

pub fn parse_tokens(tokens: &[Token]) -> Result<Expression, String> {
    let mut parser = Parser::new(tokens);

    while let Some(token) = parser.advance() {
        use TokenType::*;
        match token.kind() {
            Local => parser.parse_local()?,
            Identifier => {}
            Function => {
                if parser.peek().map_or(false, |t| t.kind() != Identifier) {
                    // lambda function
                    todo!()
                } else {
                    // named function
                    todo!()
                }
            }
            If => {}
            While => {}
            For => {}
            Repeat => {}
            Break => {}
            Do => {}
            Return => {}
            _ => Err(format!(
                "Unexpected token {} at line {}",
                token.lexeme(),
                token.line()
            ))?,
        }
    }

    Ok(parser.expr)
}
