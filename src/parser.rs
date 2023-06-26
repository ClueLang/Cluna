use crate::lexer::{Token, TokenType};
use std::collections::VecDeque;

macro_rules! vec_deque {
    ($($elem:expr),*) => {
      VecDeque::from([$(($elem)),*])
    };
}

pub type Expression = VecDeque<ComplexToken>;
pub type FunctionArgs = Vec<String>;
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
        names: Vec<ComplexToken>,
        values: Vec<Expression>,
        line: usize,
        column: usize,
    },
    Table {
        data: Vec<(Option<Expression>, Expression)>,
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
        iter: String,
        start: Expression,
        end: Expression,
        step: Option<Expression>,
        code: CodeBlock,
        line: usize,
        column: usize,
    },
    ForFuncLoop {
        iters: Vec<String>,
        expr: Expression,
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
    #[inline]
    pub fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens,
            column: 0,
            current: 0,
            line: 1,
            expr: Expression::with_capacity(16),
        }
    }

    #[inline]
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

    fn assert_compare(&self, expected: TokenType, error: &str) -> Result<(), String> {
        if let Some(t) = self.peek() {
            if t.kind() != expected {
                return Err(format!(
                    "Expected '{}' got '{}' at line {}",
                    error,
                    &t.lexeme(),
                    t.line()
                ));
            }
        }

        Ok(())
    }

    fn assert_end<T>(&mut self, tocheck: &Token, end: OptionalEnd, iftrue: T) -> Result<T, String> {
        if let Some((kind, lexeme)) = end {
            if tocheck.kind() != kind {
                return Err(format!(
                    "Expected '{}' got '{}' at line {}",
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
                "Expected '{}' got '{}' at line {}",
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
                "Expected '{}' got '{}' at line {}",
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

    #[inline]
    fn current(&self) -> &Token {
        &self.tokens[self.current.saturating_sub(1)]
    }

    fn look_back(&self) -> Option<&Token> {
        (self.current > 2).then(|| &self.tokens[self.current - 2])
    }

    #[inline]
    fn is_statement(&self) -> bool {
        use TokenType::*;
        matches!(
            self.peek().map(|t| t.kind()),
            Some(Identifier | Function | Local | If | While | For | Repeat | Do | Return | Break)
        )
    }

    fn find_expressions(&mut self, end: OptionalEnd) -> Result<Vec<Expression>, String> {
        let mut exprs = vec![];
        loop {
            let expr = self.parse_expression(None)?;
            self.advance_if(TokenType::Comma);
            let t = self.current().clone();
            exprs.push(expr);

            if t.kind() != TokenType::Comma {
                if let Some((end_token, expected_end)) = end {
                    if self.peek().map_or(false, |t| t.kind() != end_token) {
                        self.advance();

                        return Err(format!(
                            "Expected '{}' got '{}' at line {}",
                            expected_end,
                            &t.lexeme(),
                            t.line()
                        ));
                    }
                }
                return Ok(exprs);
            }
        }
    }

    fn is_arg(&self, kind: TokenType) -> bool {
        use TokenType::*;
        matches!(kind, String | MultilineString | LeftBrace)
    }

    fn parse_call(&mut self) -> Result<Vec<Expression>, String> {
        if self.advance_if(TokenType::RightParen) {
            Ok(vec![])
        } else {
            let exprs = self.find_expressions(Some((TokenType::RightParen, ")")));
            self.advance();
            exprs
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
                    LeftBracket => {
                        let index = self.parse_expression(Some((RightBracket, "]")))?;
                        self.assert(TokenType::RightBracket, "]")?;

                        expr.push_back(ComplexToken::Symbol("[".to_owned()));
                        expr.push_back(ComplexToken::Expr(index));
                        expr.push_back(ComplexToken::Symbol("]".to_owned()));
                    }
                    // String | MultilineString | LeftBrace => {
                    //     // implicit function calls in lua such require "os"
                    //     // should be handled here
                    //     if !self
                    //         .look_back()
                    //         .map_or(false, |t| t.kind() == TokenType::Identifier)
                    //     {
                    //         break;
                    //     } else {
                    //         self.parse_call()?;
                    //     }
                    // }
                    _ => {
                        self.go_back();
                        break;
                    }
                }
            } else {
                break;
            }
        }
        Ok(ComplexToken::Ident { expr, line })
    }

    fn parse_identifier_statement(&mut self) -> Result<ComplexToken, String> {
        let ident = self.parse_identifier()?;
        let ComplexToken::Ident { expr, .. } = &ident else {unreachable!()};

        if let Some(ComplexToken::Call(_)) = expr.back() {
            return Ok(ident);
        }

        if let Some(
            t @ Token {
                kind: TokenType::Equals | TokenType::Comma,
                ..
            },
        ) = self.advance()
        {
            let mut names = vec![ident];

            if t.kind() == TokenType::Comma {
                while !self.done() {
                    let ident = self.parse_identifier()?;
                    names.push(ident);
                    if !self.advance_if(TokenType::Comma) {
                        break;
                    }
                }
                self.assert(TokenType::Equals, "=")?;
            } else if self.current().kind() != TokenType::Equals {
                return Err(format!(
                    "Expected '=' got {} at line {}",
                    self.current().lexeme(),
                    self.current().line()
                ));
            }

            let values = self.find_expressions(None)?;
            self.advance_if(TokenType::Semicolon);

            return Ok(ComplexToken::Alter {
                names,
                values,
                line: self.line,
                column: self.column,
            });
        }

        Err(format!(
            "Expected assignment or function call got {} at line {}",
            self.current().lexeme(),
            self.current().line()
        ))
    }

    fn check_val(&mut self) -> bool {
        use TokenType::*;
        match self.peek().map(|t|t.kind()){
            //literals
            Some(Number
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
                | Hash
                | Not)=>{
                    true
                }
            _=>{
                false
            }
        }
    }

    fn parse_table(&mut self) -> Result<ComplexToken, String> {
        let mut data = vec![];

        while let Some(t) = self.advance().cloned() {
            use TokenType::*;
            match t.kind() {
                Identifier => {
                    if self.advance_if(TokenType::Equals) {
                        data.push((
                            Some(vec_deque![ComplexToken::Symbol(t.lexeme())]),
                            self.parse_expression(None)?,
                        ));
                    } else {
                        self.go_back();
                        data.push((None, self.parse_expression(None)?));
                    }
                }
                LeftBracket => {
                    let expr = self.parse_expression(Some((RightBracket, "]")))?;
                    self.assert(TokenType::RightBracket, "]")?;
                    self.assert(TokenType::Equals, "=")?;

                    let value = self.parse_expression(None)?;

                    data.push((Some(expr), value));
                }
                RightBrace => {
                    break;
                }
                Comma | Semicolon => {
                    continue;
                }
                _ => {
                    self.go_back();
                    let value = self.parse_expression(None)?;
                    data.push((None, value));
                }
            }
        }

        Ok(ComplexToken::Table {
            data,
            line: self.line,
            column: self.column,
        })
    }

    fn parse_code_block_common(scope: &mut usize, in_special_do: &mut bool, t: &Token) -> bool {
        use TokenType::*;

        match t.kind() {
            Function | Repeat | If => {
                *scope += 1;
                true
            }
            Do => {
                if !*in_special_do {
                    *scope += 1;
                }
                *in_special_do = false;
                true
            }
            While | For => {
                *scope += 1;
                *in_special_do = true;
                true
            }
            _ => false,
        }
    }
    fn parse_code_block(&mut self) -> Result<CodeBlock, String> {
        let start = self.current;
        let mut scope = 0;
        let mut in_special_do = false;

        while let Some(t) = self.advance() {
            use TokenType::*;
            if !Self::parse_code_block_common(&mut scope, &mut in_special_do, t) {
                match t.kind() {
                    End => {
                        if scope == 0 {
                            return Ok(CodeBlock {
                                code: parse_tokens(
                                    &self.tokens[start..self.current.saturating_sub(1)],
                                )?,
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
        }

        Err(format!("Expected 'end' at line {}", self.line))
    }

    fn parse_repeat_block(&mut self) -> Result<CodeBlock, String> {
        let start = self.current;
        let mut scope = 0;
        let mut in_special_do = false;

        while let Some(t) = self.advance() {
            use TokenType::*;
            if !Self::parse_code_block_common(&mut scope, &mut in_special_do, t) {
                match t.kind() {
                    End => {
                        if scope == 0 {
                            return Err(format!("Error: Unexpected 'end' at line {}", self.line));
                        } else {
                            scope -= 1;
                        }
                    }
                    Until => {
                        if scope == 0 {
                            return Ok(CodeBlock {
                                code: parse_tokens(
                                    &self.tokens[start..self.current.saturating_sub(1)],
                                )?,
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
        }

        Err(format!("Expected 'until' at line {}", self.line))
    }

    fn parse_if_block(&mut self) -> Result<CodeBlock, String> {
        let start = self.current;
        let mut scope = 0;
        let mut in_special_do = false;

        while let Some(t) = self.advance() {
            use TokenType::*;
            if !Self::parse_code_block_common(&mut scope, &mut in_special_do, t) {
                match t.kind() {
                    ElseIf | Else | End => {
                        if scope == 0 {
                            return Ok(CodeBlock {
                                code: parse_tokens(
                                    &self.tokens[start..self.current.saturating_sub(1)],
                                )?,
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
        }

        Err(format!("Expected 'end' at line {}", self.line))
    }

    fn parse_expression(&mut self, end: OptionalEnd) -> Result<Expression, String> {
        let mut expr = Expression::with_capacity(16);
        let start = self.current;

        let last = loop {
            use TokenType::*;
            if let Some(t) = self.advance().cloned() {
                match t.kind() {
                    Identifier => {
                        self.go_back();
                        let ident = self.parse_identifier()?;
                        expr.push_back(ident);

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
                    LeftBrace => {
                        let table = self.parse_table()?;
                        expr.push_back(table);
                        if self.check_val() {
                            self.advance();
                            break t;
                        }
                    }
                    // unary ops
                    Hash | Not => {
                        expr.push_back(ComplexToken::Symbol(t.lexeme()));
                        if !self.check_val() {
                            return Err(format!(
                                "Expected expression after unary op {} at line {}",
                                &t.lexeme(),
                                t.line()
                            ));
                        }
                    }
                    // binary ops
                    Plus | Star | Slash | Caret | Percent | DoubleDot | LessThan
                    | LessThanOrEqual | GreaterThan | GreaterThanOrEqual | DoubleEquals
                    | NotEquals | And | Or => {
                        if expr.is_empty() {
                            return Err(format!(
                                "Expected expression before binary op {} at line {}",
                                &t.lexeme(),
                                t.line()
                            ));
                        }
                        expr.push_back(ComplexToken::Symbol(t.lexeme()));

                        if !self.check_val() {
                            return Err(format!(
                                "Expected expression after binary op {} at line {}",
                                &t.lexeme(),
                                t.line()
                            ));
                        }
                    }
                    Minus => {
                        if expr.is_empty() {
                            expr.push_back(ComplexToken::Symbol("-".to_owned()));
                            continue;
                        } else {
                            expr.push_back(ComplexToken::Symbol(t.lexeme()));
                        }

                        if !self.check_val() {
                            return Err(format!(
                                "Expected expression after binary op {} at line {}",
                                &t.lexeme(),
                                t.line()
                            ));
                        }
                    }
                    Function
                        if self
                            .peek()
                            .map_or(false, |t| t.kind() == TokenType::LeftParen) =>
                    {
                        self.assert(TokenType::LeftParen, "(")?;
                        let args = self.parse_function_args()?;
                        let body = self.parse_code_block()?;

                        expr.push_back(ComplexToken::Lambda {
                            args,
                            body,
                            line: self.line,
                            column: self.column,
                        });
                    }
                    LeftParen => {
                        let exprs = self.parse_expression(Some((RightParen, ")")))?;
                        expr.push_back(ComplexToken::Expr(exprs));
                        self.assert(TokenType::RightParen, ")")?;

                        if self.check_val() {
                            break t;
                        }
                    }
                    _ => {
                        self.go_back();
                        break t;
                    }
                }
            } else {
                break self.current().clone();
            }
        };

        if expr.is_empty() {
            return Err(format!(
                "Expected expression got {} at line {}",
                &last.lexeme(),
                last.line()
            ));
        }

        self.assert_end(&last, end, expr)
    }

    fn parse_local_variable(&mut self) -> Result<ComplexToken, String> {
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
            self.find_expressions(None)?
        } else {
            vec![]
        };
        self.advance_if(TokenType::Semicolon);

        Ok(ComplexToken::Variable {
            names,
            values,
            line: self.line,
            column: self.column,
        })
    }

    fn parse_function_args(&mut self) -> Result<FunctionArgs, String> {
        let mut args = FunctionArgs::new();
        loop {
            use TokenType::*;
            let name = {
                let t = self.advance().unwrap().clone();
                match t.kind() {
                    TokenType::Identifier => t,
                    TokenType::TripleDot => {
                        self.assert_compare(TokenType::RightParen, ")")?;
                        t
                    }
                    TokenType::RightParen => {
                        break;
                    }
                    _ => {
                        return Err(format!(
                            "Expected identifier got {} at line {}",
                            t.lexeme(),
                            t.line()
                        ));
                    }
                }
            };

            if let Some(t) = self.advance() {
                match t.kind() {
                    Comma => args.push(name.lexeme()),
                    RightParen => {
                        args.push(name.lexeme());
                        break;
                    }
                    _ => return Err(format!("Expected ',' or ')' got {}", t.lexeme())),
                }
            } else {
                return Err(format!(
                    "Unexpected end of file at line {}. Expected '<args>'",
                    self.line
                ));
            }
        }

        Ok(args)
    }

    fn parse_function(&mut self, local: bool) -> Result<ComplexToken, String> {
        let name = {
            use TokenType::*;

            let mut expr = Expression::new();
            loop {
                let t = self.advance().ok_or("Unexpected end of file")?.clone();

                match t.kind() {
                    Identifier => {
                        let nt = self.peek().ok_or("Unexpected end of file")?;
                        if nt.kind() == Identifier {
                            return Err(format!(
                                "Unexpected {} at line {}",
                                nt.lexeme(),
                                nt.line()
                            ));
                        }
                        expr.push_back(ComplexToken::Symbol(t.lexeme()));
                    }
                    Dot | Colon => {
                        if self
                            .peek()
                            .map_or(false, |t| t.kind() == TokenType::Identifier)
                        {
                            expr.push_back(ComplexToken::Symbol(t.lexeme()));
                        } else {
                            return Err(format!(
                                "{} should only be used when indexing at line {}",
                                t.lexeme(),
                                t.line()
                            ));
                        }
                    }
                    LeftParen => break,
                    _ => {
                        return Err(format!(
                            "Expected identifier, ':' or '.' got {} at line {}",
                            t.lexeme(),
                            t.line()
                        ))
                    }
                }
            }
            expr
        };

        let args = self.parse_function_args()?;
        let body = self.parse_code_block()?;

        Ok(ComplexToken::Function {
            local,
            name,
            args,
            body,
            line: self.line,
            column: self.column,
        })
    }

    fn parse_if_else_chain(&mut self) -> Result<ComplexToken, String> {
        let condition = self.parse_expression(Some((TokenType::Then, "then")))?;
        self.advance();
        let body = self.parse_if_block()?;

        let next = match self.current().kind() {
            TokenType::Else => {
                let body = self.parse_code_block()?;
                Some(Box::new(ComplexToken::IfStatement {
                    condition: Expression::new(),
                    body,
                    next: None,
                    line: self.line,
                    column: self.column,
                }))
            }
            TokenType::ElseIf => Some(Box::new(self.parse_if_else_chain()?)),
            TokenType::End => None,
            _ => {
                return Err(format!(
                    "Expected 'else', 'elseif' or 'end' got {} at line {}",
                    self.current().lexeme(),
                    self.current().line()
                ))
            }
        };

        Ok(ComplexToken::IfStatement {
            condition,
            body,
            next,
            line: self.line,
            column: self.column,
        })
    }

    fn parse_for_loop(&mut self) -> Result<ComplexToken, String> {
        let mut iters = vec![];
        let mut for_func = false;

        while let Some(t) = self.advance() {
            match t.kind() {
                TokenType::Identifier => {
                    iters.push(t.lexeme());
                }
                TokenType::Comma => {
                    continue;
                }
                TokenType::In => {
                    for_func = true;
                    break;
                }
                TokenType::Equals => {
                    if iters.len() != 1 {
                        return Err(format!(
                            "Expected 1 in numeric for loop identifier got {} at line {}",
                            iters.len(),
                            t.line()
                        ));
                    }
                    break;
                }
                _ => {
                    return Err(format!(
                        "Expected identifier, ',' or 'in' got {} at line {}",
                        t.lexeme(),
                        t.line()
                    ))
                }
            }
        }

        if for_func {
            let expr = self.parse_expression(None)?;
            self.assert_advance(TokenType::Do, "do")?;

            let code = self.parse_code_block()?;

            Ok(ComplexToken::ForFuncLoop {
                iters,
                expr,
                code,
                line: self.line,
                column: self.column,
            })
        } else {
            let start = self.parse_expression(Some((TokenType::Comma, ",")))?;
            self.assert_advance(TokenType::Comma, ",")?;
            let end = self.parse_expression(None)?;
            let step = if self.advance_if(TokenType::Comma) {
                Some(self.parse_expression(Some((TokenType::Do, "do")))?)
            } else {
                None
            };

            self.assert_advance(TokenType::Do, "do")?;
            let code = self.parse_code_block()?;

            Ok(ComplexToken::ForLoop {
                iter: iters.pop().unwrap(),
                start,
                end,
                step,
                code,
                line: self.line,
                column: self.column,
            })
        }
    }
}

pub fn parse_tokens(tokens: &[Token]) -> Result<Expression, String> {
    let mut parser = Parser::new(tokens);

    while let Some(token) = parser.advance() {
        use TokenType::*;

        match token.kind() {
            Local => match parser.peek().map(|t| t.kind()) {
                Some(TokenType::Function) => {
                    parser.advance();
                    let function = parser.parse_function(true)?;
                    parser.expr.push_back(function);
                }
                Some(TokenType::Identifier) => {
                    parser.advance();
                    let variable = parser.parse_local_variable()?;
                    parser.expr.push_back(variable);
                }
                _ => {
                    return Err(format!(
                        "Expected 'function' or identifier got {} at line {}",
                        parser.peek().unwrap().lexeme(),
                        parser.peek().unwrap().line()
                    ))
                }
            },
            Identifier => {
                parser.go_back();
                let ident = parser.parse_identifier_statement()?;
                parser.expr.push_back(ident);
            }
            Function => {
                let function = parser.parse_function(false)?;
                parser.expr.push_back(function);
            }
            If => {
                let ifs = parser.parse_if_else_chain()?;
                parser.expr.push_back(ifs);
            }
            While => {
                let condition = parser.parse_expression(Some((Do, "do")))?;
                parser.advance();

                let body = parser.parse_code_block()?;
                parser.expr.push_back(ComplexToken::WhileLoop {
                    condition,
                    body,
                    line: parser.line,
                    column: parser.column,
                });
            }
            For => {
                let for_loop = parser.parse_for_loop()?;
                parser.expr.push_back(for_loop);
            }
            Repeat => {
                let body = parser.parse_repeat_block()?;
                let condition = parser.parse_expression(None)?;
                parser.expr.push_back(ComplexToken::RepeatLoop {
                    condition,
                    body,
                    line: parser.line,
                    column: parser.column,
                });
            }
            Break => {
                parser.expr.push_back(ComplexToken::Break);
            }
            Do => {
                let block = parser.parse_code_block()?;
                parser.expr.push_back(ComplexToken::DoBlock(block));
            }
            Return => {
                let exprs = if parser
                    .peek()
                    .map_or(false, |t| t.kind() != TokenType::Semicolon)
                {
                    Some(parser.find_expressions(None)?)
                } else {
                    None
                };
                parser.advance_if(TokenType::Semicolon);

                if !parser.done() {
                    return Err(format!(
                        "Return must be the last statement in a block at line {}",
                        parser.line
                    ));
                }
                parser.expr.push_back(ComplexToken::Return(exprs));
            }
            _ => Err(format!(
                "Unexpected token {} at line {}",
                token.lexeme(),
                token.line()
            ))?,
        }
    }

    Ok(parser.expr)
}
