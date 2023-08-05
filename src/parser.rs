use crate::{
    error::{Diagnostic, DiagnosticLevel},
    lexer::{Token, TokenType},
    number::Number,
};
use std::{collections::VecDeque, rc::Rc};

macro_rules! vec_deque {
    ($($elem:expr),*) => {
      VecDeque::from([$(($elem)),*])
    };
}

pub type Expression = VecDeque<ComplexToken>;
pub type FunctionArgs = Vec<Rc<str>>;
type OptionalEnd = Option<(TokenType, &'static str)>;

#[derive(Debug, Clone, PartialEq)]
pub enum ComplexToken {
    Variable {
        names: Vec<(Rc<str>, bool)>,
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
        iter: Rc<str>,
        start: Expression,
        end: Expression,
        step: Option<Expression>,
        code: CodeBlock,
        line: usize,
        column: usize,
    },
    ForFuncLoop {
        iters: Vec<Rc<str>>,
        expr: Expression,
        stop: Option<Expression>,
        initial: Option<Expression>,
        code: CodeBlock,
        line: usize,
        column: usize,
    },
    Ident {
        expr: Expression,
        line: usize,
    },
    MultilineString(Rc<str>),
    Number(Number),
    Symbol(Rc<str>),
    Operator((Rc<str>, bool)),
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
    path: Option<String>,
    expr: Expression,
    current: usize,
    line: usize,
    column: usize,
}

impl<'a> Parser<'a> {
    #[inline]
    pub fn new(tokens: &'a [Token], path: Option<String>) -> Self {
        Self {
            tokens,
            path,
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

    fn assert_compare(&self, expected: TokenType, error: &str) -> Result<(), Diagnostic> {
        if let Some(t) = self.peek() {
            if t.kind() != expected {
                return Err(Diagnostic::expected_found(
                    error.to_owned(),
                    t.lexeme().to_string(),
                    self.path.clone(),
                    t.line(),
                    t.column(),
                ));
            }
        }

        Ok(())
    }

    fn assert_end<T>(
        &mut self,
        tocheck: &Token,
        end: OptionalEnd,
        iftrue: T,
    ) -> Result<T, Diagnostic> {
        if let Some((kind, lexeme)) = end {
            if tocheck.kind() != kind {
                return Err(Diagnostic::expected_found(
                    lexeme.to_string(),
                    tocheck.lexeme().to_string(),
                    self.path.clone(),
                    tocheck.line(),
                    tocheck.column(),
                ));
            }
        }

        Ok(iftrue)
    }

    fn assert(&mut self, expected: TokenType, error: &str) -> Result<(), Diagnostic> {
        if !self.advance_if(expected) {
            let t = self.peek().ok_or_else(|| {
                Diagnostic::unexpected(
                    "end of file".to_owned(),
                    self.path.clone(),
                    self.line,
                    self.column,
                )
            })?;
            return Err(Diagnostic::expected_found(
                error.to_owned(),
                t.lexeme().to_string(),
                self.path.clone(),
                t.line(),
                t.column(),
            ));
        }
        Ok(())
    }

    fn assert_advance(&mut self, expected: TokenType, error: &str) -> Result<&Token, Diagnostic> {
        if !self.advance_if(expected) {
            let t = self.peek().ok_or_else(|| {
                Diagnostic::unexpected(
                    "end of file".to_owned(),
                    self.path.clone(),
                    self.line,
                    self.column,
                )
            })?;
            return Err(Diagnostic::expected_found(
                error.to_owned(),
                t.lexeme().to_string(),
                self.path.clone(),
                t.line(),
                t.column(),
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

    #[inline]
    fn current(&self) -> &Token {
        &self.tokens[self.current.saturating_sub(1)]
    }

    fn find_expressions(&mut self, end: OptionalEnd) -> Result<Vec<Expression>, Diagnostic> {
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

                        return Err(Diagnostic::expected_found(
                            expected_end.to_owned(),
                            t.lexeme().to_string(),
                            self.path.clone(),
                            t.line(),
                            t.column(),
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

    fn parse_call(&mut self) -> Result<Vec<Expression>, Diagnostic> {
        if self.advance_if(TokenType::RightParen) {
            Ok(vec![])
        } else {
            let exprs = self.find_expressions(Some((TokenType::RightParen, ")")));
            self.advance();
            exprs
        }
    }

    fn parse_identifier(&mut self) -> Result<ComplexToken, Diagnostic> {
        let line = self.current().line();
        let mut expr = Expression::with_capacity(8);

        loop {
            use TokenType::*;
            if let Some(t) = self.advance() {
                match t.kind() {
                    Identifier => {
                        expr.push_back(ComplexToken::Symbol(t.lexeme().as_symbol()));
                    }
                    LeftParen => {
                        let call = self.parse_call()?;
                        expr.push_back(ComplexToken::Call(call));
                    }
                    Dot => {
                        let path = self.path.clone();
                        let line = self.line;
                        let column = self.column;

                        let t = self
                            .advance()
                            .ok_or_else(|| {
                                Diagnostic::expected("identifier".to_owned(), path, line, column)
                            })?
                            .clone();
                        if t.kind() != Identifier {
                            return Err(Diagnostic::expected_found(
                                "identifier".to_owned(),
                                t.lexeme().to_string(),
                                self.path.clone(),
                                t.line(),
                                t.column(),
                            ));
                        }
                        expr.push_back(ComplexToken::Symbol(".".into()));
                        expr.push_back(ComplexToken::Symbol(t.lexeme().as_symbol()));
                    }
                    Colon => {
                        let path = self.path.clone();
                        let line = self.line;
                        let column = self.column;
                        let t = self
                            .advance()
                            .ok_or_else(|| {
                                Diagnostic::expected("identifier".to_owned(), path, line, column)
                            })?
                            .clone();
                        if t.kind() != Identifier {
                            return Err(Diagnostic::expected_found(
                                "identifier".to_owned(),
                                t.lexeme().to_string(),
                                self.path.clone(),
                                t.line(),
                                t.column(),
                            ));
                        }

                        if self.peek().unwrap().kind() != TokenType::LeftParen
                            && !self.is_arg(self.peek().unwrap().kind())
                        {
                            return Err(Diagnostic::expected_found(
                                "'(' or a single argument".to_owned(),
                                t.lexeme().to_string(),
                                self.path.clone(),
                                t.line(),
                                t.column(),
                            ));
                        }
                        expr.push_back(ComplexToken::Symbol(":".into()));
                        expr.push_back(ComplexToken::Symbol(t.lexeme().as_symbol()));
                    }
                    LeftBracket => {
                        let index = self.parse_expression(Some((RightBracket, "]")))?;
                        self.assert(TokenType::RightBracket, "]")?;

                        expr.push_back(ComplexToken::Symbol("[".into()));
                        expr.push_back(ComplexToken::Expr(index));
                        expr.push_back(ComplexToken::Symbol("]".into()));
                    }
                    String | MultilineString => {
                        expr.push_back(ComplexToken::Call(vec![vec_deque![ComplexToken::Symbol(
                            t.lexeme().as_symbol(),
                        )]]));
                    }
                    LeftBrace => {
                        let table = self.parse_table()?;
                        expr.push_back(ComplexToken::Call(vec![vec_deque![table]]));
                    }
                    _ => {
                        self.go_back();
                        break;
                    }
                }

                if matches!(
                    self.peek().map(|t| t.kind()),
                    Some(Number | Nil | Identifier | True | False | TripleDot | Hash | Not)
                ) {
                    break;
                }
            } else {
                break;
            }
        }

        Ok(ComplexToken::Ident { expr, line })
    }

    fn parse_identifier_statement(&mut self) -> Result<ComplexToken, Diagnostic> {
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
                return Err(Diagnostic::expected_found(
                    "'='".to_owned(),
                    self.current().lexeme().to_string(),
                    self.path.clone(),
                    self.current().line(),
                    self.current().column(),
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

        Err(Diagnostic::expected_found(
            "assignment or function call".to_owned(),
            self.current().lexeme().to_string(),
            self.path.clone(),
            self.current().line(),
            self.current().column(),
        ))
    }

    fn check_val(&mut self) -> bool {
        use TokenType::*;
        matches!(
            self.peek().map(|t| t.kind()),
            Some(
                Number
                    | String
                    | MultilineString
                    | Nil
                    | Identifier
                    | True
                    | False
                    | LeftBrace
                    | TripleDot
                    | Hash
                    | Not
            )
        )
    }

    fn check_op(&mut self) -> bool {
        use TokenType::*;
        matches!(
            self.peek().map(|t| t.kind()),
            Some(
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
                    | Hash
                    | Not
                    | Tilde
                    | Minus
                    | Function,
            )
        )
    }

    fn parse_table(&mut self) -> Result<ComplexToken, Diagnostic> {
        let mut data = vec![];

        while let Some(t) = self.advance().cloned() {
            use TokenType::*;
            match t.kind() {
                Identifier => {
                    if self.advance_if(TokenType::Equals) {
                        data.push((
                            Some(vec_deque![ComplexToken::Symbol(t.lexeme().as_symbol())]),
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
    fn parse_code_block(&mut self) -> Result<CodeBlock, Diagnostic> {
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
                                    self.path.clone(),
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
                            return Err(Diagnostic::unexpected(
                                "'until'".to_owned(),
                                self.path.clone(),
                                self.line,
                                self.column,
                            ));
                        } else {
                            scope -= 1;
                        }
                    }
                    _ => {}
                }
            }
        }
        Err(Diagnostic::expected(
            "'end'".to_owned(),
            self.path.clone(),
            self.line,
            self.column,
        ))
    }

    fn parse_repeat_block(&mut self) -> Result<CodeBlock, Diagnostic> {
        let start = self.current;
        let mut scope = 0;
        let mut in_special_do = false;

        while let Some(t) = self.advance() {
            use TokenType::*;
            if !Self::parse_code_block_common(&mut scope, &mut in_special_do, t) {
                match t.kind() {
                    End => {
                        if scope == 0 {
                            return Err(Diagnostic::unexpected(
                                "'end'".to_owned(),
                                self.path.clone(),
                                self.line,
                                self.column,
                            ));
                        } else {
                            scope -= 1;
                        }
                    }
                    Until => {
                        if scope == 0 {
                            return Ok(CodeBlock {
                                code: parse_tokens(
                                    &self.tokens[start..self.current.saturating_sub(1)],
                                    self.path.clone(),
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

        Err(Diagnostic::expected(
            "'until'".to_owned(),
            self.path.clone(),
            self.line,
            self.column,
        ))
    }

    fn parse_if_block(&mut self) -> Result<CodeBlock, Diagnostic> {
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
                                    self.path.clone(),
                                )?,
                                start,
                                end: self.current,
                            });
                        } else {
                            scope -= 1;
                        }
                    }
                    ElseIf | Else => {
                        if scope == 0 {
                            return Ok(CodeBlock {
                                code: parse_tokens(
                                    &self.tokens[start..self.current.saturating_sub(1)],
                                    self.path.clone(),
                                )?,
                                start,
                                end: self.current,
                            });
                        }
                    }
                    Until => {
                        if scope == 0 {
                            return Err(Diagnostic::unexpected(
                                "'until'".to_owned(),
                                self.path.clone(),
                                self.line,
                                self.column,
                            ));
                        } else {
                            scope -= 1;
                        }
                    }
                    _ => {}
                }
            }
        }

        Err(Diagnostic::expected(
            "'end'".to_owned(),
            self.path.clone(),
            self.line,
            self.column,
        ))
    }

    fn parse_expression(&mut self, end: OptionalEnd) -> Result<Expression, Diagnostic> {
        let mut expr = Expression::with_capacity(16);

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
                    TripleDot | True | False | String | Nil => {
                        expr.push_back(ComplexToken::Symbol(t.lexeme().as_symbol()));
                        if self.check_val() {
                            break t;
                        }
                    }
                    Number => {
                        expr.push_back(ComplexToken::Number(t.lexeme().as_number().clone()));
                        if self.check_val() {
                            break t;
                        }
                    }
                    MultilineString => {
                        expr.push_back(ComplexToken::MultilineString(t.lexeme().as_symbol()));
                        if self.check_val() {
                            break t;
                        }
                    }
                    LeftBrace => {
                        let table = self.parse_table()?;
                        expr.push_back(table);
                        if self.check_val() {
                            break t;
                        }
                    }
                    // unary ops
                    Hash | Not => {
                        expr.push_back(ComplexToken::Operator((t.lexeme().as_symbol(), false)));
                        if !self.check_op() {
                            return Err(Diagnostic::expected_found(
                                "expression".to_owned(),
                                t.lexeme().to_string(),
                                self.path.clone(),
                                t.line(),
                                t.column(),
                            ));
                        }
                    }
                    // binary ops
                    Plus | Star | Slash | FloorDiv | Caret | Percent | DoubleDot | LessThan
                    | LessThanOrEqual | GreaterThan | GreaterThanOrEqual | DoubleEquals
                    | NotEquals | And | Or | BitAnd | BitOr | BitShiftLeft | BitShiftRight => {
                        if expr.is_empty() {
                            return Err(Diagnostic::expected_before(
                                "expression".to_owned(),
                                "binary op".to_owned(),
                                self.path.clone(),
                                t.line(),
                                t.column(),
                            ));
                        }
                        expr.push_back(ComplexToken::Operator((t.lexeme().as_symbol(), true)));

                        if !self.check_op() {
                            return Err(Diagnostic::expected_after(
                                "expression".to_owned(),
                                "binary op".to_owned(),
                                self.path.clone(),
                                t.line(),
                                t.column(),
                            ));
                        }
                    }
                    Minus => {
                        if expr.is_empty()
                            || expr.back().map_or(false, |back| {
                                matches!(back, ComplexToken::Operator((_, true)))
                            })
                        {
                            expr.push_back(ComplexToken::Operator((t.lexeme().as_symbol(), false)));
                            continue;
                        } else {
                            expr.push_back(ComplexToken::Operator((t.lexeme().as_symbol(), true)));
                        }

                        if !self.check_op() {
                            return Err(Diagnostic::expected_after(
                                "expression".to_owned(),
                                "binary op".to_owned(),
                                self.path.clone(),
                                t.line(),
                                t.column(),
                            ));
                        }
                    }
                    Tilde => {
                        if expr.is_empty()
                            || expr.back().map_or(false, |back| {
                                matches!(back, ComplexToken::Operator((_, true)))
                            })
                        {
                            expr.push_back(ComplexToken::Operator((t.lexeme().as_symbol(), false)));
                            continue;
                        } else {
                            expr.push_back(ComplexToken::Operator((t.lexeme().as_symbol(), true)));
                        }

                        if !self.check_op() {
                            return Err(Diagnostic::expected_after(
                                "expression".to_owned(),
                                "binary op".to_owned(),
                                self.path.clone(),
                                t.line(),
                                t.column(),
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

                        if self.check_val() {
                            break t;
                        }
                    }
                    LeftParen => {
                        let exprs = self.parse_expression(Some((RightParen, ")")))?;
                        expr.push_back(ComplexToken::Expr(exprs));
                        self.assert(TokenType::RightParen, ")")?;

                        if self.check_val() {
                            break t;
                        }

                        let index = self.parse_identifier()?;
                        expr.push_back(index);
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
            return Err(Diagnostic::expected_found(
                "expression".to_owned(),
                last.lexeme().to_string(),
                self.path.clone(),
                last.line(),
                last.column(),
            ));
        }

        self.assert_end(&last, end, expr)
    }

    fn parse_local_variable(&mut self) -> Result<ComplexToken, Diagnostic> {
        let mut names = vec![];

        self.go_back();

        loop {
            let name = self
                .assert_advance(TokenType::Identifier, "<name>")?
                .clone();
            let line = name.line();
            let column = name.column();
            let lexeme = name.lexeme();
            let peek = self.peek().clone();

            if peek.map_or(false, |t| t.kind() == TokenType::LessThan) {
                self.advance();
                let kind = self
                    .assert_advance(TokenType::Identifier, "<specifier>")?
                    .clone();
                let kind = kind.lexeme();
                self.assert(TokenType::GreaterThan, ">")?;

                if kind.as_symbol().as_ref() == "const" {
                    names.push((lexeme.as_symbol(), false));
                    eprintln!(
                        "{}",
                        Diagnostic::new(
                            "const variables are not supported in clue, ignoring const specifier"
                                .to_owned(),
                            self.path.clone(),
                            line,
                            column
                        )
                        .level(DiagnosticLevel::Warning)
                    );
                } else if kind.as_symbol().as_ref() == "close" {
                    names.push((lexeme.as_symbol(), true));
                    eprintln!(
                        "{}",
                        Diagnostic::new(
                            "to-be-closed variables are not supported in clue, ignoring const specifier. Manual close calls will be added at the end of the scope, so variable shadowing will cause side effects".to_owned(),
                            self.path.clone(),
                            line,
                            column
                        ).level(DiagnosticLevel::Warning)
                    );
                } else {
                    return Err(Diagnostic::expected_found(
                        "'const' or 'close'".to_owned(),
                        kind.to_string(),
                        self.path.clone(),
                        line,
                        column,
                    ));
                }
            } else {
                names.push((lexeme.as_symbol(), false));
            }

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

    fn parse_function_args(&mut self) -> Result<FunctionArgs, Diagnostic> {
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
                        return Err(Diagnostic::expected_found(
                            "identifier".to_owned(),
                            t.lexeme().to_string(),
                            self.path.clone(),
                            t.line(),
                            t.column(),
                        ));
                    }
                }
            };

            if let Some(t) = self.advance().cloned() {
                match t.kind() {
                    Comma => args.push(name.lexeme().as_symbol()),
                    RightParen => {
                        args.push(name.lexeme().as_symbol());
                        break;
                    }
                    _ => {
                        let path = self.path.clone();
                        return Err(Diagnostic::expected_found(
                            "',' or ')'".to_owned(),
                            t.lexeme().to_string(),
                            path,
                            t.line(),
                            t.column(),
                        ));
                    }
                }
            } else {
                return Err(Diagnostic::expected(
                    "'<args>'".to_owned(),
                    self.path.clone(),
                    self.line,
                    self.column,
                ));
            }
        }

        Ok(args)
    }

    fn parse_function(&mut self, local: bool) -> Result<ComplexToken, Diagnostic> {
        let name = {
            use TokenType::*;

            let mut expr = Expression::new();
            let path = self.path.clone();
            let line = self.line;
            let column = self.column;
            loop {
                let t = self
                    .advance()
                    .ok_or_else(|| {
                        Diagnostic::unexpected("end of file".to_owned(), path.clone(), line, column)
                    })?
                    .clone();

                match t.kind() {
                    Identifier => {
                        let nt = self.peek().ok_or_else(|| {
                            Diagnostic::unexpected(
                                "end of file".to_owned(),
                                self.path.clone(),
                                self.line,
                                self.column,
                            )
                        })?;
                        if nt.kind() == Identifier {
                            return Err(Diagnostic::unexpected(
                                nt.lexeme().to_string(),
                                self.path.clone(),
                                nt.line(),
                                nt.column(),
                            ));
                        }
                        expr.push_back(ComplexToken::Symbol(t.lexeme().as_symbol()));
                    }
                    Dot | Colon => {
                        if self
                            .peek()
                            .map_or(false, |t| t.kind() == TokenType::Identifier)
                        {
                            expr.push_back(ComplexToken::Symbol(t.lexeme().as_symbol()));
                        } else {
                            return Err(Diagnostic::new(
                                format!("{} should only be used when indexing", t.lexeme()),
                                self.path.clone(),
                                t.line(),
                                t.column(),
                            ));
                        }
                    }
                    LeftParen => break,
                    _ => {
                        return Err(Diagnostic::expected_found(
                            "identifier, ':' or '.'".to_string(),
                            t.lexeme().to_string(),
                            self.path.clone(),
                            t.line(),
                            t.column(),
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

    fn parse_if_else_chain(&mut self) -> Result<ComplexToken, Diagnostic> {
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
                return Err(Diagnostic::expected_found(
                    "'else', 'elseif' or 'end'".to_owned(),
                    self.current().lexeme().to_string(),
                    self.path.clone(),
                    self.current().line(),
                    self.current().column(),
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

    fn parse_for_loop(&mut self) -> Result<ComplexToken, Diagnostic> {
        let mut iters = vec![];
        let mut for_func = false;

        while let Some(t) = self.advance().cloned() {
            match t.kind() {
                TokenType::Identifier => {
                    iters.push(t.lexeme().as_symbol());
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
                        return Err(Diagnostic::expected_found(
                            "1 iter in numeric for loop identifier".to_owned(),
                            iters.len().to_string(),
                            self.path.clone(),
                            t.line(),
                            t.column(),
                        ));
                    }
                    break;
                }
                _ => {
                    return Err(Diagnostic::expected_found(
                        "Expected identifier, ',' or 'in'".to_owned(),
                        t.lexeme().to_string(),
                        self.path.clone(),
                        t.line(),
                        t.column(),
                    ))
                }
            }
        }

        if for_func {
            let expr = self.parse_expression(None)?;
            let stop = if self.advance_if(TokenType::Comma) {
                Some(self.parse_expression(None)?)
            } else {
                None
            };
            let initial = if self.advance_if(TokenType::Comma) {
                Some(self.parse_expression(None)?)
            } else {
                None
            };

            self.assert_advance(TokenType::Do, "do")?;

            let code = self.parse_code_block()?;

            Ok(ComplexToken::ForFuncLoop {
                iters,
                expr,
                stop,
                initial,
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

pub fn parse_tokens(tokens: &[Token], path: Option<String>) -> Result<Expression, Diagnostic> {
    let mut parser = Parser::new(tokens, path);

    while let Some(token) = parser.advance().cloned() {
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
                    return Err(Diagnostic::expected_found(
                        "function or identifier".to_owned(),
                        parser.peek().unwrap().lexeme().to_string(),
                        parser.path.clone(),
                        parser.peek().unwrap().line(),
                        parser.peek().unwrap().column(),
                    ));
                }
            },
            Identifier => {
                parser.go_back();
                let ident = parser.parse_identifier_statement()?;
                parser.expr.push_back(ident);
                parser.advance_if(TokenType::Semicolon);
            }
            LeftParen => {
                let expr = parser.parse_expression(Some((RightParen, ")")))?;
                parser.expr.push_back(ComplexToken::Expr(expr));
                parser.advance();
                let call = parser.parse_identifier()?;
                let ComplexToken::Ident {expr, ..} = &call else {unreachable!()};

                if let Some(ComplexToken::Call(_)) = expr.back() {
                    parser.expr.push_back(call);
                } else {
                    let token = parser.peek().ok_or_else(|| {
                        Diagnostic::unexpected(
                            "end of file".to_owned(),
                            parser.path.clone(),
                            parser.line,
                            parser.column,
                        )
                    })?;

                    return Err(Diagnostic::expected_found(
                        "function call".to_owned(),
                        token.lexeme().to_string(),
                        parser.path.clone(),
                        token.line(),
                        token.column(),
                    ));
                }

                parser.advance_if(TokenType::Semicolon);
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
                parser.advance_if(TokenType::Semicolon);
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
                    return Err(Diagnostic::new(
                        "Return must be the last statement in a block".to_owned(),
                        parser.path.clone(),
                        parser.line,
                        parser.column,
                    ));
                }
                parser.expr.push_back(ComplexToken::Return(exprs));
                parser.advance_if(TokenType::Semicolon);
            }
            Semicolon => {
                continue;
            }
            _ => Err(Diagnostic::unexpected(
                token.lexeme().to_string(),
                parser.path.clone(),
                token.line(),
                token.column(),
            ))?,
        }
        parser.advance_if(TokenType::Semicolon);
    }

    Ok(parser.expr)
}
