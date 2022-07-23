#![allow(non_camel_case_types)]

use self::ComplexToken::*;
use crate::{
	// compiler::CompileTokens,
	scanner::{Token, TokenType, TokenType::*},
	BlockType::*,
	ConditionType::*,
};
use std::{cmp, collections::LinkedList};

macro_rules! expression {
	($($x: expr),*) => {
		{
			let mut expr = Expression::new();
			$(expr.push_back($x);)*
			expr
		}
	};
}

pub type Expression = LinkedList<ComplexToken>;
pub type FunctionArgs = Vec<(String, Option<(Expression, usize)>)>;
type OptionalEnd = Option<(TokenType, &'static str)>;
type MatchCase = (Vec<Expression>, Option<Expression>, CodeBlock);

#[derive(Debug, Clone, PartialEq)]
pub enum ComplexToken {
	VARIABLE {
		local: bool,
		names: Vec<String>,
		values: Vec<Expression>,
		line: usize,
	},

	ALTER {
		names: Vec<Expression>,
		values: Vec<Expression>,
		line: usize,
	},

	TABLE {
		values: Vec<(Option<Expression>, Expression, usize)>,
		metas: Vec<(String, Expression, usize)>,
	},

	FUNCTION {
		local: bool,
		name: Expression,
		args: FunctionArgs,
		code: CodeBlock,
	},

	LAMBDA {
		args: FunctionArgs,
		code: CodeBlock,
	},

	IF_STATEMENT {
		condition: Expression,
		code: CodeBlock,
		next: Option<Box<ComplexToken>>,
	},

	WHILE_LOOP {
		condition: Expression,
		code: CodeBlock,
	},

	LOOP_UNTIL {
		condition: Expression,
		code: CodeBlock,
	},

	FOR_LOOP {
		iterator: String,
		start: Expression,
		end: Expression,
		alter: Expression,
		code: CodeBlock,
	},

	FOR_FUNC_LOOP {
		iterators: Vec<String>,
		expr: Expression,
		code: CodeBlock,
	},

	IDENT {
		expr: Expression,
		line: usize,
	},

	SYMBOL(String),
	PSEUDO(usize),
	CALL(Vec<Expression>),
	EXPR(Expression),
	DO_BLOCK(CodeBlock),
	ELSE_BLOCK(CodeBlock),
	RETURN_EXPR(Option<Vec<Expression>>),
	CONTINUE_LOOP,
	BREAK_LOOP,
}

#[derive(Clone, Debug, PartialEq)]
pub struct CodeBlock {
	pub start: usize,
	pub code: Expression,
	pub end: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub enum BlockType {
	THEN_TYPE,
	DO_TYPE,
	REPEAT_TYPE,
	NONE_TYPE,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ConditionType {
	IF_TYPE,
	ELSE_TYPE,
}

#[derive(Debug)]
struct ParserInfo {
	current: usize,
	size: usize,
	tokens: Vec<Token>,
	filename: String,
	expr: Expression,
	testing: Option<usize>,
	localid: u8,
}

impl ParserInfo {
	fn new(tokens: Vec<Token>, filename: String) -> ParserInfo {
		ParserInfo {
			current: 0,
			size: tokens.len() - 1,
			tokens,
			filename,
			expr: Expression::new(),
			testing: None,
			localid: 0,
		}
	}

	fn test<T>(&mut self, func: impl FnOnce(&mut ParserInfo) -> T) -> (T, usize) {
		let start = self.current - 1;
		self.testing = Some(0);
		let result = func(self);
		self.testing = match self.testing {
			Some(line) if line > 0 => self.testing,
			_ => None,
		};
		let reached = self.current;
		self.current = start;
		(result, reached)
	}

	fn error(&mut self, msg: impl Into<String>, line: usize) -> String {
		if let Some(0) = self.testing {
			self.testing = Some(line);
		} else {
			println!("Error in file \"{}\" at line {}!", self.filename, line);
		}
		msg.into()
	}

	fn expected(&mut self, expected: &str, got: &str, line: usize) -> String {
		self.error(format!("Expected '{}', got '{}'", expected, got), line)
	}

	fn expected_before(&mut self, expected: &str, before: &str, line: usize) -> String {
		self.error(format!("Expected '{}' before '{}'", expected, before), line)
	}

	fn unexpected(&mut self, str: &str, line: usize) -> String {
		self.error(format!("Unexpected token '{}'", str), line)
	}

	fn ended(&self) -> bool {
		self.current >= self.size
	}

	fn at(&self, pos: usize) -> Token {
		self.tokens[cmp::min(pos, self.size)].to_owned()
	}

	fn advance(&mut self) -> Token {
		self.current += 1;
		self.look_back(0)
	}
	fn peek(&self, pos: usize) -> Token {
		let pos: usize = self.current + pos;
		self.at(pos)
	}

	fn look_back(&self, pos: usize) -> Token {
		let pos: usize = self.current - pos - 1;
		self.at(pos)
	}

	fn compare(&self, expected: TokenType) -> bool {
		if self.ended() {
			return false;
		}
		if self.peek(0).kind != expected {
			return false;
		}
		true
	}

	fn advance_if(&mut self, expected: TokenType) -> bool {
		if self.ended() {
			return false;
		}
		if self.peek(0).kind != expected {
			return false;
		}
		self.current += 1;
		true
	}

	fn assert_advance(&mut self, expected: TokenType, error: &str) -> Result<Token, String> {
		let t = self.advance();
		if t.kind != expected {
			return Err(self.expected(error, &t.lexeme, t.line));
		}
		Ok(t)
	}

	fn assert_compare(&mut self, expected: TokenType, error: &str) -> Result<(), String> {
		if !self.compare(expected) {
			let t = self.peek(0);
			return Err(self.expected(error, &t.lexeme, t.line));
		}
		Ok(())
	}

	fn assert_end<T>(&mut self, tocheck: &Token, end: OptionalEnd, iftrue: T) -> Result<T, String> {
		if let Some((kind, lexeme)) = end {
			if tocheck.kind != kind {
				return Err(self.expected(lexeme, &tocheck.lexeme, tocheck.line));
			}
		}
		Ok(iftrue)
	}

	fn assert(&mut self, expected: TokenType, error: &str) -> Result<(), String> {
		if !self.advance_if(expected) {
			let t = self.peek(0);
			return Err(self.expected(error, &t.lexeme, t.line));
		}
		Ok(())
	}

	fn build_call(&mut self) -> Result<ComplexToken, String> {
		self.current += 2;
		let args: Vec<Expression> = if self.advance_if(ROUND_BRACKET_CLOSED) {
			Vec::new()
		} else {
			self.find_expressions(COMMA, Some((ROUND_BRACKET_CLOSED, ")")))?
		};
		Ok(CALL(args))
	}

	fn find_expressions(
		&mut self,
		separator: TokenType,
		end: OptionalEnd,
	) -> Result<Vec<Expression>, String> {
		let mut exprs: Vec<Expression> = Vec::new();
		loop {
			let expr = self.build_expression(None)?;
			let t = self.look_back(0);
			exprs.push(expr);
			if t.kind != separator {
				return self.assert_end(&t, end, exprs);
			}
		}
	}

	fn build_table(&mut self) -> Result<ComplexToken, String> {
		let mut values: Vec<(Option<Expression>, Expression, usize)> = Vec::new();
		let mut metas: Vec<(String, Expression, usize)> = Vec::new();
		loop {
			if self.advance_if(CURLY_BRACKET_CLOSED) {
				break;
			}
			let start = self.current;
			let mut qscope = 1u8;
			let mut iskey = false;
			while match self.peek(0).kind {
				CURLY_BRACKET_OPEN => {
					qscope += 1;
					true
				}
				CURLY_BRACKET_CLOSED => {
					qscope -= 1;
					qscope != 0
				}
				COMMA => qscope != 1,
				DEFINE => {
					iskey = true;
					false
				}
				EOF => return Err(self.expected_before("}", "<end>", self.peek(0).line)),
				_ => true,
			} {
				self.current += 1;
			}
			self.current = start;
			if !iskey {
				values.push((None, self.build_expression(None)?, self.at(start).line));
				self.current -= 1;
				self.advance_if(COMMA);
				continue;
			}
			let name: Result<Expression, String>;
			let pn = self.advance();
			match pn.kind {
				IDENTIFIER => {
					name = Ok(expression![SYMBOL(pn.lexeme.clone())]);
				}
				SQUARE_BRACKET_OPEN => {
					let mut qscope = 1u8;
					let start = self.current;
					while match self.advance().kind {
						SQUARE_BRACKET_OPEN => {
							qscope += 1;
							true
						}
						SQUARE_BRACKET_CLOSED => {
							qscope -= 1;
							match qscope {
								0 => false,
								_ => true,
							}
						}
						EOF => return Err(self.expected_before("]", "<end>", self.peek(0).line)),
						_ => true,
					} {}
					self.current = start;
					name = Ok(self.build_name()?);
					self.current -= 1;
				}
				_ => return Err(self.expected("<name>", &pn.lexeme, pn.line)),
			}
			if !self.advance_if(DEFINE) {
				let t = self.peek(0);
				return Err(self.expected("=", &t.lexeme, t.line));
			}
			let start = self.current;
			let mut cscope = 0u8;
			while match self.peek(0).kind {
				COMMA | CURLY_BRACKET_CLOSED => {
					if cscope == 0 {
						false
					} else {
						true
					}
				}
				ROUND_BRACKET_OPEN => {
					cscope += 1;
					true
				}
				ROUND_BRACKET_CLOSED => {
					if cscope == 0 {
						return Err(self.expected_before("(", ")", self.peek(0).line));
					}
					cscope -= 1;
					true
				}
				EOF => return Err(self.expected_before("}", "<end>", self.peek(0).line)),
				_ => true,
			} {
				self.current += 1;
			}
			self.current = start;
			match name {
				Ok(n) => values.push((Some(n), self.build_expression(None)?, pn.line)),
				Err(n) => metas.push((n, self.build_expression(None)?, pn.line)),
			}
			self.current -= 1;
			self.advance_if(COMMA);
		}
		Ok(TABLE { values, metas })
	}

	fn check_operator(&mut self, t: &Token, checkback: bool) -> Result<(), String> {
		if match self.peek(0).kind {
			NUMBER | IDENTIFIER | STRING | DOLLAR | TRUE | FALSE | MINUS | NIL | NOT | HASHTAG
			| ROUND_BRACKET_OPEN | THREEDOTS | CURLY_BRACKET_OPEN => false,
			_ => true,
		} {
			return Err(self.error(
				format!("Operator '{}' has invalid right hand token", t.lexeme),
				t.line,
			));
		}
		if checkback
			&& match self.look_back(1).kind {
				NUMBER
				| IDENTIFIER
				| STRING
				| DOLLAR
				| TRUE
				| FALSE
				| NIL
				| ROUND_BRACKET_CLOSED
				| SQUARE_BRACKET_CLOSED
				| THREEDOTS => false,
				_ => true,
			}
		{
			return Err(self.error(
				format!("Operator '{}' has invalid left hand token", t.lexeme),
				t.line,
			));
		}
		Ok(())
	}

	fn check_index(
		&mut self,
		t: &Token,
		expr: &mut Expression,
		lexeme: &str,
	) -> Result<(), String> {
		if !self.compare(IDENTIFIER)
			|| match self.look_back(0).kind {
				IDENTIFIER | SQUARE_BRACKET_CLOSED => true,
				_ => false,
			}
		{
			return Err(self.error(
				format!("'{}' should be used only when indexing", t.lexeme),
				self.peek(0).line,
			));
		}
		expr.push_back(SYMBOL(lexeme.to_string()));
		Ok(())
	}

	fn check_val(&mut self) -> bool {
		match self.peek(0).kind {
			NUMBER | IDENTIFIER | STRING | DOLLAR | TRUE | FALSE | NIL | NOT | HASHTAG
			| CURLY_BRACKET_OPEN | THREEDOTS => {
				self.current += 1;
				true
			}
			_ => false,
		}
	}

	fn build_expression(&mut self, end: OptionalEnd) -> Result<Expression, String> {
		let mut expr = Expression::new();
		let last = loop {
			let t = self.advance();

			match t.kind {
				IDENTIFIER => {
					let fname = self.build_identifier()?;
					self.current -= 1;
					expr.push_back(fname);
					if self.check_val() {
						break t;
					}
				}
				CURLY_BRACKET_OPEN => {
					if let Some((kind, ..)) = end {
						if kind == CURLY_BRACKET_OPEN {
							break t;
						}
					}
					expr.push_back(self.build_table()?);
					if self.check_val() {
						break t;
					}
				}
				PLUS | STAR | SLASH | PERCENTUAL | CARET | TWODOTS | EQUAL | BIGGER
				| BIGGER_EQUAL | SMALLER | SMALLER_EQUAL => {
					self.check_operator(&t, true)?;
					expr.push_back(SYMBOL(t.lexeme))
				}
				MINUS => {
					self.check_operator(&t, false)?;
					expr.push_back(SYMBOL(if self.look_back(1).kind == MINUS {
						format!(" {}", t.lexeme)
					} else {
						t.lexeme
					}))
				}
				NOT_EQUAL => {
					self.check_operator(&t, true)?;
					expr.push_back(SYMBOL(String::from("~=")))
				}
				HASHTAG => {
					if match self.peek(0).kind {
						IDENTIFIER | CURLY_BRACKET_OPEN | ROUND_BRACKET_OPEN => false,
						_ => true,
					} {
						let t = self.peek(0);
						return Err(self.expected("<table>", &t.lexeme, t.line));
					}
					expr.push_back(SYMBOL(String::from("#")))
				}
				/*PROTECTED_GET => {
					self.assert(ROUND_BRACKET_OPEN, "(")?;
					self.current += 1;
					expr.push_back(PGET(self.buildIdentifier(true)?));
				}*/
				AND => {
					self.check_operator(&t, true)?;
					expr.push_back(SYMBOL(String::from(" or ")))
				}
				OR => {
					self.check_operator(&t, true)?;
					expr.push_back(SYMBOL(String::from(" and ")))
				}
				NOT => {
					self.check_operator(&t, false)?;
					expr.push_back(SYMBOL(String::from("not ")))
				}
				THREEDOTS | NUMBER | TRUE | FALSE | NIL => {
					expr.push_back(SYMBOL(t.lexeme.clone()));
					if self.check_val() {
						break t;
					}
				}
				STRING => {
					expr.push_back(SYMBOL(format!("\"{}\"", t.lexeme)));
					if self.check_val() {
						break t;
					}
				}
				ROUND_BRACKET_OPEN => {
					expr.push_back(EXPR(
						self.build_expression(Some((ROUND_BRACKET_CLOSED, ")")))?,
					));
					self.current += 1;
					let fname = self.build_identifier()?;
					expr.push_back(fname);
					self.current -= 1;
				}
				FN => {
					if self.advance_if(IDENTIFIER) {
						expr.push_back(self.build_function(false)?)
					} else {
						let args: FunctionArgs = if self.advance_if(ROUND_BRACKET_OPEN)
							&& !self.advance_if(ROUND_BRACKET_CLOSED)
						{
							self.build_function_args()?
						} else {
							Vec::new()
						};
						let code = self.build_code_block(NONE_TYPE)?;
						expr.push_back(LAMBDA { args, code });
						if self.check_val() {
							break t;
						}
					}
				}
				SEMICOLON => {
					self.current += 1;
					break t;
				}
				UNTIL => {
					expr.push_back(EXPR(self.build_expression(None)?));
					break t;
				}
				_ => break t,
			}
		};

		if expr.len() == 0 {
			return Err(self.expected("<expr>", &last.lexeme, last.line));
		}
		self.assert_end(&self.look_back(0), end, expr)
	}

	fn build_name(&mut self) -> Result<Expression, String> {
		let mut expr = Expression::new();
		self.current -= 1;
		loop {
			let t = self.advance();
			match t.kind {
				IDENTIFIER => {
					expr.push_back(SYMBOL(t.lexeme));
					if self.check_val() {
						break;
					}
				}
				DOT => self.check_index(&t, &mut expr, ".")?,
				COLON => return Err(self.error("You can't call functions here", t.line)),
				SQUARE_BRACKET_OPEN => {
					let qexpr = self.build_expression(Some((SQUARE_BRACKET_CLOSED, "]")))?;
					expr.push_back(SYMBOL(String::from("[")));
					expr.push_back(EXPR(qexpr));
					expr.push_back(SYMBOL(String::from("]")));
				}
				ROUND_BRACKET_OPEN => {
					return Err(self.error("You can't call functions here", t.line))
				}
				_ => break,
			}
		}
		Ok(expr)
	}

	fn build_identifier(&mut self) -> Result<ComplexToken, String> {
		let mut expr = Expression::new();
		let line = self.look_back(0).line;
		self.current -= 1;
		loop {
			let t = self.advance();
			match t.kind {
				IDENTIFIER => {
					expr.push_back(SYMBOL(t.lexeme));
					if self.check_val() {
						break;
					}
				}
				DOT => self.check_index(&t, &mut expr, ".")?,
				COLON => {
					self.check_index(&t, &mut expr, ":")?;
					if self.peek(1).kind != ROUND_BRACKET_OPEN {
						let t = self.peek(1);
						return Err(self.expected("(", &t.lexeme, t.line));
					}
				}
				SQUARE_BRACKET_OPEN => {
					let qexpr = self.build_expression(Some((SQUARE_BRACKET_CLOSED, "]")))?;
					expr.push_back(SYMBOL(String::from("[")));
					expr.push_back(EXPR(qexpr));
					expr.push_back(SYMBOL(String::from("]")));
					if self.check_val() {
						break;
					}
				}
				ROUND_BRACKET_OPEN => {
					self.current -= 2;
					expr.push_back(self.build_call()?);
					if self.check_val() {
						break;
					}
				}
				_ => break,
			}
		}
		Ok(IDENT { expr, line })
	}

	fn build_code_block(&mut self, blockType: BlockType) -> Result<CodeBlock, String> {
		let blockTypeValue = match blockType {
			THEN_TYPE => "then",
			DO_TYPE => "do",
			NONE_TYPE => "",
			REPEAT_TYPE => "",
		};
		let tokenType = match blockType {
			THEN_TYPE => THEN,
			DO_TYPE => DO,
			NONE_TYPE | REPEAT_TYPE => NONE_BLOCK,
		};
		let start = {
			let t = self.advance();
			if t.kind != tokenType && tokenType != NONE_BLOCK {
				self.current -= 2;
				self.assert_advance(tokenType, blockTypeValue)?.line
			} else if t.kind != tokenType && tokenType == NONE_BLOCK {
				self.current -= 1;
				t.line
			} else {
				t.line
			}
		};
		let mut tokens: Vec<Token> = Vec::new();
		let mut cscope = 1u8;
		let end: usize;
		loop {
			let t = self.advance();

			match t.kind {
				THEN | DO => cscope += 1,
				REPEAT => {
					cscope += 1;
				}
				NONE_BLOCK => {
					cscope += 1;
				}
				END => {
					cscope -= 1;

					if cscope == 0 {
						end = t.line;
						break;
					}
				}
				UNTIL => {
					if blockType == REPEAT_TYPE {
						cscope -= 1;

						if cscope == 0 {
							end = t.line;
							break;
						}
					} else {
						return Err(self.unexpected("until", t.line));
					}
				}
				ELSE | ELSEIF => {
					let t = self.look_back(0);
					if blockType != THEN_TYPE {
						continue;
					}

					cscope -= 1;

					if cscope == 0 {
						end = t.line;
						break;
					}
				}
				EOF => return Err(self.expected_before("end", "<end>", t.line)),
				_ => {
					// return Err(self.unexpected(&t.lexeme, t.line));
				}
			}
			tokens.push(t);
		}
		let code = if tokens.is_empty() {
			Expression::new()
		} else {
			tokens.push(self.tokens.last().unwrap().clone());
			parse_tokens(tokens, self.filename.clone())?
		};
		Ok(CodeBlock { start, code, end })
	}

	fn build_loop_block(&mut self) -> Result<CodeBlock, String> {
		let code = self.build_code_block(DO_TYPE)?;
		Ok(code)
	}

	fn build_identifier_list(&mut self) -> Result<Vec<String>, String> {
		let mut idents: Vec<String> = Vec::new();
		while {
			let t = self.assert_advance(IDENTIFIER, "<name>")?;
			idents.push(t.lexeme);
			self.advance_if(COMMA)
		} {}
		Ok(idents)
	}

	fn build_function_args(&mut self) -> Result<FunctionArgs, String> {
		let mut args = FunctionArgs::new();
		while {
			let name = {
				let t = self.advance();
				match t.kind {
					IDENTIFIER => t,
					THREEDOTS => {
						self.assert_compare(ROUND_BRACKET_CLOSED, ")")?;
						t
					}
					_ => return Err(self.expected("<name>", &t.lexeme, t.line)),
				}
			};
			let t = self.advance();
			match t.kind {
				COMMA => {
					args.push((name.lexeme, None));
					true
				}
				DEFINE => {
					let default = self.build_expression(None)?;
					args.push((name.lexeme, Some((default, name.line))));
					let notended = self.peek(0).kind != THEN;
					if notended {
						match self.look_back(0).kind {
							COMMA => {}
							ROUND_BRACKET_CLOSED => self.current -= 1,
							_ => {
								let t = self.peek(0);
								return Err(self.expected(")", &t.lexeme, t.line));
							}
						}
					}
					notended
				}
				ROUND_BRACKET_CLOSED => {
					args.push((name.lexeme, None));
					false
				}
				_ => return Err(self.expected(")", &t.lexeme, t.line)),
			}
		} {}
		Ok(args)
	}

	fn build_else_if_chain(
		&mut self,
		conditionType: ConditionType,
	) -> Result<ComplexToken, String> {
		let condition = self.build_expression(Some((THEN, "then")))?;
		let blockType = match conditionType {
			IF_TYPE => THEN_TYPE,
			_ => NONE_TYPE,
		};
		let code = self.build_code_block(blockType)?;
		Ok(IF_STATEMENT {
			condition,
			code,
			next: {
				let t = self.look_back(0);
				let result = match t.kind {
					ELSEIF => Some(Box::new(self.build_else_if_chain(IF_TYPE)?)),
					ELSE => Some(Box::new(ELSE_BLOCK(self.build_code_block(NONE_TYPE)?))),
					_ => {
						self.current -= 1;
						None
					}
				};

				result
			},
		})
	}

	fn build_function(&mut self, local: bool) -> Result<ComplexToken, String> {
		self.current += 1;
		let name = expression![SYMBOL(self.assert_advance(IDENTIFIER, "<name>")?.lexeme)];
		self.assert(ROUND_BRACKET_OPEN, "(")?;
		let args = if !self.advance_if(ROUND_BRACKET_CLOSED) {
			self.build_function_args()?
		} else {
			FunctionArgs::new()
		};
		let code = self.build_code_block(NONE_TYPE)?;
		Ok(ComplexToken::FUNCTION {
			local,
			name,
			args,
			code,
		})
	}

	fn build_variables(&mut self, local: bool, line: usize) -> Result<ComplexToken, String> {
		let mut names: Vec<String> = Vec::new();
		loop {
			let pname = self.assert_advance(IDENTIFIER, "<name>")?;
			names.push(pname.lexeme);
			if !self.compare(COMMA) {
				self.advance_if(SEMICOLON);
				break;
			}
			self.current += 1;
		}
		let check = self.advance();
		let areinit = check.kind == DEFINE;
		let values: Vec<Expression> = if !areinit {
			Vec::new()
		} else {
			self.find_expressions(COMMA, None)?
		};
		self.current -= 1;
		Ok(VARIABLE {
			local,
			names,
			values,
			line,
		})
	}
}
pub fn parse_tokens(tokens: Vec<Token>, filename: String) -> Result<Expression, String> {
	let mut i = ParserInfo::new(tokens, filename);
	while !i.ended() {
		let t = i.advance();

		match t.kind {
			LOCAL => {
				let local = t.kind == LOCAL;
				let value: ComplexToken;
				if i.peek(0).kind == FN && local {
					value = i.build_function(true)?;
				} else {
					value = i.build_variables(local, t.line)?;
				}
				i.expr.push_back(value);
			}
			METHOD => {
				let name = {
					let mut expr = Expression::new();
					loop {
						let t = i.advance();
						match t.kind {
							IDENTIFIER => {
								let nt = i.peek(0);
								if nt.kind == IDENTIFIER {
									return Err(i.unexpected(&nt.lexeme, nt.line));
								}
								expr.push_back(SYMBOL(t.lexeme))
							}
							DOT => i.check_index(&t, &mut expr, ".")?,
							COLON => {
								let t = i.peek(1);
								if t.kind != ROUND_BRACKET_OPEN {
									return Err(i.expected("(", &t.lexeme, t.line));
								}
							}
							ROUND_BRACKET_OPEN => break,
							_ => return Err(i.expected("(", &t.lexeme, t.line)),
						}
					}
					expr
				};
				let args: FunctionArgs = if !i.advance_if(ROUND_BRACKET_CLOSED) {
					i.build_function_args()?
				} else {
					Vec::new()
				};
				let code = i.build_code_block(DO_TYPE)?;
				i.expr.push_back(ComplexToken::FUNCTION {
					local: false,
					name,
					args,
					code,
				});
			}
			IDENTIFIER => {
				let testexpr = i.test(|i| i.build_name()).0;
				if let Err(msg) = testexpr {
					match msg.as_str() {
						"You can't call functions here" => {
							let expr = &mut i.build_expression(None)?;
							i.expr.append(expr);
							i.current -= 1;
							continue;
						}
						_ => return Err(i.error(msg, i.testing.unwrap())),
					}
				}
				i.current += 1;
				let mut names: Vec<Expression> = Vec::new();
				while {
					names.push(i.build_name()?);
					i.current += 1;
					i.look_back(1).kind == COMMA
				} {}
				i.current -= 1;
				let checkt = i.look_back(0);
				let check = checkt.kind.clone() as u8;
				if check != DEFINE as u8 {
					return Err(i.expected("=", &checkt.lexeme, checkt.line));
				}
				let values: Vec<Expression> = i.find_expressions(COMMA, None)?;
				i.expr.push_back(ALTER {
					line: t.line,
					names,
					values,
				});
				i.current -= 1;
			}
			ROUND_BRACKET_OPEN => {
				let expr = i.build_expression(Some((ROUND_BRACKET_CLOSED, ")")))?;
				i.expr.push_back(EXPR(expr));
				i.current += 1;
				let call = i.build_identifier()?;
				i.expr.push_back(call);
				i.current += 1;
				i.advance_if(SEMICOLON);
			}
			DO => {
				i.current -= 1;
				let block = i.build_code_block(DO_TYPE)?;
				i.expr.push_back(DO_BLOCK(block));
			}
			IF => {
				let ctoken = i.build_else_if_chain(IF_TYPE)?;
				i.expr.push_back(ctoken);
			}
			WHILE => {
				let condition = i.build_expression(Some((DO, "do")))?;
				let code = i.build_loop_block()?;
				i.expr.push_back(WHILE_LOOP { condition, code })
			}
			REPEAT => {
				let code = i.build_code_block(REPEAT_TYPE)?;

				let condition = i.build_expression(None)?;
				i.expr.push_back(LOOP_UNTIL { condition, code })
			}
			FOR => {
				if i.peek(1).kind == DEFINE {
					let iterator = i.assert_advance(IDENTIFIER, "<name>")?.lexeme;
					i.current += 1;
					let start = i.build_expression(Some((COMMA, ",")))?;
					let end = i.build_expression(None)?;
					i.current -= 1;
					let t = i.advance();
					let alter = match t.kind {
						COMMA => i.build_expression(Some((DO, "do")))?,

						DO => {
							i.current -= 1;
							expression![SYMBOL(String::from("1"))]
						}

						_ => return Err(i.expected(",", &t.lexeme, t.line)),
					};
					let code = i.build_loop_block()?;
					i.expr.push_back(FOR_LOOP {
						iterator,
						start,
						end,
						alter,
						code,
					})
				} else {
					let iterators = i.build_identifier_list()?;
					i.advance();
					let expr = i.build_expression(Some((DO, "do")))?;
					let code = i.build_loop_block()?;
					i.expr.push_back(FOR_FUNC_LOOP {
						iterators,
						expr,
						code,
					});
				}
			}
			BREAK => {
				i.expr.push_back(BREAK_LOOP);
				i.advance_if(SEMICOLON);
			}
			RETURN => {
				let expr = if i.advance_if(SEMICOLON) {
					None
				} else {
					Some(i.find_expressions(COMMA, None)?)
				};
				i.expr.push_back(RETURN_EXPR(expr));
			}
			EOF => break,
			END => break,
			_ => return Err(i.expected("<end>", &t.lexeme, t.line)),
		}
	}
	Ok(i.expr)
}
