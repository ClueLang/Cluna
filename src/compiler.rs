use std::iter::Peekable;

use crate::parser::{CodeBlock, ComplexToken, Expression};

fn indent(scope: usize) -> String {
    let mut result = String::new();
    for _ in 0..scope {
        result += "    ";
    }
    result
}

fn indent_if<T: Iterator>(ctokens: &mut Peekable<T>, scope: usize) -> String {
    match ctokens.peek() {
        Some(_) => String::from('\n') + &indent(scope),
        None => String::with_capacity(4),
    }
}

fn compile_multiline_string(string: String) -> String {
    let mut start = 1;
    let mut equals_count = 0;
    let chars = string.chars().skip(1);

    for c in chars {
        start += 1;
        if c == '=' {
            equals_count += 1;
        } else {
            break;
        }
    }

    let end = string.len() - equals_count - 2;

    String::from('`') + &string[start..end] + "`"
}

fn compile_list<T>(
    list: Vec<T>,
    separator: &str,
    tostring: &mut impl FnMut(T) -> String,
) -> String {
    let mut result = String::new();
    let end = list.len().saturating_sub(1);

    for (i, element) in list.into_iter().enumerate() {
        result += &(tostring(element));

        if i != end {
            result += separator
        }
    }
    result
}
fn compile_expressions(scope: usize, exprs: Vec<Expression>) -> String {
    compile_list(exprs, ", ", &mut |expr| compile_expression(scope, expr))
}

fn is_binop(lexeme: &str) -> bool {
    matches!(
        lexeme,
        "+" | "-" | "*" | "/" | "%" | "^" | "==" | "<" | ">" | "<=" | ">=" | "~=" | ".."
    )
}

fn compile_symbol(lexeme: String) -> String {
    match &*lexeme {
        "and" => " && ".to_owned(),
        "or" => " || ".to_owned(),
        "not" => "!".to_owned(),
        "!=" => " ~= ".to_owned(),
        ":" => "::".to_owned(),
        s if is_binop(s) => format!(" {} ", s),
        _ => lexeme,
    }
}

fn compile_identifier(scope: usize, ident: ComplexToken) -> String {
    use crate::parser::ComplexToken::*;

    let mut result = String::new();
    let Ident {expr, ..} = ident else {unreachable!()};

    for ctoken in expr {
        match ctoken {
            Symbol(lexeme) => result += &compile_symbol(lexeme),
            Expr(expr) => {
                result.push('(');
                result += &compile_expression(scope, expr);
                result.push(')');
            }
            Call(args) => {
                result.push('(');
                result += &compile_expressions(scope, args);
                result.push(')');
            }
            _ => unreachable!(),
        }
    }

    result
}

fn compile_code_block(body: CodeBlock, scope: usize) -> String {
    let code = compile_ast_helper(body.code, scope + 1);

    String::from('\n') + &code + "\n" + &indent(scope)
}

fn compile_if_else_chain(
    scope: usize,
    condition: Expression,
    code: CodeBlock,
    next: Option<Box<ComplexToken>>,
) -> String {
    use crate::parser::ComplexToken::*;
    let mut result = String::new();

    let condition = compile_expression(scope, condition);
    let body = compile_code_block(code, scope);

    let next = if let Some(next) = next {
        String::from(" else")
            + &match *next {
                IfStatement {
                    condition,
                    body,
                    next,
                    ..
                } => {
                    if condition.is_empty() {
                        format!(" {{{}}}", compile_code_block(body, scope))
                    } else {
                        format!("if {}", compile_if_else_chain(scope, condition, body, next))
                    }
                }
                _ => unreachable!(),
            }
    } else {
        String::new()
    };

    result += "if ";
    result += &condition;
    result += " {";
    result += &body;
    result.push('}');
    result += &next;

    result
}

fn compile_expression(mut scope: usize, expr: Expression) -> String {
    use crate::parser::ComplexToken::*;

    let mut result = String::new();

    for ctoken in expr {
        match ctoken {
            Symbol(lexeme) => result += &compile_symbol(lexeme),
            MultilineString(string) => result += &compile_multiline_string(string),
            Table { data, .. } => {
                scope += 1;
                let pre = indent(scope);
                result.push('{');
                if !data.is_empty() {
                    result += &compile_list(data, ", ", &mut |(key, value)| {
                        if let Some(key) = key {
                            format!(
                                "\n{}{} = {}",
                                pre,
                                compile_expression(scope, key),
                                compile_expression(scope, value),
                            )
                        } else {
                            String::from('\n') + &pre + &compile_expression(scope, value)
                        }
                    });
                    result.push('\n');
                    result += &indent(scope - 1);
                }
                result.push('}');
            }
            Lambda { args, body, .. } => {
                result += "fn ";
                result.push('(');
                result += &compile_list(args, ", ", &mut |arg| arg);
                result += ") ";
                result += "{";
                result += &compile_code_block(body, scope);
                result.push('}');
            }
            ident @ Ident { .. } => {
                result += &compile_identifier(scope, ident);
            }
            Call(args) => {
                result.push('(');
                result += &compile_expressions(scope, args);
                result.push(')');
            }
            Expr(expr) => {
                result.push('(');
                result += &compile_expression(scope, expr);
                result.push(')');
            }
            _ => unreachable!(),
        }
    }

    result
}

fn compile_ast_helper(tree: Expression, scope: usize) -> String {
    use crate::parser::ComplexToken::*;

    let mut result = indent(scope);
    let tree = &mut tree.into_iter().peekable();

    while let Some(ctoken) = tree.next() {
        match ctoken {
            Variable { names, values, .. } => {
                result += "local ";
                result += &compile_list(names, ", ", &mut |name| name);
                if !values.is_empty() {
                    result += " = ";
                    result += &compile_expressions(scope, values);
                }
                result += &indent_if(tree, scope);
            }
            Alter { names, values, .. } => {
                result += &compile_list(names, ", ", &mut |name| compile_identifier(scope, name));
                result += " = ";
                result += &compile_expressions(scope, values);
                result += &indent_if(tree, scope);
            }
            Function {
                local,
                name,
                args,
                body,
                ..
            } => {
                let end = indent_if(tree, scope);

                if name.len() == 1 {
                    if local {
                        result += "local ";
                    } else {
                        result += "global ";
                    }
                    result += "fn ";
                } else {
                    result += "method ";
                }

                result += &compile_expression(scope, name);
                result.push('(');
                result += &compile_list(args, ", ", &mut |arg| arg);
                result += ") ";
                result += "{";
                result += &compile_code_block(body, scope);
                result.push('}');
                result += &end;
            }
            Lambda { args, body, .. } => {
                result += "fn ";
                result.push('(');
                result += &compile_list(args, ", ", &mut |arg| arg);
                result += ") ";
                result += "{";
                result += &compile_code_block(body, scope);
                result.push('}');
            }
            IfStatement {
                condition,
                body,
                next,
                ..
            } => {
                let code = compile_if_else_chain(scope, condition, body, next);
                result += &code;
                result += &indent_if(tree, scope);
            }
            WhileLoop {
                condition, body, ..
            } => {
                let condition = compile_expression(scope, condition);
                let body = compile_code_block(body, scope);

                result += "while ";
                result += &condition;
                result += " {";
                result += &body;
                result.push('}');
                result += &indent_if(tree, scope);
            }
            ForLoop {
                iter,
                start,
                end,
                step,
                code,
                ..
            } => {
                result += "for ";
                result += &iter;
                result += " = ";
                result += &compile_expression(scope, start);
                result += ", ";
                result += &compile_expression(scope, end);
                if let Some(step) = step {
                    result += ", ";
                    result += &compile_expression(scope, step);
                }
                result += " {";
                result += &compile_code_block(code, scope);
                result.push('}');
                result += &indent_if(tree, scope);
            }
            ForFuncLoop {
                iters,
                expr,
                stop,
                initial,
                code,
                ..
            } => {
                if stop.is_some() || initial.is_some() {
                    let s = scope;
                    let iters_compiled = &compile_list(iters.clone(), ", ", &mut |iter| iter);

                    result += "{\n";
                    let scope = scope + 1;
                    result += &indent(scope);
                    result += &format!(
                        "local _internal_expr_{0}, _internal_stop_{0}, _internal_acc_{0} = ",
                        s
                    );
                    result += &compile_expression(scope, expr);
                    result += ", ";
                    result += &compile_expression(scope, stop.unwrap());
                    result += ", ";
                    result += &initial.map_or("nil".to_owned(), |initial| {
                        compile_expression(scope, initial)
                    });
                    result += ";\n";
                    result += &indent(scope);
                    result += "while true {\n";
                    let scope = scope + 1;
                    result += &indent(scope);
                    result += "local ";
                    result += iters_compiled;
                    result += " = ";
                    result += &format!(
                        "_internal_expr_{0}(_internal_stop_{0}, _internal_acc_{0});\n",
                        s
                    );
                    result += &indent(scope);
                    result += &format!("_internal_acc_{s} = ");
                    result += &iters[0];
                    result += ";\n";
                    result += &indent(scope);
                    result += "if ";
                    result += &format!("_internal_acc_{s}");
                    result += " == nil";
                    result += " { break; }";

                    let scope = scope - 1;
                    result += &compile_code_block(code, scope);
                    result += "}\n";
                    result += &indent(scope - 1);
                    result += "}\n";
                } else {
                    result += "for ";
                    result += &compile_list(iters, ", ", &mut |iter| iter);
                    result += " with ";
                    result += &compile_expression(scope, expr);
                    result += " {";
                    result += &compile_code_block(code, scope);
                    result.push('}');
                    result += &indent_if(tree, scope);
                }
            }
            RepeatLoop {
                condition, body, ..
            } => {
                let condition = compile_expression(scope, condition);
                let body = compile_code_block(body, scope);

                result += "loop ";
                result += " {";
                result += &body;
                result.push('}');
                result += " until ";
                result += &condition;
                result += &indent_if(tree, scope);
            }
            ctoken @ Ident { .. } => {
                result += &compile_identifier(scope, ctoken);
                result.push(';');
                result += &indent_if(tree, scope);
            }
            Symbol(lexeme) => result += &compile_symbol(lexeme),
            MultilineString(string) => result += &compile_multiline_string(string),
            Call(args) => {
                result.push('(');
                result += &compile_expressions(scope, args);
                result.push(')');
            }
            Expr(expr) => {
                result.push('(');
                result += &compile_expression(scope, expr);
                result.push(')');
            }
            DoBlock(body) => {
                result += "{";
                result += &compile_code_block(body, scope);
                result += "}";
                result += &indent_if(tree, scope);
            }
            Return(exprs) => {
                result += "return ";
                if let Some(exprs) = exprs {
                    result +=
                        &compile_list(exprs, ", ", &mut |expr| compile_expression(scope, expr));
                }
            }
            Break => {
                result += "break";
                result += &indent_if(tree, scope);
            }

            _ => unreachable!(),
        }
    }

    result
}

pub fn compile_ast(tree: Expression) -> String {
    compile_ast_helper(tree, 0)
}
