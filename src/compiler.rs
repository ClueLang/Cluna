use crate::{
    parser::{CodeBlock, ComplexToken, ComplexToken::*, Expression, FunctionArgs},
    ENV_DEBUGCOMMENTS,
};
use std::iter::{Iterator, Peekable};

fn indentate(scope: usize) -> String {
    let mut result = String::new();
    for _ in 0..scope {
        result += "\t";
    }
    result
}

fn indentate_if<T: Iterator>(ctokens: &mut Peekable<T>, scope: usize) -> String {
    match ctokens.peek() {
        Some(_) => format!("\n{}", indentate(scope)),
        None => String::new(),
    }
}

fn compile_list<T>(
    list: Vec<T>,
    separator: &str,
    tostring: &mut impl FnMut(T) -> String,
) -> String {
    let mut result = String::new();
    let end = list.iter().count();
    let mut start = 0usize;
    for element in list {
        result += &(tostring(element));
        start += 1;
        if start < end {
            result += separator
        }
    }
    result
}

fn compile_identifiers(names: Vec<String>) -> String {
    compile_list(names, ", ", &mut |name| name)
}

fn compile_expressions(
    scope: usize,
    names: Option<&Vec<String>>,
    values: Vec<Expression>,
) -> String {
    compile_list(values, ", ", &mut |expr| {
        compile_expression(scope, names, expr)
    })
}

fn compile_function(
    scope: usize,
    names: Option<&Vec<String>>,
    args: FunctionArgs,
    code: CodeBlock,
) -> (String, String) {
    let mut code = compile_code_block(scope, "", code);
    let args = compile_list(args, ", ", &mut |(arg, default)| {
        if let Some((default, _)) = default {
            let default = compile_expression(scope + 2, names, default);
            let pre = indentate(scope + 1);
            code = format!(
                "\n{}if {} == nil then\n{}\t{} = {}\n{}}}{}",
                pre, arg, pre, arg, default, pre, code
            )
        }
        arg
    });
    (code, args)
}

fn compile_code_block(scope: usize, start: &str, block: CodeBlock) -> String {
    let code = compile_tokens(scope + 1, block.code);
    let pre = indentate(scope);
    if arg!(ENV_DEBUGCOMMENTS) {
        format!(
            "{}\n{}\t--{}->{}\n{}\n{}",
            start, pre, block.start, block.end, code, pre
        )
    } else {
        format!("{}\n{}\n{}", start, code, pre)
    }
}

fn compile_identifier(scope: usize, names: Option<&Vec<String>>, expr: Expression) -> String {
    let mut result = String::new();
    let mut checked = String::new();
    let mut iter = expr.into_iter().peekable();
    while let Some(t) = iter.next() {
        match t.clone() {
            SYMBOL(lexeme) => {
                let lexeme = lexeme.as_str();
                match lexeme {
                    "?." => {
                        result += &(checked.clone() + " and ");
                        checked += ".";
                    }
                    "?::" => {
                        result += &(checked.clone() + " and ");
                        checked += ":";
                    }
                    "?[" => {
                        result += &(checked.clone() + " and ");
                        let texpr = iter.next();
                        let rexpr = if let Some(EXPR(expr)) = texpr {
                            compile_expression(scope, names, expr.clone())
                        } else {
                            panic!("This message should never appear");
                        };
                        checked += &format!("[({})]", rexpr);
                    }
                    "]" => {}
                    _ => checked += lexeme,
                }
            }
            EXPR(expr) => {
                let expr = compile_expression(scope, names, expr);
                checked += &format!("({})]", expr);
            }
            CALL(args) => checked += &format!("({})", compile_expressions(scope, names, args)),
            _ => {}
        }
    }
    if result.is_empty() {
        result + &checked
    } else {
        format!("({})", result + &checked)
    }
}

fn compile_expression(mut scope: usize, names: Option<&Vec<String>>, expr: Expression) -> String {
    let mut result = String::new();
    for t in expr {
        result += &match t {
            SYMBOL(lexeme) => lexeme,
            TABLE { values, metas } => {
                scope += 1;
                let mut prevline = 0usize;
                let pre1 = indentate(scope);
                let values = if values.is_empty() {
                    String::new()
                } else {
                    compile_list(values, ", ", &mut |(name, value, line)| {
                        let value = compile_expression(scope, names, value);

                        prevline = line;
                        if let Some(name) = name {
                            let name = compile_expression(scope, names, name);
                            format!("\n{}{} = {}", pre1, name, value)
                        } else {
                            format!("\n{}{}", pre1, value)
                        }
                    }) + "\n"
                };
                prevline = 0;
                let pre2 = indentate(scope - 1);
                if metas.is_empty() {
                    scope -= 1;

                    format!("{{{}{}}}", values, pre2)
                } else {
                    let metas = compile_list(metas, ", ", &mut |(name, value, line)| {
                        let value = compile_expression(scope, names, value);

                        prevline = line;
                        format!("\n{}{} = {}", pre1, name, value)
                    });
                    scope -= 1;
                    format!(
                        "setmetatable({{{}{}}}, {{{}\n{}}})",
                        values, pre2, metas, pre2
                    )
                }
            }
            LAMBDA { args, code } => {
                let (code, args) = compile_function(scope, names, args, code);
                format!("function({}){}end", args, code)
            }
            IDENT { expr, .. } => compile_identifier(scope, names, expr),
            CALL(args) => format!("({})", compile_expressions(scope, names, args)),
            EXPR(expr) => format!("({})", compile_expression(scope, names, expr)),
            _ => {
                panic!("Unexpected ComplexToken found")
            }
        }
    }
    result
}

fn compile_else_if_chain(
    scope: usize,
    condition: Expression,
    code: CodeBlock,
    next: Option<Box<ComplexToken>>,
) -> String {
    let condition = compile_expression(scope, None, condition);
    let code = compile_code_block(scope, "{", code);
    let next = if let Some(next) = next {
        String::from("else")
            + &match *next {
                IF_STATEMENT {
                    condition,
                    code,
                    next,
                } => compile_else_if_chain(scope, condition, code, next),
                ELSE_BLOCK(code) => compile_code_block(scope, "{", code),
                _ => {
                    panic!("Unexpected ComplexToken found")
                }
            }
    } else {
        String::new()
    };
    format!("if ({}) {}}}{}", condition, code, next)
}

pub fn compile_tokens(scope: usize, ctokens: Expression) -> String {
    let mut result = String::new();
    let ctokens = &mut ctokens.into_iter().peekable();
    while let Some(t) = ctokens.next() {
        result += &match t {
            VARIABLE {
                local,
                names,
                values,
                line: _,
            } => {
                let end = indentate_if(ctokens, scope);
                let pre = if local { "local " } else { "" };
                if local && values.is_empty() {
                    format!("{}{}{}", pre, compile_identifiers(names), end)
                } else {
                    let values = compile_expressions(scope, Some(&names), values);
                    let names = compile_identifiers(names);
                    format!("{}{} = {}{}", pre, names, values, end)
                }
            }
            ALTER {
                names,
                values,
                line: _,
            } => {
                let iter = names.into_iter();
                let mut names: Vec<String> = Vec::new();
                for name in iter {
                    names.push(compile_expression(scope, None, name))
                }
                let values = compile_list(values, ", ", &mut |expr| {
                    compile_expression(scope, Some(&names), expr)
                });
                let names = compile_identifiers(names);

                format!("{} = {}{}", names, values, indentate_if(ctokens, scope))
            }
            FUNCTION {
                local,
                name,
                args,
                code,
            } => {
                let pre = if local { "local " } else { "" };
                let end = indentate_if(ctokens, scope);
                let name = compile_expression(scope, None, name);
                let (code, args) = compile_function(scope, None, args, code);
                format!("{}fn {}({}){{{}}}{}", pre, name, args, code, end)
            }
            LAMBDA { args, code } => {
                let (code, args) = compile_function(scope, None, args, code);
                format!("fn({}){{{}}}", args, code)
            }
            IF_STATEMENT {
                condition,
                code,
                next,
            } => {
                let code = compile_else_if_chain(scope, condition, code, next);
                format!("{}}}{}", code, indentate_if(ctokens, scope))
            }
            WHILE_LOOP { condition, code } => {
                let condition = compile_expression(scope, None, condition);
                let code = compile_code_block(scope, "{", code);
                format!(
                    "while {} {}}}{}",
                    condition,
                    code,
                    indentate_if(ctokens, scope)
                )
            }
            REPEAT_UNTIL { condition, code } => {
                let condition = compile_expression(scope, None, condition);
                let code = compile_code_block(scope, "", code);
                format!(
                    "loop {{{}}} until {}{}",
                    code,
                    condition,
                    indentate_if(ctokens, scope)
                )
            }
            FOR_LOOP {
                iterator,
                start,
                end,
                alter,
                code,
            } => {
                let start = compile_expression(scope, None, start);
                let endexpr = compile_expression(scope, None, end);
                let alter = compile_expression(scope, None, alter);
                let code = compile_code_block(scope, "", code);
                let end = indentate_if(ctokens, scope);
                format!(
                    "for {} = {}, {}, {} {{{}}}{}",
                    iterator, start, endexpr, alter, code, end
                )
            }
            FOR_FUNC_LOOP {
                iterators,
                expr,
                code,
            } => {
                let expr = compile_expression(scope, Some(&iterators), expr);
                let iterators = compile_identifiers(iterators);
                let code = compile_code_block(scope, "", code);
                format!(
                    "for {} with {}{{{}}}{}",
                    iterators,
                    expr,
                    code,
                    indentate_if(ctokens, scope)
                )
            }
            IDENT { expr, line: _ } => {
                let expr = compile_identifier(scope, None, expr);
                format!("{};{}", expr, indentate_if(ctokens, scope))
            }
            SYMBOL(lexeme) => lexeme,
            CALL(args) => {
                format!(
                    "({}){}",
                    compile_expressions(scope, None, args),
                    indentate_if(ctokens, scope)
                )
            }
            EXPR(expr) => format!("({})", compile_expression(scope, None, expr)),
            DO_BLOCK(code) => format!(
                "{}}}{}",
                compile_code_block(scope, "{", code),
                indentate_if(ctokens, scope)
            ),
            RETURN_EXPR(exprs) => {
                if let Some(exprs) = exprs {
                    format!("return {}", compile_expressions(scope, None, exprs))
                } else {
                    String::from("return")
                }
            }
            BREAK_LOOP => String::from("break") + &indentate_if(ctokens, scope),
            _ => {
                panic!("Unexpected ComplexToken {:?} found", t)
            }
        }
    }
    result
}
