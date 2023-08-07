use crate::{error::Diagnostic, lexer::Lexer};
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub struct HexDecimal {
    before_decimal: String,
    after_decimal: String,
}

impl HexDecimal {
    #[inline]
    fn new(before_decimal: String, after_decimal: String) -> Self {
        HexDecimal {
            before_decimal,
            after_decimal,
        }
    }

    fn to_decimal(&self) -> f64 {
        let mut num = 0.0f64;

        for (i, c) in (0i32..).zip(self.before_decimal.chars().rev()) {
            let digit = c.to_digit(16).unwrap() as f64;
            num += digit * 16f64.powi(i);
        }

        for (i, c) in (0i32..).zip(self.after_decimal.chars().skip(1)) {
            let digit = c.to_digit(16).unwrap() as f64;
            num += digit * 16f64.powi(-(i + 1));
        }

        num
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Decimal {
    before_decimal: String,
    after_decimal: String,
}

#[inline]
fn normalize_before_decimal(before_decimal: String) -> String {
    if before_decimal.is_empty() {
        "0".to_string()
    } else {
        before_decimal
    }
}

#[inline]
fn normalize_after_decimal(after_decimal: String) -> String {
    if after_decimal == "." {
        ".0".to_string()
    } else {
        after_decimal
    }
}

impl Decimal {
    #[inline]
    fn new(before_decimal: String, after_decimal: String) -> Self {
        Decimal {
            before_decimal: normalize_before_decimal(before_decimal),
            after_decimal: normalize_after_decimal(after_decimal),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Number {
    Hex(String),
    HexDecimal(HexDecimal),
    HexScientific {
        mantissa: HexDecimal,
        exponent: String,
    },
    Decimal(Decimal),
    Scientific {
        mantissa: Decimal,
        exponent: String,
    },
}

impl Number {
    pub fn from_source(lexer: &mut Lexer) -> Result<Number, Diagnostic> {
        let start = lexer.current;
        let mut digit_encountered = false;
        let mut decimal_encountered = false;
        let mut is_scientific = false;
        let mut is_hex = false;
        let mut sign_encountered = false;

        let mut before_decimal = String::with_capacity(4);
        let mut after_decimal = String::with_capacity(4);
        let mut exponent = String::new();

        while let Some(c) = lexer.advance() {
            match c {
                '0'..='9' => {
                    digit_encountered = true;
                    if is_scientific {
                        exponent.push(c);
                    } else if decimal_encountered {
                        after_decimal.push(c);
                    } else {
                        before_decimal.push(c);
                    }
                }
                '.' => {
                    if decimal_encountered {
                        return Err(Diagnostic::new(
                            "malformed number".to_owned(),
                            lexer.path.clone(),
                            lexer.position.with_span(start..lexer.current),
                        ));
                    }

                    decimal_encountered = true;
                    if is_scientific {
                        return Err(Diagnostic::new(
                            "malformed number".to_owned(),
                            lexer.path.clone(),
                            lexer.position.with_span(start..lexer.current),
                        ));
                    }

                    if is_hex
                        && !digit_encountered
                        && !lexer.peek().map_or(false, |c| c.is_ascii_hexdigit())
                    {
                        return Err(Diagnostic::new(
                            "malformed number".to_owned(),
                            lexer.path.clone(),
                            lexer.position.with_span(start..lexer.current),
                        ));
                    }

                    if !is_hex
                        && !digit_encountered
                        && !lexer.peek().map_or(false, |c| c.is_ascii_digit())
                    {
                        return Err(Diagnostic::new(
                            "malformed number".to_owned(),
                            lexer.path.clone(),
                            lexer.position.with_span(start..lexer.current),
                        ));
                    }
                    after_decimal.push(c);
                }
                'x' | 'X' => {
                    if !lexer.look_back().map_or(false, |c| c == '0')
                        || is_scientific
                        || is_hex
                        || !lexer
                            .peek()
                            .map_or(false, |c| c.is_ascii_hexdigit() || c == '.')
                    {
                        return Err(Diagnostic::new(
                            "malformed number".to_owned(),
                            lexer.path.clone(),
                            lexer.position.with_span(start..lexer.current),
                        ));
                    }
                    is_hex = true;
                    digit_encountered = false;
                }
                'e' | 'E' => {
                    if !is_scientific && is_hex {
                        continue;
                    }

                    if !digit_encountered
                        || !lexer
                            .peek()
                            .map_or(false, |c| c.is_ascii_digit() || c == '+' || c == '-')
                        || is_scientific
                    {
                        return Err(Diagnostic::new(
                            "malformed number".to_owned(),
                            lexer.path.clone(),
                            lexer.position.with_span(start..lexer.current),
                        ));
                    } else {
                        exponent.push(lexer.advance().unwrap());
                    }
                    is_scientific = true;
                }
                'p' | 'P' => {
                    if !is_hex || !digit_encountered || is_scientific {
                        return Err(Diagnostic::new(
                            "malformed number".to_owned(),
                            lexer.path.clone(),
                            lexer.position.with_span(start..lexer.current),
                        ));
                    }

                    if lexer
                        .peek()
                        .map_or(false, |c| c == '+' || c == '-' || c.is_ascii_digit())
                    {
                        exponent.push(lexer.advance().unwrap());
                    } else {
                        return Err(Diagnostic::new(
                            "malformed number".to_owned(),
                            lexer.path.clone(),
                            lexer.position.with_span(start..lexer.current),
                        ));
                    }
                    is_scientific = true;
                }
                'a'..='f' | 'A'..='F' => {
                    if !is_hex || is_scientific {
                        return Err(Diagnostic::new(
                            "malformed number".to_owned(),
                            lexer.path.clone(),
                            lexer.position.with_span(start..lexer.current),
                        ));
                    }
                    if decimal_encountered {
                        after_decimal.push(c);
                    } else {
                        before_decimal.push(c);
                    }
                    digit_encountered = true;
                }
                '+' | '-' => {
                    if !is_scientific {
                        lexer.go_back();
                        break;
                    }

                    if sign_encountered {
                        return Err(Diagnostic::new(
                            "malformed number".to_owned(),
                            lexer.path.clone(),
                            lexer.position.with_span(start..lexer.current),
                        ));
                    }
                    sign_encountered = true;
                    exponent.push(c);
                }
                _ => {
                    lexer.go_back();
                    break;
                }
            }
        }

        if is_scientific
            && !(lexer.source[lexer.current - 1].is_ascii_digit()
                || lexer.peek().map_or(false, |c| c.is_ascii_digit()))
        {
            return Err(Diagnostic::new(
                "malformed number".to_owned(),
                lexer.path.clone(),
                lexer.position.with_span(start..lexer.current),
            ));
        }

        if !is_hex && !digit_encountered {
            return Err(Diagnostic::new(
                "malformed number".to_owned(),
                lexer.path.clone(),
                lexer.position.with_span(start..lexer.current),
            ));
        }

        let len = lexer.current - start;
        let lexeme: String = lexer.source[lexer.current - len..lexer.current]
            .iter()
            .collect();

        let number = if is_scientific {
            if is_hex {
                let mantissa = HexDecimal::new(before_decimal, after_decimal);
                Number::HexScientific { mantissa, exponent }
            } else {
                let mantissa = Decimal::new(before_decimal, after_decimal);
                Number::Scientific { mantissa, exponent }
            }
        } else if is_hex {
            if decimal_encountered {
                let hex_decimal = HexDecimal::new(before_decimal[1..].to_string(), after_decimal);
                Number::HexDecimal(hex_decimal)
            } else {
                Number::Hex(lexeme)
            }
        } else if decimal_encountered {
            let decimal = Decimal::new(before_decimal, after_decimal);
            Number::Decimal(decimal)
        } else {
            Number::Decimal(Decimal::new(before_decimal, after_decimal))
        };
        Ok(number)
    }
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Number::Hex(hex) => write!(f, "{}", hex),
            Number::HexDecimal(hex) => {
                let number = hex.to_decimal().to_string();
                if number == "inf" {
                    write!(f, "1 / 0")
                } else if number == "-inf" {
                    write!(f, "-1 / 0")
                } else {
                    write!(f, "{}", number)
                }
            }
            Number::HexScientific { mantissa, exponent } => {
                let number = mantissa.to_decimal();
                let number = number * 2f64.powi(exponent.parse::<i32>().unwrap());
                let number = number.to_string();

                if number == "inf" {
                    write!(f, "1 / 0")
                } else if number == "-inf" {
                    write!(f, "-1 / 0")
                } else {
                    write!(f, "{}", number)
                }
            }
            Number::Decimal(Decimal {
                before_decimal,
                after_decimal,
            }) => {
                write!(f, "{}{}", before_decimal, after_decimal)
            }
            Number::Scientific { mantissa, exponent } => {
                write!(
                    f,
                    "{}{}e{}",
                    mantissa.before_decimal, mantissa.after_decimal, exponent
                )
            }
        }
    }
}
