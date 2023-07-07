use crate::lexer::Lexer;

#[derive(Debug)]
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

    fn into_decimal(self) -> f64 {
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

#[derive(Debug)]
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

#[derive(Debug)]
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
    pub fn from_source(lexer: &mut Lexer) -> Result<Number, String> {
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
                        return Err(format!(
                            "Error: Malformed number at {}:{}",
                            lexer.line, lexer.column
                        ));
                    }

                    decimal_encountered = true;
                    if is_scientific {
                        return Err(format!(
                            "Error: Malformed number at {}:{}",
                            lexer.line, lexer.column
                        ));
                    }

                    if is_hex
                        && !digit_encountered
                        && !lexer.peek().map_or(false, |c| c.is_ascii_hexdigit())
                    {
                        return Err(format!(
                            "Error: Malformed number at {}:{}",
                            lexer.line, lexer.column
                        ));
                    }

                    if !is_hex
                        && !digit_encountered
                        && !lexer.peek().map_or(false, |c| c.is_ascii_digit())
                    {
                        return Err(format!(
                            "Error: Malformed number at {}:{}",
                            lexer.line, lexer.column
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
                        return Err(format!(
                            "Error: Malformed number at {}:{}",
                            lexer.line, lexer.column
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
                        return Err(format!(
                            "Error: Malformed number at {}:{}",
                            lexer.line, lexer.column
                        ));
                    } else {
                        exponent.push(lexer.advance().unwrap());
                    }
                    is_scientific = true;
                }
                'p' | 'P' => {
                    if !is_hex || !digit_encountered || is_scientific {
                        return Err(format!(
                            "Error: Malformed number at {}:{}",
                            lexer.line, lexer.column
                        ));
                    }

                    if lexer
                        .peek()
                        .map_or(false, |c| c == '+' || c == '-' || c.is_ascii_digit())
                    {
                        exponent.push(lexer.advance().unwrap());
                    } else {
                        return Err(format!(
                            "Error: Malformed number at {}:{}",
                            lexer.line, lexer.column
                        ));
                    }
                    is_scientific = true;
                }
                'a'..='f' | 'A'..='F' => {
                    if !is_hex || is_scientific {
                        return Err(format!(
                            "Error: Malformed number at {}:{}",
                            lexer.line, lexer.column
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
                        return Err(format!(
                            "Error: Malformed number at {}:{}",
                            lexer.line, lexer.column
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
            return Err(format!(
                "Error: Malformed number at {}:{}",
                lexer.line, lexer.column
            ));
        }

        if !is_hex && !digit_encountered {
            return Err(format!(
                "Error: Malformed number at {}:{}",
                lexer.line, lexer.column
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

    pub fn into_clue_number(self) -> String {
        match self {
            Number::Hex(hex) => hex,
            Number::HexDecimal(hex) => {
                let number = hex.into_decimal().to_string();
                if number == "inf" {
                    "1 / 0".to_owned()
                } else if number == "-inf" {
                    "-1 / 0".to_owned()
                } else {
                    number
                }
            }
            Number::HexScientific { mantissa, exponent } => {
                let number = mantissa.into_decimal();
                let number = number * 2f64.powi(exponent.parse::<i32>().unwrap());
                let number = number.to_string();

                if number == "inf" {
                    "1 / 0".to_owned()
                } else if number == "-inf" {
                    "-1 / 0".to_owned()
                } else {
                    number
                }
            }
            Number::Decimal(Decimal {
                before_decimal,
                after_decimal,
            }) => {
                let mut s = String::with_capacity(4);

                s.push_str(&before_decimal);
                s.push_str(&after_decimal);
                s
            }
            Number::Scientific { mantissa, exponent } => {
                let mut s = String::with_capacity(4);

                s.push_str(&mantissa.before_decimal);
                s.push_str(&mantissa.after_decimal);
                s.push('e');
                s.push_str(&exponent);
                s
            }
        }
    }
}
