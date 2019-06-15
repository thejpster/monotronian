//! # Monotronian
//!
//! A BASIC-alike language for Monotron. See README.md.

#![cfg_attr(not(test), no_std)]

#[macro_use]
extern crate nom;

#[macro_use]
extern crate num_derive;

use heapless::{consts::*, Vec};

pub mod byte_code;
pub mod lexer;

pub struct Parser<'a> {
    operator_stack: Vec<lexer::Token<'a>, U8>,
    postfix: Vec<lexer::Token<'a>, U8>,
}

#[derive(Debug)]
pub enum Error<'a> {
    Unknown,
    Lexer(lexer::Error<'a>),
    UnexpectedToken(lexer::Token<'a>),
    Incomplete,
}

trait Peekable<T> {
    fn peek(&self) -> Option<T>;
}

impl<T, S> Peekable<T> for Vec<T, S>
where
    S: heapless::ArrayLength<T>,
    T: Clone,
{
    fn peek(&self) -> Option<T> {
        if self.len() == 0 {
            None
        } else {
            Some(self[self.len() - 1].clone())
        }
    }
}

impl<'a> Parser<'a> {
    pub fn new() -> Parser<'a> {
        Parser {
            operator_stack: Vec::new(),
            postfix: Vec::new(),
        }
    }

    pub fn parse(&mut self, buffer: &'a [u8]) -> Result<i64, Error<'a>> {
        use lexer::{Lexer, Token};
        let mut buffer = buffer;
        loop {
            // lex the input and use the shunting-yard algorithm to convert it
            // to reverse polish notation
            let token = Lexer::lex_tokens(&buffer);
            match token {
                Ok((Token::EOF, _)) => {
                    break;
                }
                Ok((Token::Operator(op), remainder)) => {
                    // Check top of operator stack - pop off lower then push on
                    while let Some(Token::Operator(top_op)) = self.operator_stack.peek() {
                        if op > top_op {
                            self.operator_stack.pop().unwrap();
                            self.postfix.push(Token::Operator(top_op)).unwrap();
                        } else {
                            break;
                        }
                    }
                    self.operator_stack.push(Token::Operator(op)).unwrap();
                    buffer = remainder;
                }
                Ok((Token::DecimalIntLiteral(number), remainder)) => {
                    // Copy number to output array
                    self.postfix.push(Token::DecimalIntLiteral(number)).unwrap();
                    buffer = remainder;
                }
                Ok((Token::LeftRoundBracket, remainder)) => {
                    self.operator_stack.push(Token::LeftRoundBracket).unwrap();
                    buffer = remainder;
                }
                Ok((Token::RightRoundBracket, remainder)) => {
                    while let Some(op) = self.operator_stack.pop() {
                        match op {
                            Token::LeftRoundBracket => {
                                break;
                            }
                            other => {
                                self.postfix.push(other).unwrap();
                            }
                        }
                    }
                    buffer = remainder;
                }
                Ok((Token::NewLine, _remainder)) => {
                    // End of line
                    break;
                }
                Ok((token, _remainder)) => {
                    // Bad input
                    return Err(Error::UnexpectedToken(token));
                }
                Err(e) => {
                    return Err(Error::Lexer(e));
                }
            }
        }
        while let Some(op) = self.operator_stack.pop() {
            self.postfix.push(op).unwrap();
        }
        // Now evaluate the RPN
        self.evaluate()
    }

    pub fn evaluate(&mut self) -> Result<i64, Error<'a>> {
        use lexer::{Operator, Token};
        match self.postfix.pop().unwrap() {
            Token::DecimalIntLiteral(u) => {
                return Ok(u);
            }
            Token::Operator(Operator::Plus) => {
                let right = self.evaluate()?;
                let left = self.evaluate()?;
                return Ok(left + right);
            }
            Token::Operator(Operator::Minus) => {
                let right = self.evaluate()?;
                let left = self.evaluate()?;
                return Ok(left - right);
            }
            Token::Operator(Operator::Star) => {
                let right = self.evaluate()?;
                let left = self.evaluate()?;
                return Ok(left * right);
            }
            Token::Operator(Operator::Slash) => {
                let right = self.evaluate()?;
                let left = self.evaluate()?;
                return Ok(left / right);
            }
            t => {
                panic!("Unexpected {:?}", t);
            }
        }
    }
}

/// Display an ASCII string. We use CodePage 850, but `write!` expects Unicode
/// so we limit ourselves to the visible ASCII subset.
pub(crate) fn display_ascii_string(
    buf: &[u8],
    fmt: &mut ::core::fmt::Formatter,
) -> ::core::fmt::Result {
    for ch in buf.iter() {
        match *ch {
            0x20...0x7f => write!(fmt, "{}", *ch as char).unwrap(),
            b'\n' => write!(fmt, "\\n").unwrap(),
            _ => write!(fmt, "?").unwrap(),
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_maths() {
        let input = b"1 + 2";
        let mut p = Parser::new();
        assert_eq!(p.parse(input).unwrap(), 3);
    }

    #[test]
    fn two_operations() {
        let input = b"1 + 2 * 3";
        let mut p = Parser::new();
        assert_eq!(p.parse(input).unwrap(), 7);
    }

    #[test]
    fn brackets() {
        let input = b"(1 + 2) * 3";
        let mut p = Parser::new();
        assert_eq!(p.parse(input).unwrap(), 9);
    }
}
