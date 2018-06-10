//! # Lexer/Interpreter for Monotronian
//!
//! The lexer is based on monkey-rust.

#![no_std]
#![feature(alloc)]

#[macro_use]
extern crate alloc;

// #[macro_use]
// extern crate nom;

pub mod lexer;

use alloc::{String, Vec};

pub struct Parser {
    state: State,
}

#[derive(Debug)]
pub enum Error {
    Unknown,
    Lexer(lexer::Error),
    Incomplete
}

#[derive(Debug)]
pub enum Value {
    String(String),
    Integer(i64),
    Real(f64),
}

enum State {
    Init
}

impl Parser {
    pub fn new() -> Parser {
        Parser {
            state: State::Init,
        }
    }

    pub fn parse(&mut self, buffer: &str, debug: &mut core::fmt::Write) -> Result<Value, Error> {
        let mut buffer = buffer;
        loop {
            // Step 1. lex(buffer) -> Vec<Tokens>
            match lexer::Lexer::lex_tokens(&buffer) {
                Ok((lexer::Token::EOF, _remainder)) => {
                    break;
                }
                Ok((token, remainder)) => {
                    buffer = remainder;
                    writeln!(debug, "Got {:?}", token).unwrap();
                }
                Err(e) => {
                    return Err(Error::Lexer(e));
                }
            }
        }
        Ok(Value::Integer(0))
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
