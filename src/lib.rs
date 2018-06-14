//! # Lexer/Interpreter for Monotronian
//!
//! The lexer is loosely based on monkey-rust.
//! The syntax is loosely based on monkey (and it's a bit C like).
//!
//! Grammar:
//!
//! function := fn `name`(arguments) { expression }
//! arguments := nil | identifier arguments
//! expression := if | while | for | 

#![cfg_attr(feature="no_std", no_std)]

#![cfg(not(feature="no_std"))]
#![feature(alloc)]

extern crate alloc;

pub mod lexer;
pub mod parser;

#[cfg(not(feature="no_std"))]
use alloc::String;

#[cfg(not(feature="no_std"))]
use std as core;

pub struct Parser {
    _state: State,
}

#[derive(Debug)]
pub enum Error<'a> {
    Unknown,
    Lexer(lexer::Error, &'a str),
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
            _state: State::Init,
        }
    }

    pub fn parse<'a>(&mut self, buffer: &'a str, debug: &mut core::fmt::Write) -> Result<Value, Error<'a>> {
        let mut buffer = buffer;
        loop {
            // Step 1. lex(buffer) -> tokens
            // Need to feed the tokens into some sort of token buffer
            // so we can create an AST. We then store the AST and execute it later.
            match lexer::Lexer::lex_tokens(&buffer) {
                Ok((lexer::Token::EOF, _)) => {
                    break;
                }
                Ok((token, remainder)) => {
                    buffer = remainder;
                    writeln!(debug, "Got {:?}", token).unwrap();
                }
                Err(e) => {
                    return Err(Error::Lexer(e, buffer));
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
