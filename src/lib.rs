//! # Monotronian
//!
//! A BASIC-alike language for Monotron. See README.md.

#![cfg_attr(not(test), no_std)]
#![cfg_attr(not(test), feature(alloc))]

#[cfg(test)]
use std as core;

#[macro_use]
extern crate nom;

#[cfg(not(test))]
extern crate alloc;
#[cfg(not(test))]
use alloc::prelude::*;

pub mod lexer;
pub mod parser;

pub struct Parser {
    _state: State,
}

#[derive(Debug)]
pub enum Error<'a> {
    Unknown,
    Lexer(lexer::Error<'a>),
    Incomplete,
}

#[derive(Debug)]
pub enum Value {
    /// Our strings are 8-bit strings in CodePage 850
    String(Vec<u8>),
    /// Our numbers are 64-bit signed
    Integer(i64),
    /// This is currently unsupported.
    Real(f64),
}

enum State {
    Init,
}

impl Parser {
    pub fn new() -> Parser {
        Parser {
            _state: State::Init,
        }
    }

    pub fn parse<'a>(
        &mut self,
        buffer: &'a [u8],
        debug: &mut core::fmt::Write,
    ) -> Result<Value, Error<'a>> {
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
                    return Err(Error::Lexer(e));
                }
            }
        }
        Ok(Value::Integer(0))
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
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
