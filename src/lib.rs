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

#![cfg_attr(feature = "no_std", no_std)]
#![cfg(not(feature = "no_std"))]
#![feature(alloc)]

#[macro_use]
extern crate nom;
extern crate alloc;

pub mod lexer;
pub mod parser;

#[cfg(not(feature = "no_std"))]
use std as core;

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
pub(crate) fn display_ascii_string(buf: &[u8], fmt: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
    for ch in buf.iter() {
        match *ch {
            0x20 ... 0x7f => write!(fmt, "{}", *ch as char).unwrap(),
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
