//! # Monotronian Byte Code
//!
//! This module defines the Monotronian byte-code format. Unlike say, WASM or
//! LLVM IR, this byte-code is designed to be losslessly converted back to the
//! original source code (or at least as close as possible).

use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

#[repr(u8)]
#[derive(Debug, Copy, Clone, FromPrimitive, PartialEq, Eq)]
enum Instruction {
    Add,
    Assign,
    BAnd,
    Boolean,
    BOr,
    Break,
    Call,
    Different,
    Divide,
    ElIf,
    Else,
    EndFn,
    EndIf,
    EndWhile,
    Equals,
    False,
    Fn,
    GetVal,
    GetValIdx,
    Gt,
    Gte,
    If,
    LAnd,
    LiteralArray,
    LiteralChar,
    LiteralFloat,
    LiteralInteger,
    LiteralNil,
    LiteralString,
    LOr,
    Lt,
    Lte,
    Modulo,
    Negate,
    Next,
    Nil,
    Not,
    Return,
    Subtract,
    Times,
    True,
    While,
}

pub struct Program<'a> {
    code: &'a [u8],
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Value<'a> {
    StringLiteral(&'a [u8]),
    Integer(i32),
    Boolean(bool),
    Nil,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Error {
    FunctionNotFound,
    UnrecognisedToken,
    OutOfBounds(usize),
}

impl<'a> Program<'a> {
    pub fn new(code: &'a [u8]) -> Program {
        Program { code }
    }

    fn read_instruction(&self, offset: usize) -> Result<Instruction, Error> {
        FromPrimitive::from_u8(
            *self
                .code
                .get(offset)
                .ok_or_else(|| Error::OutOfBounds(offset))?,
        )
        .ok_or(Error::UnrecognisedToken)
    }

    fn skip_instruction(&self, instruction_offset: usize) -> Result<usize, Error> {
        let instruction = self.read_instruction(instruction_offset)?;
        self.decoded_skip_instruction(instruction, instruction_offset + 1)
    }

    fn decoded_skip_instruction(
        &self,
        instruction: Instruction,
        mut arg_offset: usize,
    ) -> Result<usize, Error> {
        match instruction {
            // Instructions with two arguments
            Instruction::Add
            | Instruction::BAnd
            | Instruction::BOr
            | Instruction::LAnd
            | Instruction::LOr
            | Instruction::Equals
            | Instruction::Different
            | Instruction::Gt
            | Instruction::Gte
            | Instruction::Lte
            | Instruction::Lt
            | Instruction::Divide
            | Instruction::Modulo
            | Instruction::Subtract
            | Instruction::Times => {
                arg_offset = self.skip_instruction(arg_offset)?;
                arg_offset = self.skip_instruction(arg_offset)?;
            }
            // An index and then an array of instructions
            Instruction::LiteralArray => {
                let num_args = self.code[arg_offset] as usize;
                arg_offset += 1;
                for _ in 0..num_args {
                    arg_offset = self.skip_instruction(arg_offset)?;
                }
            }
            // An l-value string name, and an instruction
            Instruction::Assign => {
                // variable name string, but without the LiteralString token because it's implicit
                arg_offset =
                    self.decoded_skip_instruction(Instruction::LiteralString, arg_offset)?;
                // an expression
                arg_offset = self.skip_instruction(arg_offset)?;
            }
            Instruction::Boolean => {
                arg_offset = self.skip_instruction(arg_offset)?;
            }
            Instruction::Break
            | Instruction::ElIf
            | Instruction::Else
            | Instruction::EndFn
            | Instruction::EndIf
            | Instruction::EndWhile
            | Instruction::False
            | Instruction::LiteralNil
            | Instruction::True
            | Instruction::Next
            | Instruction::Nil => {
                // No args
            }
            // Call a function
            Instruction::Call => {
                // function name string, but without the LiteralString token because it's implicit
                arg_offset =
                    self.decoded_skip_instruction(Instruction::LiteralString, arg_offset)?;
                // list of expressions (which is the same as a LiteralArray)
                arg_offset =
                    self.decoded_skip_instruction(Instruction::LiteralArray, arg_offset)?;
            }
            // Define a function
            Instruction::Fn => {
                // function name string, but without the LiteralString token because it's implicit
                arg_offset =
                    self.decoded_skip_instruction(Instruction::LiteralString, arg_offset)?;
                // a list of named parameters
                let num_params = self.code[arg_offset] as usize;
                arg_offset += 1;
                for _ in 0..num_params {
                    // parameter name string, but without the LiteralString token because it's implicit
                    arg_offset =
                        self.decoded_skip_instruction(Instruction::LiteralString, arg_offset)?;
                }
            }
            // Read a variable
            Instruction::GetVal => {
                // function name string, but without the LiteralString token because it's implicit
                arg_offset =
                    self.decoded_skip_instruction(Instruction::LiteralString, arg_offset)?;
            }
            // Read a variable at a given index
            Instruction::GetValIdx => {
                // variable name string, but without the LiteralString token because it's implicit
                arg_offset =
                    self.decoded_skip_instruction(Instruction::LiteralString, arg_offset)?;
                // Index
                arg_offset += 1;
            }
            // If statement
            Instruction::If => {
                // An expression
                arg_offset = self.skip_instruction(arg_offset)?;
            }
            // An i32 literal integer
            Instruction::LiteralInteger => {
                arg_offset += 4;
            }
            // A literal integer
            Instruction::LiteralString => {
                // String length
                let length = self.code[arg_offset] as usize;
                arg_offset += 1;
                // Array of bytes
                arg_offset += length;
            }
            // A literal 8-bit char
            Instruction::LiteralChar => {
                arg_offset += 1;
            }
            // A literal f32 float
            Instruction::LiteralFloat => {
                arg_offset += 4;
            }
            // Unary operators
            Instruction::Not | Instruction::Negate => {
                // An expression
                arg_offset = self.skip_instruction(arg_offset)?;
            }
            // Return an expression
            Instruction::Return => {
                // An expression
                arg_offset = self.skip_instruction(arg_offset)?;
            }
            // Start a while loop
            Instruction::While => {
                // An expression
                arg_offset = self.skip_instruction(arg_offset)?;
            }
        }
        Ok(arg_offset)
    }

    fn read_string(&self, offset: usize) -> &'a [u8] {
        let string_len = self.code[offset] as usize;
        &self.code[offset + 1..offset + 1 + string_len]
    }

    fn find_function(&self, search_name: &[u8]) -> Result<usize, Error> {
        let mut offset = 0;
        loop {
            match self.read_instruction(offset) {
                Ok(Instruction::Fn) => {
                    let fun_name = self.read_string(offset + 1);
                    if fun_name == search_name {
                        return Ok(offset);
                    } else {
                        offset = self.decoded_skip_instruction(Instruction::Fn, offset + 1)?;
                    }
                }
                Ok(i) => offset = self.decoded_skip_instruction(i, offset + 1)?,
                Err(Error::OutOfBounds(_)) => {
                    return Err(Error::FunctionNotFound);
                }
                Err(e) => {
                    return Err(e);
                }
            }
        }
    }

    pub fn run_function(&mut self, name: &[u8]) -> Result<Value<'a>, Error> {
        let mut offset = self.find_function(name)?;
        loop {
            match self.read_instruction(offset) {
                Ok(Instruction::Return) => {
                    return Ok(Value::Nil);
                }
                Ok(i) => {
                    offset = self.decoded_skip_instruction(i, offset + 1)?;
                }
                Err(e) => {
                    return Err(e);
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn empty_function() {
        let byte_code = [
            Instruction::Fn as u8,
            0x04,
            b'm',
            b'a',
            b'i',
            b'n',
            0x01,
            0x04,
            b'a',
            b'r',
            b'g',
            b's',
            Instruction::Return as u8,
            Instruction::Nil as u8,
        ];
        let mut p = Program::new(&byte_code);
        assert_eq!(p.run_function(b"main"), Ok(Value::Nil));
    }

    #[test]
    fn function_not_found() {
        let byte_code = [
            Instruction::Fn as u8,
            0x04,
            b'm',
            b'a',
            b'i',
            b'n',
            0x01,
            0x04,
            b'a',
            b'r',
            b'g',
            b's',
            Instruction::Return as u8,
            Instruction::Nil as u8,
        ];
        let mut p = Program::new(&byte_code);
        assert_eq!(p.run_function(b"mainX"), Err(Error::FunctionNotFound));
    }

    #[test]
    fn two_functions() {
        let byte_code = [
            Instruction::Fn as u8,
            0x04,
            b'm',
            b'a',
            b'i',
            b'n',
            0x01,
            0x04,
            b'a',
            b'r',
            b'g',
            b's',
            Instruction::Return as u8,
            Instruction::Nil as u8,
            Instruction::Fn as u8,
            0x03,
            b'f',
            b'o',
            b'o',
            0x01,
            0x04,
            b'a',
            b'r',
            b'g',
            b's',
            Instruction::Return as u8,
            Instruction::Nil as u8,
        ];
        let mut p = Program::new(&byte_code);
        assert_eq!(p.run_function(b"foo"), Ok(Value::Nil));
    }
}
