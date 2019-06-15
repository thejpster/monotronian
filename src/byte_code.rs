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

#[derive(Debug, Copy, Clone)]
pub enum Value<'a> {
    StringLiteral(&'a [u8]),
    Integer(i32),
    Boolean(bool),
    Nil,
}

#[derive(Debug, Copy, Clone)]
pub enum Error {
    FunctionNotFound,
    UnrecognisedToken,
}

impl<'a> Program<'a> {
    pub fn new(code: &'a [u8]) -> Program {
        Program { code }
    }

    fn read_instruction(&self, offset: usize) -> Result<Instruction, Error> {
        let instruction = match FromPrimitive::from_u8(self.code[offset]) {
            Some(Instruction::Add) => Instruction::Add,
            Some(Instruction::Assign) => Instruction::Assign,
            Some(Instruction::BAnd) => Instruction::BAnd,
            Some(Instruction::Boolean) => Instruction::Boolean,
            Some(Instruction::BOr) => Instruction::BOr,
            Some(Instruction::Break) => Instruction::Break,
            Some(Instruction::Call) => Instruction::Call,
            Some(Instruction::Different) => Instruction::Different,
            Some(Instruction::Divide) => Instruction::Divide,
            Some(Instruction::ElIf) => Instruction::ElIf,
            Some(Instruction::Else) => Instruction::Else,
            Some(Instruction::EndFn) => Instruction::EndFn,
            Some(Instruction::EndIf) => Instruction::EndIf,
            Some(Instruction::EndWhile) => Instruction::EndWhile,
            Some(Instruction::Equals) => Instruction::Equals,
            Some(Instruction::False) => Instruction::False,
            Some(Instruction::Fn) => Instruction::Fn,
            Some(Instruction::GetVal) => Instruction::GetVal,
            Some(Instruction::GetValIdx) => Instruction::GetValIdx,
            Some(Instruction::Gt) => Instruction::Gt,
            Some(Instruction::Gte) => Instruction::Gte,
            Some(Instruction::If) => Instruction::If,
            Some(Instruction::LAnd) => Instruction::LAnd,
            Some(Instruction::LiteralArray) => Instruction::LiteralArray,
            Some(Instruction::LiteralChar) => Instruction::LiteralChar,
            Some(Instruction::LiteralFloat) => Instruction::LiteralFloat,
            Some(Instruction::LiteralInteger) => Instruction::LiteralInteger,
            Some(Instruction::LiteralNil) => Instruction::LiteralNil,
            Some(Instruction::LiteralString) => Instruction::LiteralString,
            Some(Instruction::LOr) => Instruction::LOr,
            Some(Instruction::Lt) => Instruction::Lt,
            Some(Instruction::Lte) => Instruction::Lte,
            Some(Instruction::Modulo) => Instruction::Modulo,
            Some(Instruction::Negate) => Instruction::Negate,
            Some(Instruction::Next) => Instruction::Next,
            Some(Instruction::Nil) => Instruction::Nil,
            Some(Instruction::Not) => Instruction::Not,
            Some(Instruction::Return) => Instruction::Return,
            Some(Instruction::Subtract) => Instruction::Subtract,
            Some(Instruction::Times) => Instruction::Times,
            Some(Instruction::True) => Instruction::True,
            Some(Instruction::While) => Instruction::While,
            None => return Err(Error::UnrecognisedToken),
        };
        Ok(instruction)
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
                // variable name
                arg_offset =
                    self.decoded_skip_instruction(Instruction::LiteralString, arg_offset)?;
                // expression
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
            Instruction::Call => {
                // func name
                arg_offset =
                    self.decoded_skip_instruction(Instruction::LiteralString, arg_offset)?;
                // list of expressions
                arg_offset =
                    self.decoded_skip_instruction(Instruction::LiteralArray, arg_offset)?;
            }
            Instruction::Fn => {
                // func name
                arg_offset =
                    self.decoded_skip_instruction(Instruction::LiteralString, arg_offset)?;
                // list of param names
                let num_params = self.code[arg_offset] as usize;
                arg_offset += 1;
                for _ in 0..num_params {
                    arg_offset =
                        self.decoded_skip_instruction(Instruction::LiteralString, arg_offset)?;
                }
            }
            Instruction::GetVal => {
                // variable name
                arg_offset =
                    self.decoded_skip_instruction(Instruction::LiteralString, arg_offset)?;
            }
            Instruction::GetValIdx => {
                // variable name
                arg_offset =
                    self.decoded_skip_instruction(Instruction::LiteralString, arg_offset)?;
                // Index
                arg_offset += 1;
            }
            Instruction::If => {
                arg_offset = self.skip_instruction(arg_offset)?;
            }
            Instruction::LiteralInteger => {
                arg_offset += 4;
            }
            Instruction::LiteralString => {
                let length = self.code[arg_offset] as usize;
                arg_offset += 1;
                arg_offset += length;
            }
            Instruction::LiteralChar => {
                arg_offset += 1;
            }
            Instruction::LiteralFloat => {
                arg_offset += 4;
            }
            Instruction::Not | Instruction::Negate => {
                arg_offset = self.skip_instruction(arg_offset)?;
            }
            Instruction::Return => {
                arg_offset = self.skip_instruction(arg_offset)?;
            }
            Instruction::While => {
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
            match self.read_instruction(offset)? {
                Instruction::Fn => {
                    let fun_name = self.read_string(offset + 1);
                    if fun_name == search_name {
                        return Ok(offset);
                    }
                }
                i => offset += self.decoded_skip_instruction(i, offset + 1)?,
            }
        }
    }

    pub fn run_function(&mut self, name: &[u8]) -> Result<Value<'a>, Error> {
        let mut offset = self.find_function(name)?;
        loop {
            let instruction = self.read_instruction(offset)?;
            if instruction == Instruction::Return {
                return Ok(Value::Nil);
            } else {
                offset = self.decoded_skip_instruction(instruction, offset + 1)?;
            }
        }
    }
}
