//! # Monotronian Byte Code
//!
//! This module defines the Monotronian byte-code format. Unlike say, WASM or
//! LLVM IR, this byte-code is designed to be losslessly converted back to the
//! original source code (or at least as close as possible).

use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

use alloc::vec::Vec;

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
    LiteralInteger8,
    LiteralIntegerHex8,
    LiteralInteger16,
    LiteralIntegerHex16,
    LiteralInteger24,
    LiteralIntegerHex24,
    LiteralInteger32,
    LiteralIntegerHex32,
    LiteralNil,
    LiteralString,
    LOr,
    Lt,
    Lte,
    Modulo,
    Negate,
    Next,
    Not,
    Return,
    Subtract,
    Times,
    True,
    While,
}

struct Variable<'a> {
    name: Vec<u8>,
    value: Value<'a>,
}

pub struct Program<'a> {
    code: &'a [u8],
    /// TODO: Give each variable a scope, so they can be deleted when we
    /// return from a function. Also, create a variable which is our current
    /// scope level.
    variables: core::cell::RefCell<Vec<Variable<'a>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value<'a> {
    OwnedString(Vec<u8>),
    StringLiteral(&'a [u8]),
    Integer(i32),
    Float(f32),
    Boolean(bool),
    Char(u8),
    Nil,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Error {
    FunctionNotFound,
    UnrecognisedToken,
    InvalidOperandType,
    OutOfBounds(usize),
    UndefinedVariable,
    UnexpectedEndOfFunction,
}

impl<'a> Program<'a> {
    pub fn new(code: &'a [u8]) -> Program {
        Program {
            code,
            variables: core::cell::RefCell::new(Vec::new()),
        }
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

    fn get_value(&self, name: &[u8]) -> Option<Value> {
        let vars = self.variables.try_borrow().unwrap();
        for item in vars.iter() {
            if item.name == name {
                return Some(item.value.clone());
            }
        }
        None
    }

    fn set_value(&self, name: &[u8], value: Value<'a>) {
        let mut vars = self.variables.try_borrow_mut().unwrap();
        for item in vars.iter_mut() {
            if item.name == name {
                item.value = value;
                return;
            }
        }
        let mut name_vec = Vec::new();
        name_vec.extend_from_slice(name);
        vars.push(Variable {
            name: name_vec,
            value,
        });
    }

    fn execute_instructions(&'a self, mut offset: usize) -> Result<(Value<'a>, usize), Error> {
        let i = self.read_instruction(offset)?;
        offset += 1;
        match i {
            Instruction::Add => {
                let (lhs, offset) = self.execute_instructions(offset)?;
                let (rhs, offset) = self.execute_instructions(offset)?;
                match (lhs, rhs) {
                    // (int, int) -> int
                    (Value::Integer(x), Value::Integer(y)) => Ok((Value::Integer(x + y), offset)),
                    // (float, float) -> float
                    (Value::Float(x), Value::Float(y)) => Ok((Value::Float(x + y), offset)),
                    // (float, int) -> float
                    (Value::Float(x), Value::Integer(y)) => {
                        Ok((Value::Float(x + y as f32), offset))
                    }
                    // (int, float) -> float
                    (Value::Integer(x), Value::Float(y)) => {
                        Ok((Value::Float(x as f32 + y), offset))
                    }
                    // Can't add any other sort of thing together
                    _ => Err(Error::InvalidOperandType),
                }
            }
            Instruction::Subtract => {
                let (lhs, offset) = self.execute_instructions(offset)?;
                let (rhs, offset) = self.execute_instructions(offset)?;
                match (lhs, rhs) {
                    // (int, int) -> int
                    (Value::Integer(x), Value::Integer(y)) => Ok((Value::Integer(x - y), offset)),
                    // (float, float) -> float
                    (Value::Float(x), Value::Float(y)) => Ok((Value::Float(x - y), offset)),
                    // (float, int) -> float
                    (Value::Float(x), Value::Integer(y)) => {
                        Ok((Value::Float(x - (y as f32)), offset))
                    }
                    // (int, float) -> float
                    (Value::Integer(x), Value::Float(y)) => {
                        Ok((Value::Float((x as f32) - y), offset))
                    }
                    // Can't subtract any other sort of things
                    _ => Err(Error::InvalidOperandType),
                }
            }
            Instruction::Times => {
                let (lhs, offset) = self.execute_instructions(offset)?;
                let (rhs, offset) = self.execute_instructions(offset)?;
                match (lhs, rhs) {
                    // (int, int) -> int
                    (Value::Integer(x), Value::Integer(y)) => Ok((Value::Integer(x * y), offset)),
                    // (float, float) -> float
                    (Value::Float(x), Value::Float(y)) => Ok((Value::Float(x * y), offset)),
                    // (float, int) -> float
                    (Value::Float(x), Value::Integer(y)) => {
                        Ok((Value::Float(x * (y as f32)), offset))
                    }
                    // (int, float) -> float
                    (Value::Integer(x), Value::Float(y)) => {
                        Ok((Value::Float((x as f32) * y), offset))
                    }
                    // Can't multiply any other sort of things
                    _ => Err(Error::InvalidOperandType),
                }
            }
            Instruction::Divide => {
                // Divide the first numeric argument by the second
                let (lhs, offset) = self.execute_instructions(offset)?;
                let (rhs, offset) = self.execute_instructions(offset)?;
                match (lhs, rhs) {
                    // (int, int) -> int
                    (Value::Integer(x), Value::Integer(y)) => Ok((Value::Integer(x / y), offset)),
                    // (float, float) -> float
                    (Value::Float(x), Value::Float(y)) => Ok((Value::Float(x / y), offset)),
                    // (float, int) -> float
                    (Value::Float(x), Value::Integer(y)) => {
                        Ok((Value::Float(x / (y as f32)), offset))
                    }
                    // (int, float) -> float
                    (Value::Integer(x), Value::Float(y)) => {
                        Ok((Value::Float((x as f32) / y), offset))
                    }
                    // Can't multiply any other sort of things
                    _ => Err(Error::InvalidOperandType),
                }
            }
            Instruction::Return => {
                // Evaluate the expression and stop executing the current function
                self.execute_instructions(offset)
            }
            Instruction::LiteralInteger8 | Instruction::LiteralIntegerHex8 => {
                // A literal little-endian 8-bit integer
                Ok((Value::Integer(self.code[offset] as i32), offset + 1))
            }
            Instruction::LiteralInteger16 | Instruction::LiteralIntegerHex16 => {
                // A literal little-endian 16-bit integer
                let buf = [self.code[offset], self.code[offset + 1]];
                let result = i16::from_le_bytes(buf);
                Ok((Value::Integer(result as i32), offset + 2))
            }
            Instruction::LiteralInteger24 | Instruction::LiteralIntegerHex24 => {
                // A literal little-endian 24-bit integer
                let buf = [
                    self.code[offset],
                    self.code[offset + 1],
                    self.code[offset + 2],
                    // Do sign extension
                    if (self.code[offset + 2] & 0x80) != 0 {
                        0xFF
                    } else {
                        0x00
                    },
                ];
                let result = i32::from_le_bytes(buf);
                Ok((Value::Integer(result), offset + 3))
            }
            Instruction::LiteralInteger32 | Instruction::LiteralIntegerHex32 => {
                // A literal little-endian 32-bit integer
                let buf = [
                    self.code[offset],
                    self.code[offset + 1],
                    self.code[offset + 2],
                    self.code[offset + 3],
                ];
                let result = i32::from_le_bytes(buf);
                Ok((Value::Integer(result), offset + 4))
            }
            Instruction::LiteralFloat => {
                // A literal 32-bit floating point number
                let buf = [
                    self.code[offset],
                    self.code[offset + 1],
                    self.code[offset + 2],
                    self.code[offset + 3],
                ];
                let result = u32::from_le_bytes(buf);
                let float_result = f32::from_bits(result);
                Ok((Value::Float(float_result), offset + 4))
            }
            Instruction::LiteralChar => {
                // A literal 8-bit character
                let result = self.code[offset];
                Ok((Value::Char(result), offset + 1))
            }
            Instruction::Call => {
                // Call a named function, with some arguments. Returns the
                // result of that function.
                //
                // Get function name
                let function_name = self.read_string(offset);
                offset += 1 + function_name.len();
                // Get number of arguments (up to 8)
                let num_args = self.code[offset] as usize;
                offset += 1;
                // Call function with those arguments
                let mut args: Vec<Value> = Vec::new();
                for _i in 0..num_args {
                    let (arg, new_offset) = self.execute_instructions(offset)?;
                    args.push(arg);
                    offset = new_offset;
                }
                let result = self.run_function(function_name, &args)?;
                Ok((result, offset))
            }
            Instruction::LiteralNil => {
                // A literal nil value
                Ok((Value::Nil, offset))
            }
            Instruction::Assign => {
                // Set the named variable to the given value
                let name = self.read_string(offset);
                offset += name.len() + 1;
                let (value, offset) = self.execute_instructions(offset)?;
                self.set_value(name, value);
                Ok((Value::Nil, offset))
            }
            Instruction::BAnd => {
                // Perform a bitwise AND on the two integer arguments
                let (lhs, offset) = self.execute_instructions(offset)?;
                let (rhs, offset) = self.execute_instructions(offset)?;
                match (lhs, rhs) {
                    // (int, int) -> int
                    (Value::Integer(x), Value::Integer(y)) => Ok((Value::Integer(x & y), offset)),
                    // Can't bitwise AND any other sort of things
                    _ => Err(Error::InvalidOperandType),
                }
            }
            Instruction::Boolean => {
                // Convert the given argument to a boolean value.
                let (value, offset) = self.execute_instructions(offset)?;
                Ok((
                    Value::Boolean(match value {
                        Value::Integer(n) => n != 0,
                        Value::StringLiteral(s) => !s.is_empty(),
                        Value::OwnedString(s) => !s.is_empty(),
                        Value::Float(_f) => {
                            return Err(Error::InvalidOperandType);
                        }
                        Value::Boolean(b) => b,
                        Value::Char(c) => c != 0,
                        Value::Nil => false,
                    }),
                    offset,
                ))
            }
            Instruction::BOr => {
                // jump to the end of the current while loop or for loop
                let (lhs, offset) = self.execute_instructions(offset)?;
                let (rhs, offset) = self.execute_instructions(offset)?;
                match (lhs, rhs) {
                    // (int, int) -> int
                    (Value::Integer(x), Value::Integer(y)) => Ok((Value::Integer(x | y), offset)),
                    // Can't bitwise AND any other sort of things
                    _ => Err(Error::InvalidOperandType),
                }
            }
            Instruction::Break => {
                // jump to the end of the current while loop or for loop
                unimplemented!();
            }
            Instruction::Different => {
                // boolean true if these two arguments are not the same
                let (lhs, offset) = self.execute_instructions(offset)?;
                let (rhs, offset) = self.execute_instructions(offset)?;
                match (lhs, rhs) {
                    (Value::StringLiteral(x), Value::StringLiteral(y)) => {
                        Ok((Value::Boolean(x != y), offset))
                    }
                    (Value::OwnedString(x), Value::OwnedString(y)) => {
                        Ok((Value::Boolean(x != y), offset))
                    }
                    (Value::OwnedString(x), Value::StringLiteral(y)) => {
                        Ok((Value::Boolean(x != y), offset))
                    }
                    (Value::StringLiteral(x), Value::OwnedString(y)) => {
                        Ok((Value::Boolean(y != x), offset))
                    }
                    (Value::Integer(x), Value::Integer(y)) => Ok((Value::Boolean(x != y), offset)),
                    (Value::Float(x), Value::Float(y)) => {
                        Ok((Value::Boolean((x - y).abs() < core::f32::EPSILON), offset))
                    }
                    (Value::Boolean(x), Value::Boolean(y)) => Ok((Value::Boolean(x != y), offset)),
                    (Value::Char(x), Value::Char(y)) => Ok((Value::Boolean(x != y), offset)),
                    (Value::Nil, Value::Nil) => Ok((Value::Boolean(false), offset)),
                    // anything else is different
                    _ => Ok((Value::Boolean(true), offset)),
                }
            }
            Instruction::ElIf => {
                // Evaluate the conditional. If false, jump to the else block,
                // or the matching endif, or the next elif (whichever comes first).
                unimplemented!();
            }
            Instruction::Else => {
                // Marks the final part of an if statement
                unimplemented!();
            }
            Instruction::EndFn => {
                // Ends a function
                unimplemented!();
            }
            Instruction::EndIf => {
                // Ends an if statement block. It's basically a no-op.
                Ok((Value::Nil, offset))
            }
            Instruction::EndWhile => {
                // Ends a while loop
                unimplemented!();
            }
            Instruction::Equals => {
                // Are these two arguments equal to each other
                let (lhs, offset) = self.execute_instructions(offset)?;
                let (rhs, offset) = self.execute_instructions(offset)?;
                match (lhs, rhs) {
                    (Value::StringLiteral(x), Value::StringLiteral(y)) => {
                        Ok((Value::Boolean(x == y), offset))
                    }
                    (Value::Integer(x), Value::Integer(y)) => Ok((Value::Boolean(x == y), offset)),
                    (Value::Float(x), Value::Float(y)) => {
                        Ok((Value::Boolean((x - y).abs() < core::f32::EPSILON), offset))
                    }
                    (Value::Boolean(x), Value::Boolean(y)) => Ok((Value::Boolean(x == y), offset)),
                    (Value::Char(x), Value::Char(y)) => Ok((Value::Boolean(x == y), offset)),
                    (Value::Nil, Value::Nil) => Ok((Value::Boolean(true), offset)),
                    // anything else is different
                    _ => Ok((Value::Boolean(false), offset)),
                }
            }
            Instruction::False => {
                // Literal boolean non-truth value
                Ok((Value::Boolean(false), offset))
            }
            Instruction::Fn => {
                // Starts a function
                unimplemented!();
            }
            Instruction::GetVal => {
                // Get a value with the given name
                let name = self.read_string(offset);
                offset += name.len() + 1;
                match self.get_value(name) {
                    Some(v) => Ok((v, offset)),
                    None => Err(Error::UndefinedVariable),
                }
            }
            Instruction::GetValIdx => {
                // Get a value from the named array at the specified index
                unimplemented!();
            }
            Instruction::Gt => {
                // Boolean result - is the first argument greater than the second argument
                let (lhs, offset) = self.execute_instructions(offset)?;
                let (rhs, offset) = self.execute_instructions(offset)?;
                match (lhs, rhs) {
                    (Value::StringLiteral(x), Value::StringLiteral(y)) => {
                        Ok((Value::Boolean(x > y), offset))
                    }
                    (Value::Integer(x), Value::Integer(y)) => Ok((Value::Boolean(x > y), offset)),
                    (Value::Float(x), Value::Float(y)) => Ok((Value::Boolean(x > y), offset)),
                    (Value::Char(x), Value::Char(y)) => Ok((Value::Boolean(x > y), offset)),
                    // Can't compare any other sort of things
                    _ => Err(Error::InvalidOperandType),
                }
            }
            Instruction::Gte => {
                // Boolean result - is the first argument greater than or equal to the second argument
                let (lhs, offset) = self.execute_instructions(offset)?;
                let (rhs, offset) = self.execute_instructions(offset)?;
                match (lhs, rhs) {
                    (Value::StringLiteral(x), Value::StringLiteral(y)) => {
                        Ok((Value::Boolean(x >= y), offset))
                    }
                    (Value::Integer(x), Value::Integer(y)) => Ok((Value::Boolean(x >= y), offset)),
                    (Value::Float(x), Value::Float(y)) => Ok((Value::Boolean(x >= y), offset)),
                    (Value::Char(x), Value::Char(y)) => Ok((Value::Boolean(x >= y), offset)),
                    // Can't compare any other sort of things
                    _ => Err(Error::InvalidOperandType),
                }
            }
            Instruction::If => {
                // Evaluate the conditional. If false, jump to the else block,
                // or the matching endif, or the next elif (whichever comes first).
                let (conditional, new_offset) = self.execute_instructions(offset)?;
                offset = new_offset;
                if let Value::Boolean(x) = conditional {
                    if x {
                        // Execute the code immediately after
                        Ok((Value::Nil, offset))
                    } else {
                        // increment offset until we find and endif
                        let mut depth = 0;
                        loop {
                            let i = self.read_instruction(offset)?;
                            offset += 1;
                            match i {
                                Instruction::If => {
                                    depth = depth + 1;
                                    offset = self.decoded_skip_instruction(i, offset)?;
                                }
                                Instruction::EndIf if depth == 0 => {
                                    offset = self.decoded_skip_instruction(i, offset)?;
                                    return Ok((Value::Nil, offset));
                                }
                                Instruction::EndIf if depth > 0 => {
                                    depth = depth - 1;
                                    offset = self.decoded_skip_instruction(i, offset)?;
                                }
                                Instruction::EndFn => {
                                    return Err(Error::UnexpectedEndOfFunction);
                                }
                                _ => {
                                    offset = self.decoded_skip_instruction(i, offset)?;
                                }
                            }
                        }
                    }
                } else {
                    Err(Error::InvalidOperandType)
                }
            }
            Instruction::LAnd => {
                // Logical AND of the two boolean arguments
                let (lhs, offset) = self.execute_instructions(offset)?;
                let (rhs, offset) = self.execute_instructions(offset)?;
                match (lhs, rhs) {
                    // (bool, bool) -> bool
                    (Value::Boolean(x), Value::Boolean(y)) => Ok((Value::Boolean(x && y), offset)),
                    // Can't logical AND any other sort of things
                    _ => Err(Error::InvalidOperandType),
                }
            }
            Instruction::LiteralArray => {
                // Don't have support for array types yet - need Vec<Value>
                unimplemented!();
            }
            Instruction::LiteralString => {
                // A string literal follows.
                //
                // Get string length (up to 255)
                let name = self.read_string(offset);
                offset += name.len() + 1;
                let result = Value::StringLiteral(name);
                Ok((result, offset))
            }
            Instruction::LOr => {
                // Logical OR of the two boolean arguments
                let (lhs, offset) = self.execute_instructions(offset)?;
                let (rhs, offset) = self.execute_instructions(offset)?;
                match (lhs, rhs) {
                    // (bool, bool) -> bool
                    (Value::Boolean(x), Value::Boolean(y)) => Ok((Value::Boolean(x || y), offset)),
                    // Can't logical AND any other sort of things
                    _ => Err(Error::InvalidOperandType),
                }
            }
            Instruction::Lt => {
                // Boolean result - is the first argument less than the second argument
                let (lhs, offset) = self.execute_instructions(offset)?;
                let (rhs, offset) = self.execute_instructions(offset)?;
                match (lhs, rhs) {
                    (Value::StringLiteral(x), Value::StringLiteral(y)) => {
                        Ok((Value::Boolean(x < y), offset))
                    }
                    (Value::Integer(x), Value::Integer(y)) => Ok((Value::Boolean(x < y), offset)),
                    (Value::Float(x), Value::Float(y)) => Ok((Value::Boolean(x < y), offset)),
                    (Value::Char(x), Value::Char(y)) => Ok((Value::Boolean(x < y), offset)),
                    // Can't compare any other sort of things
                    _ => Err(Error::InvalidOperandType),
                }
            }
            Instruction::Lte => {
                // Boolean result - is the first argument less than or equal to the second argument
                let (lhs, offset) = self.execute_instructions(offset)?;
                let (rhs, offset) = self.execute_instructions(offset)?;
                match (lhs, rhs) {
                    (Value::StringLiteral(x), Value::StringLiteral(y)) => {
                        Ok((Value::Boolean(x <= y), offset))
                    }
                    (Value::Integer(x), Value::Integer(y)) => Ok((Value::Boolean(x <= y), offset)),
                    (Value::Float(x), Value::Float(y)) => Ok((Value::Boolean(x <= y), offset)),
                    (Value::Char(x), Value::Char(y)) => Ok((Value::Boolean(x <= y), offset)),
                    // Can't compare any other sort of things
                    _ => Err(Error::InvalidOperandType),
                }
            }
            Instruction::Modulo => {
                // Perform modulo arithmetic on the two numeric arguments,
                // which must be of the same type.
                let (lhs, offset) = self.execute_instructions(offset)?;
                let (rhs, offset) = self.execute_instructions(offset)?;
                match (lhs, rhs) {
                    (Value::Integer(x), Value::Integer(y)) => Ok((Value::Integer(x % y), offset)),
                    (Value::Float(x), Value::Float(y)) => Ok((Value::Float(x % y), offset)),
                    // Can't compare any other sort of things
                    _ => Err(Error::InvalidOperandType),
                }
            }
            Instruction::Negate => {
                // Negate the numeric argument
                let (operand, offset) = self.execute_instructions(offset)?;
                match operand {
                    Value::Integer(x) => Ok((Value::Integer(-x), offset)),
                    Value::Float(x) => Ok((Value::Float(-x), offset)),
                    // Can't negate any other sort of things
                    _ => Err(Error::InvalidOperandType),
                }
            }
            Instruction::Next => {
                // Check the conditional of the most recent for loop and
                // either go back, or continue
                unimplemented!();
            }
            Instruction::Not => {
                // Invert the boolean argument
                let (operand, offset) = self.execute_instructions(offset)?;
                match operand {
                    Value::Boolean(x) => Ok((Value::Boolean(!x), offset)),
                    // Can't negate any other sort of things
                    _ => Err(Error::InvalidOperandType),
                }
            }
            Instruction::True => {
                // A literal boolean True value
                Ok((Value::Boolean(true), offset))
            }
            Instruction::While => {
                // Evaluate the conditional and, if true, run the loop. Else
                // jump to the matching endwhile.
                unimplemented!();
            }
        }
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
            | Instruction::Next => {
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
            // An i8 literal integer
            Instruction::LiteralInteger8 | Instruction::LiteralIntegerHex8 => {
                arg_offset += 1;
            }
            // An i16 literal integer
            Instruction::LiteralInteger16 | Instruction::LiteralIntegerHex16 => {
                arg_offset += 2;
            }
            // An i24 literal integer
            Instruction::LiteralInteger24 | Instruction::LiteralIntegerHex24 => {
                arg_offset += 3;
            }
            // An i32 literal integer
            Instruction::LiteralInteger32 | Instruction::LiteralIntegerHex32 => {
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
                    offset = self.decoded_skip_instruction(Instruction::Fn, offset + 1)?;
                    if fun_name == search_name {
                        return Ok(offset);
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

    pub fn run_function(&'a self, name: &[u8], _args: &[Value]) -> Result<Value<'a>, Error> {
        let mut offset = self.find_function(name)?;
        loop {
            match self.read_instruction(offset) {
                Ok(Instruction::Return) => {
                    let (value, _new_offset) = self.execute_instructions(offset)?;
                    return Ok(value);
                }
                Ok(Instruction::EndFn) => {
                    return Ok(Value::Nil);
                }
                Ok(_i) => {
                    println!("Executing at {} ({:02x})", offset, self.code[offset]);
                    let (_value, new_offset) = self.execute_instructions(offset)?;
                    offset = new_offset;
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
            Instruction::EndFn as u8,
        ];
        let p = Program::new(&byte_code);
        assert_eq!(p.run_function(b"main", &[]), Ok(Value::Nil));
    }

    #[test]
    fn function_call() {
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
            Instruction::Add as u8,
            Instruction::Call as u8,
            3,
            b'f',
            b'o',
            b'o',
            0,
            Instruction::Call as u8,
            3,
            b'f',
            b'o',
            b'o',
            0,
            Instruction::EndFn as u8,
            Instruction::Fn as u8,
            0x03,
            b'f',
            b'o',
            b'o',
            0x00,
            Instruction::Return as u8,
            Instruction::LiteralInteger32 as u8,
            1,
            2,
            3,
            4,
            Instruction::EndFn as u8,
        ];
        let p = Program::new(&byte_code);
        assert_eq!(p.run_function(b"main", &[]), Ok(Value::Integer(0x08060402)));
    }

    #[test]
    fn if_statement() {
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
            Instruction::Assign as u8,
            0x03,
            b'r',
            b'e',
            b's',
            Instruction::LiteralInteger32 as u8,
            0x00,
            0x00,
            0x00,
            0x00,
            Instruction::If as u8,
            Instruction::Equals as u8,
            Instruction::LiteralInteger32 as u8,
            0xCC,
            0x00,
            0x00,
            0x00,
            Instruction::LiteralInteger32 as u8,
            0xCC,
            0x00,
            0x00,
            0x00,
            Instruction::Assign as u8,
            0x03,
            b'r',
            b'e',
            b's',
            Instruction::LiteralInteger32 as u8,
            0x01,
            0x00,
            0x00,
            0x00,
            Instruction::EndIf as u8,
            Instruction::Return as u8,
            Instruction::GetVal as u8,
            0x03,
            b'r',
            b'e',
            b's',
            Instruction::EndFn as u8,
        ];
        let p = Program::new(&byte_code);
        assert_eq!(p.run_function(b"main", &[]), Ok(Value::Integer(1)));
    }

    #[test]
    fn if_statement_negative() {
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
            Instruction::Assign as u8,
            0x03,
            b'r',
            b'e',
            b's',
            Instruction::LiteralInteger32 as u8,
            0x00,
            0x00,
            0x00,
            0x00,
            Instruction::If as u8,
            Instruction::Equals as u8,
            Instruction::LiteralInteger32 as u8,
            0xCC,
            0x00,
            0x00,
            0x01,
            Instruction::LiteralInteger32 as u8,
            0xCC,
            0x00,
            0x00,
            0x00,
            Instruction::Assign as u8,
            0x03,
            b'r',
            b'e',
            b's',
            Instruction::LiteralInteger8 as u8,
            0x01,
            Instruction::EndIf as u8,
            Instruction::Assign as u8,
            0x03,
            b'r',
            b'e',
            b's',
            Instruction::Add as u8,
            Instruction::GetVal as u8,
            0x03,
            b'r',
            b'e',
            b's',
            Instruction::LiteralInteger8 as u8,
            0x01,
            Instruction::Return as u8,
            Instruction::GetVal as u8,
            0x03,
            b'r',
            b'e',
            b's',
            Instruction::EndFn as u8,
        ];
        let p = Program::new(&byte_code);
        assert_eq!(p.run_function(b"main", &[]), Ok(Value::Integer(1)));
    }

    #[test]
    fn make_variable() {
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
            Instruction::Assign as u8,
            0x03,
            b'f',
            b'o',
            b'o',
            Instruction::LiteralInteger8 as u8,
            0x00,
            Instruction::Return as u8,
            Instruction::GetVal as u8,
            0x03,
            b'f',
            b'o',
            b'o',
        ];
        let p = Program::new(&byte_code);
        assert_eq!(p.run_function(b"main", &[]), Ok(Value::Integer(0)));
    }

    #[test]
    fn make_two_variables() {
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
            Instruction::Assign as u8,
            0x03,
            b'f',
            b'o',
            b'o',
            Instruction::LiteralInteger16 as u8,
            0x02,
            0x00,
            Instruction::Assign as u8,
            0x03,
            b'b',
            b'a',
            b'r',
            Instruction::LiteralInteger8 as u8,
            0x01,
            Instruction::Return as u8,
            Instruction::Add as u8,
            Instruction::GetVal as u8,
            0x03,
            b'b',
            b'a',
            b'r',
            Instruction::GetVal as u8,
            0x03,
            b'f',
            b'o',
            b'o',
        ];
        let p = Program::new(&byte_code);
        assert_eq!(p.run_function(b"main", &[]), Ok(Value::Integer(3)));
    }

    #[test]
    fn return_integer_zero() {
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
            Instruction::LiteralInteger24 as u8,
            0x00,
            0x00,
            0x00,
        ];
        let p = Program::new(&byte_code);
        assert_eq!(p.run_function(b"main", &[]), Ok(Value::Integer(0)));
    }

    #[test]
    fn return_integer_sum() {
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
            Instruction::Add as u8,
            Instruction::LiteralInteger8 as u8,
            0x01,
            Instruction::LiteralIntegerHex8 as u8,
            0x02,
        ];
        let p = Program::new(&byte_code);
        assert_eq!(p.run_function(b"main", &[]), Ok(Value::Integer(3)));
    }

    #[test]
    fn return_float_sum() {
        let arg1 = 0.1f32;
        let arg1_bytes = arg1.to_bits().to_le_bytes();
        let arg2 = 0.2f32;
        let arg2_bytes = arg2.to_bits().to_le_bytes();
        let result = arg1 + arg2;
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
            Instruction::Add as u8,
            Instruction::LiteralFloat as u8,
            arg1_bytes[0],
            arg1_bytes[1],
            arg1_bytes[2],
            arg1_bytes[3],
            Instruction::LiteralFloat as u8,
            arg2_bytes[0],
            arg2_bytes[1],
            arg2_bytes[2],
            arg2_bytes[3],
        ];
        let p = Program::new(&byte_code);
        assert_eq!(p.run_function(b"main", &[]), Ok(Value::Float(result)));
    }

    #[test]
    fn return_float_times() {
        let arg1 = 10.0f32;
        let arg1_bytes = arg1.to_bits().to_le_bytes();
        let arg2 = 20.0f32;
        let arg2_bytes = arg2.to_bits().to_le_bytes();
        let result = arg1 * arg2;
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
            Instruction::Times as u8,
            Instruction::LiteralFloat as u8,
            arg1_bytes[0],
            arg1_bytes[1],
            arg1_bytes[2],
            arg1_bytes[3],
            Instruction::LiteralFloat as u8,
            arg2_bytes[0],
            arg2_bytes[1],
            arg2_bytes[2],
            arg2_bytes[3],
        ];
        let p = Program::new(&byte_code);
        assert_eq!(p.run_function(b"main", &[]), Ok(Value::Float(result)));
    }

    #[test]
    fn check_implicit_int_float_conversion() {
        let arg1 = 10i32;
        let arg1_bytes = arg1.to_le_bytes();
        let arg2 = 20.0f32;
        let arg2_bytes = arg2.to_bits().to_le_bytes();
        let result = (arg1 as f32) * arg2;
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
            Instruction::Times as u8,
            Instruction::LiteralInteger32 as u8,
            arg1_bytes[0],
            arg1_bytes[1],
            arg1_bytes[2],
            arg1_bytes[3],
            Instruction::LiteralFloat as u8,
            arg2_bytes[0],
            arg2_bytes[1],
            arg2_bytes[2],
            arg2_bytes[3],
        ];
        let p = Program::new(&byte_code);
        assert_eq!(p.run_function(b"main", &[]), Ok(Value::Float(result)));
    }

    #[test]
    fn check_implicit_float_int_conversion() {
        let arg1 = 20.0f32;
        let arg1_bytes = arg1.to_bits().to_le_bytes();
        let arg2 = 10i32;
        let arg2_bytes = arg2.to_le_bytes();
        let result = arg1 * (arg2 as f32);
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
            Instruction::Times as u8,
            Instruction::LiteralFloat as u8,
            arg1_bytes[0],
            arg1_bytes[1],
            arg1_bytes[2],
            arg1_bytes[3],
            Instruction::LiteralInteger32 as u8,
            arg2_bytes[0],
            arg2_bytes[1],
            arg2_bytes[2],
            arg2_bytes[3],
        ];
        let p = Program::new(&byte_code);
        assert_eq!(p.run_function(b"main", &[]), Ok(Value::Float(result)));
    }

    #[test]
    fn return_integer_one() {
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
            Instruction::LiteralInteger8 as u8,
            0x01,
        ];
        let p = Program::new(&byte_code);
        assert_eq!(p.run_function(b"main", &[]), Ok(Value::Integer(1)));
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
            Instruction::LiteralNil as u8,
        ];
        let p = Program::new(&byte_code);
        assert_eq!(p.run_function(b"mainX", &[]), Err(Error::FunctionNotFound));
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
            Instruction::LiteralNil as u8,
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
            Instruction::LiteralNil as u8,
        ];
        let p = Program::new(&byte_code);
        assert_eq!(p.run_function(b"foo", &[]), Ok(Value::Nil));
    }
}
