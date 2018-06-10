#[derive(PartialEq, Debug, Clone)]
pub enum Token<'a> {
    Illegal,
    EOF,
    // identifier and literals
    Ident(&'a str),
    StringLiteral(&'a str),
    IntLiteral(i64),
    BoolLiteral(bool),
    // statements
    Assign,
    If,
    Else,
    // operators
    Plus,
    Minus,
    Divide,
    Multiply,
    Equal,
    NotEqual,
    GreaterThanEqual,
    LessThanEqual,
    GreaterThan,
    LessThan,
    Not,
    // reserved words
    Let,
    Return,
    // punctuations
    Comma,
    Colon,
    SemiColon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
}
