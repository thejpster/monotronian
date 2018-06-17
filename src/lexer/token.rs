#[derive(PartialEq, Debug, Clone)]
pub enum Token<'a> {
    EOF,
    Identifier(&'a [u8]),
    StringLiteral(&'a [u8]),
    DecimalIntLiteral(i64),
    HexIntLiteral(i64),
    BoolLiteral(bool),
    LeftRoundBracket,
    RightRoundBracket,
    LeftCurlyBracket,
    RightCurlyBracket,
    LeftSquareBracket,
    RightSquareBracket,
    Assign,
    If,
    Else,
    Let,
    For,
    While,
    Break,
    Return,
    Plus,
    Minus,
    Slash,
    Star,
    Equal,
    NotEqual,
    GreaterThanEqual,
    LessThanEqual,
    GreaterThan,
    LessThan,
    Not,
    Comma,
    Colon,
    SemiColon,
    ExclamationMark,
    Caret,
}
