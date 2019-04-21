/// Holds a [u8] slice. This wrapper ensures it is Debug printed as an ASCII
/// string (which is close enough for CodePage 850).
#[derive(PartialEq, Clone)]
pub struct ByteString<'a>(pub &'a [u8]);

/// These are just the operator tokens
#[derive(Clone, Copy, Debug, PartialOrd, PartialEq, Eq, Ord)]
pub enum Operator {
    Not,
    Star,
    Slash,
    Plus,
    Minus,
    GreaterThan,
    GreaterThanEqual,
    LessThan,
    LessThanEqual,
    NotEqual,
    Equals,
    BitAnd,
    BitXor,
    BitOr,
    And,
    Or,
}

/// These tokens include keywords, operators and identifiers
#[derive(PartialEq, Clone, Debug)]
pub enum Token<'a> {
    Operator(Operator),
    Assign,
    BoolLiteral(bool),
    Break,
    Case,
    Colon,
    Comma,
    DecimalFloatLiteral(f64),
    DecimalIntLiteral(i64),
    Else,
    ElseIf,
    EndCase,
    EndFunc,
    EndIf,
    EndWhile,
    EOF,
    For,
    Func,
    HexIntLiteral(i64),
    Identifier(ByteString<'a>),
    If,
    LeftRoundBracket,
    Global,
    NewLine,
    Next,
    Return,
    RightRoundBracket,
    SemiColon,
    Step,
    StringLiteral(ByteString<'a>),
    Then,
    To,
    Until,
    While,
}

impl<'a> ::core::fmt::Debug for ByteString<'a> {
    fn fmt(&self, fmt: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        use super::display_ascii_string;
        display_ascii_string(self.0, fmt)
    }
}

// End of file
