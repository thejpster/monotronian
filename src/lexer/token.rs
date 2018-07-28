#[derive(PartialEq, Clone)]
pub enum Token<'a> {
    As,
    Assign,
    BoolLiteral(bool),
    Break,
    Caret,
    Colon,
    Comma,
    DecimalFloatLiteral(f64),
    DecimalIntLiteral(i64),
    Dim,
    Else,
    ElseIf,
    End,
    EndIf,
    EOF,
    Equal,
    ExclamationMark,
    For,
    Gosub,
    Goto,
    GreaterThan,
    GreaterThanEqual,
    HexIntLiteral(i64),
    Identifier(&'a [u8]),
    If,
    Input,
    Integer,
    IntegerIdentifier(&'a [u8]),
    LeftRoundBracket,
    LessThan,
    LessThanEqual,
    Let,
    Minus,
    NewLine,
    Next,
    Not,
    NotEqual,
    Plus,
    Print,
    Return,
    RightRoundBracket,
    SemiColon,
    Slash,
    Star,
    StringIdentifier(&'a [u8]),
    StringLiteral(&'a [u8]),
    Then,
}

impl<'a> ::core::fmt::Debug for Token<'a> {
    fn fmt(&self, fmt: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        use super::display_ascii_string;
        match self {
            Token::As => write!(fmt, "Token::As"),
            Token::Assign => write!(fmt, "Token::Assign"),
            Token::BoolLiteral(b) => write!(fmt, "Token::BoolLiteral({})", b),
            Token::Break => write!(fmt, "Token::Break"),
            Token::Caret => write!(fmt, "Token::Caret"),
            Token::Colon => write!(fmt, "Token::Colon"),
            Token::Comma => write!(fmt, "Token::Comma"),
            Token::DecimalFloatLiteral(f) => write!(fmt, "Token::DecimalFloatLiteral({})", f),
            Token::DecimalIntLiteral(i) => write!(fmt, "Token::DecimalIntLiteral({})", i),
            Token::Dim => write!(fmt, "Token::Dim"),
            Token::Else => write!(fmt, "Token::Else"),
            Token::ElseIf => write!(fmt, "Token::ElseIf"),
            Token::End => write!(fmt, "Token::End"),
            Token::EndIf => write!(fmt, "Token::EndIf"),
            Token::EOF => write!(fmt, "Token::EOF"),
            Token::Equal => write!(fmt, "Token::Equal"),
            Token::ExclamationMark => write!(fmt, "Token::ExclamationMark"),
            Token::For => write!(fmt, "Token::For"),
            Token::Gosub => write!(fmt, "Token::Gosub"),
            Token::Goto => write!(fmt, "Token::Goto"),
            Token::GreaterThan => write!(fmt, "Token::GreaterThan"),
            Token::GreaterThanEqual => write!(fmt, "Token::GreaterThanEqual"),
            Token::HexIntLiteral(h) => write!(fmt, "Token::HexIntLiteral(0x{:x})", h),
            Token::Identifier(id) => {
                write!(fmt, "Token::Identifier(")?;
                display_ascii_string(id, fmt)?;
                write!(fmt, ")")
            }
            Token::If => write!(fmt, "Token::If"),
            Token::Input => write!(fmt, "Token::Input"),
            Token::Integer => write!(fmt, "Token::Integer"),
            Token::IntegerIdentifier(id) => {
                write!(fmt, "Token::IntegerIdentifier(")?;
                display_ascii_string(id, fmt)?;
                write!(fmt, ")")
            }
            Token::LeftRoundBracket => write!(fmt, "Token::("),
            Token::LessThan => write!(fmt, "Token::LessThan"),
            Token::LessThanEqual => write!(fmt, "Token::LessThanEqual"),
            Token::Let => write!(fmt, "Token::Let"),
            Token::Minus => write!(fmt, "Token::Minus"),
            Token::NewLine => write!(fmt, "Token::NewLine"),
            Token::Next => write!(fmt, "Token::Next"),
            Token::Not => write!(fmt, "Token::Not"),
            Token::NotEqual => write!(fmt, "Token::NotEqual"),
            Token::Plus => write!(fmt, "Token::Plus"),
            Token::Print => write!(fmt, "Token::Print"),
            Token::Return => write!(fmt, "Token::Return"),
            Token::RightRoundBracket => write!(fmt, "Token::)"),
            Token::SemiColon => write!(fmt, "Token::SemiColon"),
            Token::Slash => write!(fmt, "Token::Slash"),
            Token::Star => write!(fmt, "Token::Star"),
            Token::StringIdentifier(id) => {
                write!(fmt, "Token::StringIdentifier(")?;
                display_ascii_string(id, fmt)?;
                write!(fmt, ")")
            }
            Token::StringLiteral(lit) => {
                write!(fmt, "Token::StringLiteral(\"")?;
                display_ascii_string(lit, fmt)?;
                write!(fmt, "\")")
            }
            Token::Then => write!(fmt, "Token::Then"),
        }
    }
}
