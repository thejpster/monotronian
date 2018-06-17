//! This is a lexer for the monotronian language.
//!
//! It can handle the keywords, typical punctuation for a curly bracket
//! language, integers, identifiers and strings. It's like C, but using the
//! `let` keyword, and without any type information.
//!
//! There's also no concept of declaring a function because each function is
//! handled as a stand-alone unit to save memory.

use nom;
use nom::types::CompleteByteSlice;

use super::display_ascii_string;
pub use self::token::Token;
pub mod token;

pub struct Lexer;

#[derive(PartialEq, Clone, Copy)]
pub enum Error<'a> {
    SyntaxError(&'a [u8]),
    BadNumber(&'a [u8]),
}

impl<'a> ::core::fmt::Debug for Error<'a> {
    fn fmt(&self, fmt: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        match self {
            Error::SyntaxError(e) => {
                write!(fmt, "SyntaxError at \"")?;
                display_ascii_string(&e[0..e.len().max(20)], fmt)?;
                write!(fmt, "\"")
            }
            Error::BadNumber(e) => {
                write!(fmt, "BadNumber at \"")?;
                display_ascii_string(&e[0..e.len().max(20)], fmt)?;
                write!(fmt, "\"")
            }
        }
    }
}

pub struct TokenIterator<'a> {
    input: &'a [u8],
}

impl<'a> Iterator for TokenIterator<'a> {
    type Item = Result<Token<'a>, Error<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.input.len() == 0 {
            None
        } else {
            // get the next token from the start of the string
            let res = Lexer::lex_tokens(self.input);
            match res {
                Ok((tok, remainder)) => {
                    self.input = remainder;
                    Some(Ok(tok))
                }
                Err(e) => Some(Err(e)),
            }
        }
    }
}

/// Check if a string is a reserved word or an identifier.
fn parse_reserved(word: CompleteByteSlice) -> Option<Token> {
    match word.0 {
        b"break" => Some(Token::Break),
        b"else" => Some(Token::Else),
        b"false" => Some(Token::BoolLiteral(false)),
        b"for" => Some(Token::For),
        b"if" => Some(Token::If),
        b"let" => Some(Token::Let),
        b"return" => Some(Token::Return),
        b"while" => Some(Token::While),
        b"true" => Some(Token::BoolLiteral(true)),
        w => match w.iter().next() {
            Some(b'0'...b'9') => None,
            _ => Some(Token::Identifier(word.0)),
        },
    }
}

named!(keyword_or_identifier<CompleteByteSlice, Token>,
    do_parse!(
        id: map_opt!(take_while1!(|c: u8| is_alphanumeric(c) || c == b'_'), parse_reserved) >>
        (id)
    )
);

named!(
    op_equal<CompleteByteSlice, Token>,
    do_parse!(tag!("==") >> (Token::Equal))
);
named!(
    op_assign<CompleteByteSlice, Token>,
    do_parse!(tag!("=") >> (Token::Assign))
);
named!(
    op_plus<CompleteByteSlice, Token>,
    do_parse!(tag!("+") >> (Token::Plus))
);
named!(
    op_minus<CompleteByteSlice, Token>,
    do_parse!(tag!("-") >> (Token::Minus))
);
named!(
    op_exclamationmark<CompleteByteSlice, Token>,
    do_parse!(tag!("!") >> (Token::ExclamationMark))
);
named!(
    op_slash<CompleteByteSlice, Token>,
    do_parse!(tag!("/") >> (Token::Slash))
);
named!(
    op_star<CompleteByteSlice, Token>,
    do_parse!(tag!("*") >> (Token::Star))
);
named!(
    op_notequal<CompleteByteSlice, Token>,
    do_parse!(tag!("!=") >> (Token::NotEqual))
);
named!(
    op_greaterthanequal<CompleteByteSlice, Token>,
    do_parse!(tag!(">=") >> (Token::GreaterThanEqual))
);
named!(
    op_greaterthan<CompleteByteSlice, Token>,
    do_parse!(tag!(">") >> (Token::GreaterThan))
);
named!(
    op_lessthanequal<CompleteByteSlice, Token>,
    do_parse!(tag!("<=") >> (Token::LessThanEqual))
);
named!(
    op_lessthan<CompleteByteSlice, Token>,
    do_parse!(tag!("<") >> (Token::LessThan))
);
named!(
    op_comma<CompleteByteSlice, Token>,
    do_parse!(tag!(",") >> (Token::Comma))
);
named!(
    op_colon<CompleteByteSlice, Token>,
    do_parse!(tag!(":") >> (Token::Colon))
);
named!(
    op_semicolon<CompleteByteSlice, Token>,
    do_parse!(tag!(";") >> (Token::SemiColon))
);
named!(
    op_leftroundbracket<CompleteByteSlice, Token>,
    do_parse!(tag!("(") >> (Token::LeftRoundBracket))
);
named!(
    op_rightroundbracket<CompleteByteSlice, Token>,
    do_parse!(tag!(")") >> (Token::RightRoundBracket))
);
named!(
    op_leftcurlybracket<CompleteByteSlice, Token>,
    do_parse!(tag!("{") >> (Token::LeftCurlyBracket))
);
named!(
    op_rightcurlybracket<CompleteByteSlice, Token>,
    do_parse!(tag!("}") >> (Token::RightCurlyBracket))
);
named!(
    op_leftsquarebracket<CompleteByteSlice, Token>,
    do_parse!(tag!("[") >> (Token::LeftSquareBracket))
);
named!(
    op_rightsquarebracket<CompleteByteSlice, Token>,
    do_parse!(tag!("]") >> (Token::RightSquareBracket))
);

named!(
    op<CompleteByteSlice, Token>,
    alt!(
        op_equal |
        op_assign |
        op_plus |
        op_minus |
        op_exclamationmark |
        op_slash |
        op_star |
        op_notequal |
        op_greaterthanequal |
        op_greaterthan |
        op_lessthanequal |
        op_lessthan |
        op_comma |
        op_colon |
        op_semicolon |
        op_leftroundbracket |
        op_rightroundbracket |
        op_leftcurlybracket |
        op_rightcurlybracket |
        op_leftsquarebracket |
        op_rightsquarebracket
    )
);

fn is_alphanumeric(ch: u8) -> bool {
    (ch >= b'A' && ch <= b'Z') || (ch >= b'a' && ch <= b'z') || (ch >= b'0' && ch <= b'9')
}

named!(
    hex_literal<CompleteByteSlice, Token>,
    do_parse!(
        tag!("0x") >>
        num: map_res!(take_while1!(|c: u8| is_alphanumeric(c) || c == b'_'), |s| convert_digits(s, 16)) >>
        (Token::HexIntLiteral(num))
    )
);

named!(
    dec_literal<CompleteByteSlice, Token>,
    do_parse!(
        neg: opt!(tag!("-")) >>
        num: map_res!(take_while1!(|c: u8| is_alphanumeric(c) || c == b'_'), |s| convert_digits(s, 10)) >>
        (Token::DecimalIntLiteral(if neg.is_some() { -num } else { num }))
    )
);

// For some reason this parser causes nom to enter an infinite loop.

// named!(
//     string_content<CompleteByteSlice, CompleteByteSlice>,
//     escaped!(take_until_either!("\"\\"), '\\', one_of!("\"\\"))
// );

// named!(
//     string_literal<CompleteByteSlice, Token>,
//     do_parse!(
//         s: delimited!(tag!("\""), string_content, tag!("\"")) >>
//         (Token::StringLiteral(s.0))
//     )
// );

fn string_literal(input: CompleteByteSlice) -> nom::IResult<CompleteByteSlice, Token, u32> {
    let mut bytes = input.0.iter();
    let mut byte_count: usize = 0;
    let mut escaped = false;
    if let Some(b'"') = bytes.next() {
        loop {
            match bytes.next() {
                Some(b'\\') => {
                    escaped = true;
                    byte_count += 1;
                }
                Some(b'"') if !escaped => {
                    break
                }
                Some(_c) => {
                    escaped = false;
                    byte_count += 1;
                }
                None => {
                    // Ran out of text
                    return Err(nom::Err::Error(error_position!(input, nom::ErrorKind::OneOf)));
                }
            }
        }
        let contents = &input.0[1 .. byte_count + 1];
        let remainder = &input.0[byte_count + 2 .. input.0.len()];
        Ok((CompleteByteSlice(remainder), Token::StringLiteral(contents)))
    } else {
        Err(nom::Err::Error(error_position!(input, nom::ErrorKind::OneOf)))
    }
}

named!(parse_token<CompleteByteSlice, Token>,
    alt!(
        hex_literal |
        dec_literal |
        string_literal |
        op |
        keyword_or_identifier
    )
);

/// Like i64::from_str_radix but ignores underscores.
fn convert_digits<'a>(input: CompleteByteSlice<'a>, radix: u32) -> Result<i64, Error<'a>> {
    let mut result = 0i64;
    let mut valid = false;

    for ch in input.0.iter() {
        match ch {
            b'_' => {
                // ignore underscores except at the start
                if !valid {
                    return Err(Error::BadNumber(input.0));
                }
            }
            _ => {
                result *= i64::from(radix);
                result += (*ch as char).to_digit(radix).ok_or(Error::BadNumber(input.0))? as i64;
            }
        }
        valid = true;
    }
    if valid {
        Ok(result)
    } else {
        Err(Error::BadNumber(input.0))
    }
}

impl Lexer {
    /// Take a complete function and produce an interator over the resulting tokens.
    pub fn iterate<'a>(input: &'a [u8]) -> TokenIterator<'a> {
        TokenIterator { input }
    }

    /// Convert a complete function into a token stream.
    pub fn lex_tokens<'a>(mut input: &'a [u8]) -> Result<(Token, &'a [u8]), Error<'a>> {
        // Trim off leading whitespace
        loop {
            match input.get(0) {
                Some(b' ') | Some(b'\n') | Some(b'\t') | Some(b'\r') => input = &input[1..],
                _ => break,
            }
        }
        if input.len() == 0 {
            Ok((Token::EOF, b""))
        } else if let Ok((res, tok)) = parse_token(CompleteByteSlice(input)) {
            Ok((tok, res.0))
        } else {
            Err(Error::SyntaxError(input))
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;


    #[test]
    fn bool_literal() {
        assert_eq!(
            Lexer::lex_tokens(&b" false "[..]),
            Ok((Token::BoolLiteral(false), &b" "[..]))
        );
        assert_eq!(
            Lexer::lex_tokens(&b" true "[..]),
            Ok((Token::BoolLiteral(true), &b" "[..]))
        );
    }

    #[test]
    fn int_literal() {
        assert_eq!(
            Lexer::lex_tokens(&b"123"[..]),
            Ok((Token::DecimalIntLiteral(123), &b""[..]))
        );
        assert_eq!(
            Lexer::lex_tokens(&b" 123"[..]),
            Ok((Token::DecimalIntLiteral(123), &b""[..]))
        );
        assert_eq!(
            Lexer::lex_tokens(&b"123 "[..]),
            Ok((Token::DecimalIntLiteral(123), &b" "[..]))
        );
        assert_eq!(
            Lexer::lex_tokens(&b" 123 "[..]),
            Ok((Token::DecimalIntLiteral(123), &b" "[..]))
        );
        assert_eq!(
            Lexer::lex_tokens(&b"0x100"[..]),
            Ok((Token::HexIntLiteral(256), &b""[..]))
        );
        assert_eq!(
            Lexer::lex_tokens(&b"0x8000_0000"[..]),
            Ok((Token::HexIntLiteral(1 << 31), &b""[..]))
        );
        assert_eq!(
            Lexer::lex_tokens(&b"-567"[..]),
            Ok((Token::DecimalIntLiteral(-567), &b""[..]))
        );
    }

    #[test]
    fn keywords() {
        assert_eq!(Lexer::lex_tokens(&b" if"[..]), Ok((Token::If, &b""[..])));
        assert_eq!(Lexer::lex_tokens(&b"if#"[..]), Ok((Token::If, &b"#"[..])));
        assert_eq!(Lexer::lex_tokens(&b"let"[..]), Ok((Token::Let, &b""[..])));
        assert_eq!(Lexer::lex_tokens(&b"return"[..]), Ok((Token::Return, &b""[..])));
        assert_eq!(Lexer::lex_tokens(&b"while"[..]), Ok((Token::While, &b""[..])));
        assert_eq!(Lexer::lex_tokens(&b"for"[..]), Ok((Token::For, &b""[..])));
        assert_eq!(Lexer::lex_tokens(&b"break"[..]), Ok((Token::Break, &b""[..])));
    }

    #[test]
    fn identifiers() {
        assert_eq!(Lexer::lex_tokens(&b"x"[..]), Ok((Token::Identifier(&b"x"[..]), &b""[..])));
        assert_eq!(
            Lexer::lex_tokens(&b"lets"[..]),
            Ok((Token::Identifier(&b"lets"[..]), &b""[..]))
        );
        assert_eq!(
            Lexer::lex_tokens(&b"x==123"[..]),
            Ok((Token::Identifier(&b"x"[..]), &b"==123"[..]))
        );
        assert_eq!(Lexer::lex_tokens(&b"Abc"[..]), Ok((Token::Identifier(&b"Abc"[..]), &b""[..])));
        assert_eq!(
            Lexer::lex_tokens(&b"Abc0"[..]),
            Ok((Token::Identifier(&b"Abc0"[..]), &b""[..]))
        );
        assert_eq!(
            Lexer::lex_tokens(&b"a_bc0"[..]),
            Ok((Token::Identifier(&b"a_bc0"[..]), &b""[..]))
        );
        assert_eq!(
            Lexer::lex_tokens(&b"_abc"[..]),
            Ok((Token::Identifier(&b"_abc"[..]), &b""[..]))
        );
        assert!(Lexer::lex_tokens(&b"0abc"[..]).is_err());
    }

    #[test]
    fn strings() {
        assert_eq!(
            Lexer::lex_tokens(&b"\"test\""[..]),
            Ok((Token::StringLiteral(b"test"), &b""[..]))
        );
        assert_eq!(
            Lexer::lex_tokens(&b" \"test\""[..]),
            Ok((Token::StringLiteral(b"test"), &b""[..]))
        );
        assert_eq!(
            Lexer::lex_tokens(&b"\"test\" "[..]),
            Ok((Token::StringLiteral(b"test"), &b" "[..]))
        );
        // The lexer doesn't re-write strings to remove the escapes. That
        // would require memory allocation.
        assert_eq!(
            Lexer::lex_tokens(&b"\"te\\\"st \" "[..]),
            Ok((Token::StringLiteral(b"te\\\"st "), &b" "[..]))
        );
    }

    #[test]
    fn complicated_source() {
        let source = br#"
let x = 123;
let y = foo(x);
if y > x {
    baz(true);
} else {
    bar(false);
}
return 0x200;
        "#;
        let expected_tokens = [
            Token::Let,
            Token::Identifier(b"x"),
            Token::Assign,
            Token::DecimalIntLiteral(123),
            Token::SemiColon,
            Token::Let,
            Token::Identifier(b"y"),
            Token::Assign,
            Token::Identifier(b"foo"),
            Token::LeftRoundBracket,
            Token::Identifier(b"x"),
            Token::RightRoundBracket,
            Token::SemiColon,
            Token::If,
            Token::Identifier(b"y"),
            Token::GreaterThan,
            Token::Identifier(b"x"),
            Token::LeftCurlyBracket,
            Token::Identifier(b"baz"),
            Token::LeftRoundBracket,
            Token::BoolLiteral(true),
            Token::RightRoundBracket,
            Token::SemiColon,
            Token::RightCurlyBracket,
            Token::Else,
            Token::LeftCurlyBracket,
            Token::Identifier(b"bar"),
            Token::LeftRoundBracket,
            Token::BoolLiteral(false),
            Token::RightRoundBracket,
            Token::SemiColon,
            Token::RightCurlyBracket,
            Token::Return,
            Token::HexIntLiteral(512),
            Token::SemiColon,
            Token::EOF,
        ];
        let mut buffer = &source[..];
        for expected_token in expected_tokens.iter() {
            let (token, remainder) = Lexer::lex_tokens(buffer).expect("Error parsing tokens");
            buffer = remainder;
            assert_eq!(token, *expected_token);
        }
        assert_eq!(buffer.len(), 0);

        let tokens: Result<Vec<Token>, Error<'_>> = Lexer::iterate(source).collect();
        let tokens = tokens.unwrap();
        assert_eq!(&tokens[..], &expected_tokens[..]);
    }
}
