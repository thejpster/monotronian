//! This is a lexer for the monotronian language.
//!
//! It can handle the keywords, typical punctuation for a curly bracket
//! language, integers, identifiers and strings. It's like C, but using the
//! `let` keyword, and without any type information.
//!
//! There's also no concept of declaring a function because each function is
//! handled as a stand-alone unit to save memory.

pub mod token;

use nom::types::CompleteByteSlice;
pub use self::token::{Token, Operator};
pub use self::token::ByteString;
use super::display_ascii_string;

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
                display_ascii_string(e, fmt)?;
                write!(fmt, "\"")
            }
            Error::BadNumber(e) => {
                write!(fmt, "BadNumber at \"")?;
                display_ascii_string(e, fmt)?;
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

fn lower(byte_char: u8) -> u8 {
    if byte_char >= b'A' && byte_char <= b'Z' {
        (byte_char - b'A') + b'a'
    } else {
        byte_char
    }
}

/// Checks if `token` is equal to `compare_to`, ignoring case differences for
/// ASCII letters 'A' through 'Z'. The `compare_to` argument should be in
/// lower-case.
fn check_case_insensitive(token: &[u8], compare_to: &[u8]) -> bool {
    if token.len() != compare_to.len() {
        false
    } else {
        token
            .iter()
            .map(|c| lower(*c))
            .zip(compare_to.iter())
            .all(|(a, b)| a == *b)
    }
}

/// Check if a string is a reserved word or an identifier.
fn parse_reserved(word: CompleteByteSlice) -> Option<Token> {
    if check_case_insensitive(word.0, b"true") {
        return Some(Token::BoolLiteral(true));
    }
    if check_case_insensitive(word.0, b"break") {
        return Some(Token::Break);
    }
    if check_case_insensitive(word.0, b"case") {
        return Some(Token::Case);
    }
    if check_case_insensitive(word.0, b"next") {
        return Some(Token::Next);
    }
    if check_case_insensitive(word.0, b"global") {
        return Some(Token::Global);
    }
    if check_case_insensitive(word.0, b"else") {
        return Some(Token::Else);
    }
    if check_case_insensitive(word.0, b"elif") {
        return Some(Token::ElseIf);
    }
    if check_case_insensitive(word.0, b"endcase") {
        return Some(Token::EndCase);
    }
    if check_case_insensitive(word.0, b"endfn") {
        return Some(Token::EndFunc);
    }
    if check_case_insensitive(word.0, b"endif") {
        return Some(Token::EndIf);
    }
    if check_case_insensitive(word.0, b"endwhile") {
        return Some(Token::EndWhile);
    }
    if check_case_insensitive(word.0, b"false") {
        return Some(Token::BoolLiteral(false));
    }
    if check_case_insensitive(word.0, b"for") {
        return Some(Token::For);
    }
    if check_case_insensitive(word.0, b"fn") {
        return Some(Token::Func);
    }
    if check_case_insensitive(word.0, b"if") {
        return Some(Token::If);
    }
    if check_case_insensitive(word.0, b"step") {
        return Some(Token::Step);
    }
    if check_case_insensitive(word.0, b"then") {
        return Some(Token::Then);
    }
    if check_case_insensitive(word.0, b"to") {
        return Some(Token::To);
    }
    if check_case_insensitive(word.0, b"return") {
        return Some(Token::Return);
    }
    if check_case_insensitive(word.0, b"until") {
        return Some(Token::Until);
    }
    if check_case_insensitive(word.0, b"while") {
        return Some(Token::While);
    }
    let mut i = word.0.iter();
    let mut first = i.next();
    while let Some(b'_') = first {
        first = i.next();
    }
    match first {
        Some(b'0'...b'9') => None,
        _ => {
            // Identifiers can't have these special characters
            if word.0.iter().any(|x| *x == b'$' || *x == b'#') {
                None
            } else {
                Some(Token::Identifier(ByteString(word.0)))
            }
        }
    }
}

named!(keyword_or_identifier<CompleteByteSlice, Token>,
    do_parse!(
        id: map_opt!(take_while1!(|c: u8| is_alphanumeric(c) || c == b'_' || c == b'$' || c == b'#'), parse_reserved) >>
        (id)
    )
);

named!(
    op_assign<CompleteByteSlice, Token>,
    do_parse!(tag!("=") >> (Token::Assign))
);

named!(
    op_equals<CompleteByteSlice, Token>,
    do_parse!(tag!("==") >> (Token::Operator(Operator::Equals)))
);

named!(
    op_plus<CompleteByteSlice, Token>,
    do_parse!(tag!("+") >> (Token::Operator(Operator::Plus)))
);

named!(
    op_minus<CompleteByteSlice, Token>,
    do_parse!(tag!("-") >> (Token::Operator(Operator::Minus)))
);

named!(
    op_slash<CompleteByteSlice, Token>,
    do_parse!(tag!("/") >> (Token::Operator(Operator::Slash)))
);

named!(
    op_star<CompleteByteSlice, Token>,
    do_parse!(tag!("*") >> (Token::Operator(Operator::Star)))
);

named!(
    op_notequal<CompleteByteSlice, Token>,
    do_parse!(tag!("!=") >> (Token::Operator(Operator::NotEqual)))
);

named!(
    op_or<CompleteByteSlice, Token>,
    do_parse!(tag!("||") >> (Token::Operator(Operator::Or)))
);

named!(
    op_and<CompleteByteSlice, Token>,
    do_parse!(tag!("&&") >> (Token::Operator(Operator::And)))
);

named!(
    op_bitxor<CompleteByteSlice, Token>,
    do_parse!(tag!("^") >> (Token::Operator(Operator::BitXor)))
);

named!(
    op_bitand<CompleteByteSlice, Token>,
    do_parse!(tag!("&") >> (Token::Operator(Operator::BitAnd)))
);

named!(
    op_bitor<CompleteByteSlice, Token>,
    do_parse!(tag!("|") >> (Token::Operator(Operator::BitOr)))
);

named!(
    op_not<CompleteByteSlice, Token>,
    do_parse!(tag!("!") >> (Token::Operator(Operator::Not)))
);

named!(
    op_greaterthanequal<CompleteByteSlice, Token>,
    do_parse!(tag!(">=") >> (Token::Operator(Operator::GreaterThanEqual)))
);

named!(
    op_greaterthan<CompleteByteSlice, Token>,
    do_parse!(tag!(">") >> (Token::Operator(Operator::GreaterThan)))
);

named!(
    op_lessthanequal<CompleteByteSlice, Token>,
    do_parse!(tag!("<=") >> (Token::Operator(Operator::LessThanEqual)))
);

named!(
    op_lessthan<CompleteByteSlice, Token>,
    do_parse!(tag!("<") >> (Token::Operator(Operator::LessThan)))
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
    op<CompleteByteSlice, Token>,
    alt!(
        op_equals |
        op_assign |
        op_or |
        op_and |
        op_plus |
        op_minus |
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
        op_not |
        op_bitand |
        op_bitor |
        op_bitxor
    )
);

named!(
    new_line<CompleteByteSlice, Token>,
    do_parse!(tag!("\n") >> (Token::NewLine))
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
    dec_float_literal<CompleteByteSlice, Token>,
    do_parse!(
        neg: opt!(tag!("-")) >>
        num: map_res!(take_while1!(|c: u8| is_alphanumeric(c) || c == b'_' || c == b'.'), |s| convert_float_digits(s)) >>
        (Token::DecimalFloatLiteral(if neg.is_some() { -num } else { num }))
    )
);

named!(
    dec_literal<CompleteByteSlice, Token>,
    do_parse!(
        neg: opt!(tag!("-")) >>
        num: map_res!(take_while1!(|c: u8| is_alphanumeric(c) || c == b'_' || c == b'.'), |s| convert_digits(s, 10)) >>
        (Token::DecimalIntLiteral(if neg.is_some() { -num } else { num }))
    )
);

// For some reason this parser causes nom to enter an infinite loop.

named!(
    string_content<CompleteByteSlice, CompleteByteSlice>,
    escaped!(none_of!("\"\\"), '\\', one_of!("\"\\"))
);

named!(
    string_literal<CompleteByteSlice, Token>,
    do_parse!(
        s: delimited!(tag!("\""), string_content, tag!("\"")) >>
        (Token::StringLiteral(ByteString(s.0)))
    )
);

named!(parse_token<CompleteByteSlice, Token>,
    alt!(
        new_line |
        hex_literal |
        dec_literal |
        dec_float_literal |
        string_literal |
        op |
        keyword_or_identifier
    )
);


/// Like f64::from_str but ignores underscores.
fn convert_float_digits<'a>(input: CompleteByteSlice<'a>) -> Result<f64, Error<'a>> {
    let mut iresult = 0i64;
    let mut result = 0f64;
    let mut valid = false;
    let mut seen_dot = false;
    let mut frac = 1f64;

    for ch in input.0.iter() {
        match ch {
            b'_' => {
                // ignore underscores except at the start
                if !valid {
                    return Err(Error::BadNumber(input.0));
                }
            }
            b'.' => {
                if !valid {
                    return Err(Error::BadNumber(input.0));
                }
                seen_dot = true;
                result = iresult as f64;
            }
            _ => {
                if seen_dot {
                    frac = frac / 10.0;
                    let digit = (*ch as char).to_digit(10).ok_or(Error::BadNumber(input.0))? as f64;
                    result += digit * frac;
                } else {
                    iresult *= i64::from(10);
                    iresult += (*ch as char)
                        .to_digit(10)
                        .ok_or(Error::BadNumber(input.0))? as i64;
                }
            }
        }
        valid = true;
    }
    if valid && seen_dot {
        Ok(result)
    } else {
        Err(Error::BadNumber(input.0))
    }
}

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
            b'.' => {
                return Err(Error::BadNumber(input.0))
            }
            _ => {
                result *= i64::from(radix);
                result += (*ch as char)
                    .to_digit(radix)
                    .ok_or(Error::BadNumber(input.0))? as i64;
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
                Some(b' ') | Some(b'\t') => input = &input[1..],
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
    fn float_literal() {
        assert_eq!(
            Lexer::lex_tokens(&b"1.0"[..]),
            Ok((Token::DecimalFloatLiteral(1.0), &b""[..]))
        );
        assert_eq!(
            Lexer::lex_tokens(&b"1_000.0"[..]),
            Ok((Token::DecimalFloatLiteral(1_000.0), &b""[..]))
        );
        assert!(Lexer::lex_tokens(&b"_1_000.0"[..]).is_err());
    }

    #[test]
    fn int_literal() {
        assert_eq!(
            Lexer::lex_tokens(&b"123"[..]),
            Ok((Token::DecimalIntLiteral(123), &b""[..]))
        );
        assert_eq!(
            Lexer::lex_tokens(&b"1_23"[..]),
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
            Lexer::lex_tokens(&b"0x1_00"[..]),
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

    fn check_keyword(source: &[u8], token: Token) {
        let got = Lexer::lex_tokens(source).unwrap();
        if got.0 != token {
            panic!("Got {:?}, expected {:?} in {:?}", got, token, ::std::str::from_utf8(source));
        }
        if got.1 != &b""[..] {
            panic!("Got {:?} expected nothing", got.1);
        }
    }

    #[test]
    fn keywords() {
        check_keyword(b"=", Token::Assign);
        check_keyword(b"False", Token::BoolLiteral(false));
        check_keyword(b"True", Token::BoolLiteral(true));
        check_keyword(b"Break", Token::Break);
        check_keyword(b"Case", Token::Case);
        check_keyword(b",", Token::Comma);
        check_keyword(b"1.0", Token::DecimalFloatLiteral(1.0));
        check_keyword(b"1_000.0", Token::DecimalFloatLiteral(1_000.0));
        check_keyword(b"123", Token::DecimalIntLiteral(123));
        check_keyword(b"1_000", Token::DecimalIntLiteral(1_000));
        check_keyword(b"Else", Token::Else);
        check_keyword(b"ElIf", Token::ElseIf);
        check_keyword(b"EndCase", Token::EndCase);
        check_keyword(b"EndFn", Token::EndFunc);
        check_keyword(b"EndIf", Token::EndIf);
        check_keyword(b"EndWhile", Token::EndWhile);
        check_keyword(b"For", Token::For);
        check_keyword(b"Fn", Token::Func);
        check_keyword(b"Global", Token::Global);
        check_keyword(b"0x100", Token::HexIntLiteral(0x100));
        check_keyword(b"0x1_00", Token::HexIntLiteral(0x100));
        check_keyword(b"sin", Token::Identifier(ByteString(b"sin")));
        check_keyword(b"X", Token::Identifier(ByteString(b"X")));
        check_keyword(b"If", Token::If);
        check_keyword(b"(", Token::LeftRoundBracket);
        check_keyword(b"\n", Token::NewLine);
        check_keyword(b"Next", Token::Next);
        check_keyword(b"&&", Token::Operator(Operator::And));
        check_keyword(b"&", Token::Operator(Operator::BitAnd));
        check_keyword(b"|", Token::Operator(Operator::BitOr));
        check_keyword(b"^", Token::Operator(Operator::BitXor));
        check_keyword(b"==", Token::Operator(Operator::Equals));
        check_keyword(b">", Token::Operator(Operator::GreaterThan));
        check_keyword(b">=", Token::Operator(Operator::GreaterThanEqual));
        check_keyword(b"<", Token::Operator(Operator::LessThan));
        check_keyword(b"<=", Token::Operator(Operator::LessThanEqual));
        check_keyword(b"-", Token::Operator(Operator::Minus));
        check_keyword(b"!", Token::Operator(Operator::Not));
        check_keyword(b"!=", Token::Operator(Operator::NotEqual));
        check_keyword(b"||", Token::Operator(Operator::Or));
        check_keyword(b"||", Token::Operator(Operator::Or));
        check_keyword(b"/", Token::Operator(Operator::Slash));
        check_keyword(b"Return", Token::Return);
        check_keyword(b")", Token::RightRoundBracket);
        check_keyword(b";", Token::SemiColon);
        check_keyword(b"Step", Token::Step);
        check_keyword(b"\"X\"", Token::StringLiteral(ByteString(b"X")));
        check_keyword(b"Then", Token::Then);
        check_keyword(b"To", Token::To);
        check_keyword(b"Until", Token::Until);
        check_keyword(b"While", Token::While);
    }

    #[test]
    fn identifiers() {
        assert_eq!(
            Lexer::lex_tokens(&b"x"[..]),
            Ok((Token::Identifier(ByteString(&b"x"[..])), &b""[..]))
        );
        assert_eq!(
            Lexer::lex_tokens(&b"lets"[..]),
            Ok((Token::Identifier(ByteString(&b"lets"[..])), &b""[..]))
        );
        assert_eq!(
            Lexer::lex_tokens(&b"x==123"[..]),
            Ok((Token::Identifier(ByteString(&b"x"[..])), &b"==123"[..]))
        );
        assert_eq!(
            Lexer::lex_tokens(&b"Abc"[..]),
            Ok((Token::Identifier(ByteString(&b"Abc"[..])), &b""[..]))
        );
        assert_eq!(
            Lexer::lex_tokens(&b"Abc0"[..]),
            Ok((Token::Identifier(ByteString(&b"Abc0"[..])), &b""[..]))
        );
        assert_eq!(
            Lexer::lex_tokens(&b"a_bc0"[..]),
            Ok((Token::Identifier(ByteString(&b"a_bc0"[..])), &b""[..]))
        );
        assert_eq!(
            Lexer::lex_tokens(&b"_abc"[..]),
            Ok((Token::Identifier(ByteString(&b"_abc"[..])), &b""[..]))
        );
        assert!(Lexer::lex_tokens(&b"0abc"[..]).is_err());
    }

    #[test]
    fn strings() {
        assert_eq!(
            Lexer::lex_tokens(&b"\"test\""[..]),
            Ok((Token::StringLiteral(ByteString(b"test")), &b""[..]))
        );
        assert_eq!(
            Lexer::lex_tokens(&b" \"test\""[..]),
            Ok((Token::StringLiteral(ByteString(b"test")), &b""[..]))
        );
        assert_eq!(
            Lexer::lex_tokens(&b"\"test\" "[..]),
            Ok((Token::StringLiteral(ByteString(b"test")), &b" "[..]))
        );
        // The lexer doesn't re-write strings to remove the escapes. That
        // would require memory allocation.
        assert_eq!(
            Lexer::lex_tokens(&b"\"te\\\"st \" "[..]),
            Ok((Token::StringLiteral(ByteString(b"te\\\"st ")), &b" "[..]))
        );
    }

    fn compare(source: &[u8], expected_tokens: &[Token]) {
        let mut buffer = &source[..];
        for (idx, expected_token) in expected_tokens.iter().enumerate() {
            let (token, remainder) = Lexer::lex_tokens(buffer).expect(&format!(
                "Error parsing tokens: expected {:?} in {:?}",
                expected_token, buffer
            ));
            if token != *expected_token {
                panic!(
                    "Token {} is {:?}, but expected {:?} in {:?}",
                    idx, token, *expected_token, ::std::str::from_utf8(buffer)
                );
            }
            buffer = remainder;
        }
        assert_eq!(buffer.len(), 0);

        let tokens: Result<Vec<Token>, Error<'_>> = Lexer::iterate(source).collect();
        let tokens = tokens.unwrap();
        assert_eq!(&tokens[..], &expected_tokens[..]);
    }

    #[test]
    fn complicated_source() {
        let source = br#"x = 123
y = foo(x)
a = input()
if y > x then
    baz(true)
else
    bar(false)
endif
return 0x200
        "#;
        let expected_tokens = [
            Token::Identifier(ByteString(b"x")),
            Token::Assign,
            Token::DecimalIntLiteral(123),
            Token::NewLine,
            Token::Identifier(ByteString(b"y")),
            Token::Assign,
            Token::Identifier(ByteString(b"foo")),
            Token::LeftRoundBracket,
            Token::Identifier(ByteString(b"x")),
            Token::RightRoundBracket,
            Token::NewLine,
            Token::Identifier(ByteString(b"a")),
            Token::Assign,
            Token::Identifier(ByteString(b"input")),
            Token::LeftRoundBracket,
            Token::RightRoundBracket,
            Token::NewLine,
            Token::If,
            Token::Identifier(ByteString(b"y")),
            Token::Operator(Operator::GreaterThan),
            Token::Identifier(ByteString(b"x")),
            Token::Then,
            Token::NewLine,
            Token::Identifier(ByteString(b"baz")),
            Token::LeftRoundBracket,
            Token::BoolLiteral(true),
            Token::RightRoundBracket,
            Token::NewLine,
            Token::Else,
            Token::NewLine,
            Token::Identifier(ByteString(b"bar")),
            Token::LeftRoundBracket,
            Token::BoolLiteral(false),
            Token::RightRoundBracket,
            Token::NewLine,
            Token::EndIf,
            Token::NewLine,
            Token::Return,
            Token::HexIntLiteral(0x200),
            Token::NewLine,
            Token::EOF,
        ];
        compare(source, &expected_tokens);
    }
}

// End of file
