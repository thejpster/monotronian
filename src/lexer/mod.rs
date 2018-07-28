//! This is a lexer for the monotronian language.
//!
//! It can handle the keywords, typical punctuation for a curly bracket
//! language, integers, identifiers and strings. It's like C, but using the
//! `let` keyword, and without any type information.
//!
//! There's also no concept of declaring a function because each function is
//! handled as a stand-alone unit to save memory.

use nom::types::CompleteByteSlice;

pub use self::token::Token;
use super::display_ascii_string;
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
    if check_case_insensitive(word.0, b"as") {
        return Some(Token::As);
    }
    if check_case_insensitive(word.0, b"break") {
        return Some(Token::Break);
    }
    if check_case_insensitive(word.0, b"dim") {
        return Some(Token::Dim);
    }
    if check_case_insensitive(word.0, b"else") {
        return Some(Token::Else);
    }
    if check_case_insensitive(word.0, b"elseif") {
        return Some(Token::ElseIf);
    }
    if check_case_insensitive(word.0, b"end") {
        return Some(Token::End);
    }
    if check_case_insensitive(word.0, b"endif") {
        return Some(Token::EndIf);
    }
    if check_case_insensitive(word.0, b"false") {
        return Some(Token::BoolLiteral(false));
    }
    if check_case_insensitive(word.0, b"for") {
        return Some(Token::For);
    }
    if check_case_insensitive(word.0, b"gosub") {
        return Some(Token::Gosub);
    }
    if check_case_insensitive(word.0, b"goto") {
        return Some(Token::Goto);
    }
    if check_case_insensitive(word.0, b"if") {
        return Some(Token::If);
    }
    if check_case_insensitive(word.0, b"input") {
        return Some(Token::Input);
    }
    if check_case_insensitive(word.0, b"integer") {
        return Some(Token::Integer);
    }
    if check_case_insensitive(word.0, b"let") {
        return Some(Token::Let);
    }
    if check_case_insensitive(word.0, b"next") {
        return Some(Token::Next);
    }
    if check_case_insensitive(word.0, b"print") {
        return Some(Token::Print);
    }
    if check_case_insensitive(word.0, b"return") {
        return Some(Token::Return);
    }
    if check_case_insensitive(word.0, b"then") {
        return Some(Token::Then);
    }
    if check_case_insensitive(word.0, b"true") {
        return Some(Token::BoolLiteral(true));
    }
    match word.0.iter().next() {
        Some(b'0'...b'9') => None,
        _ => Some(Token::Identifier(word.0)),
    }
}

/// Check if a string is a reserved word or an identifier.
fn parse_reserved_inverse(word: CompleteByteSlice) -> Option<&[u8]> {
    if check_case_insensitive(word.0, b"as") {
        return None;
    }
    if check_case_insensitive(word.0, b"break") {
        return None;
    }
    if check_case_insensitive(word.0, b"dim") {
        return None;
    }
    if check_case_insensitive(word.0, b"else") {
        return None;
    }
    if check_case_insensitive(word.0, b"elseif") {
        return None;
    }
    if check_case_insensitive(word.0, b"end") {
        return None;
    }
    if check_case_insensitive(word.0, b"endif") {
        return None;
    }
    if check_case_insensitive(word.0, b"false") {
        return None;
    }
    if check_case_insensitive(word.0, b"for") {
        return None;
    }
    if check_case_insensitive(word.0, b"gosub") {
        return None;
    }
    if check_case_insensitive(word.0, b"goto") {
        return None;
    }
    if check_case_insensitive(word.0, b"if") {
        return None;
    }
    if check_case_insensitive(word.0, b"input") {
        return None;
    }
    if check_case_insensitive(word.0, b"integer") {
        return None;
    }
    if check_case_insensitive(word.0, b"let") {
        return None;
    }
    if check_case_insensitive(word.0, b"next") {
        return None;
    }
    if check_case_insensitive(word.0, b"print") {
        return None;
    }
    if check_case_insensitive(word.0, b"return") {
        return None;
    }
    if check_case_insensitive(word.0, b"then") {
        return None;
    }
    if check_case_insensitive(word.0, b"true") {
        return None;
    }
    match word.0.iter().next() {
        Some(b'0'...b'9') => None,
        _ => Some(word.0),
    }
}

named!(keyword_or_identifier<CompleteByteSlice, Token>,
    do_parse!(
        id: map_opt!(take_while1!(|c: u8| is_alphanumeric(c) || c == b'_'), parse_reserved) >>
        (id)
    )
);

named!(
    string_identifier<CompleteByteSlice, Token>,
    do_parse!(
        id: map_opt!(take_while1!(|c: u8| is_alphanumeric(c) || c == b'_'), parse_reserved_inverse) >>
        tag!("$") >>
        (Token::StringIdentifier(id))
    )
);

named!(
    integer_identifier<CompleteByteSlice, Token>,
    do_parse!(
        id: map_opt!(take_while1!(|c: u8| is_alphanumeric(c) || c == b'_'), parse_reserved_inverse) >>
        tag!("%") >>
        (Token::IntegerIdentifier(id))
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
        op_rightroundbracket
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
    dec_literal<CompleteByteSlice, Token>,
    do_parse!(
        neg: opt!(tag!("-")) >>
        num: map_res!(take_while1!(|c: u8| is_alphanumeric(c) || c == b'_'), |s| convert_digits(s, 10)) >>
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
        (Token::StringLiteral(s.0))
    )
);

// fn string_literal(input: CompleteByteSlice) -> nom::IResult<CompleteByteSlice, Token, u32> {
//     let mut bytes = input.0.iter();
//     let mut byte_count: usize = 0;
//     let mut escaped = false;
//     if let Some(b'"') = bytes.next() {
//         loop {
//             match bytes.next() {
//                 Some(b'\\') => {
//                     escaped = true;
//                     byte_count += 1;
//                 }
//                 Some(b'"') if !escaped => {
//                     break
//                 }
//                 Some(_c) => {
//                     escaped = false;
//                     byte_count += 1;
//                 }
//                 None => {
//                     // Ran out of text
//                     return Err(nom::Err::Error(error_position!(input, nom::ErrorKind::OneOf)));
//                 }
//             }
//         }
//         let contents = &input.0[1 .. byte_count + 1];
//         let remainder = &input.0[byte_count + 2 .. input.0.len()];
//         Ok((CompleteByteSlice(remainder), Token::StringLiteral(contents)))
//     } else {
//         Err(nom::Err::Error(error_position!(input, nom::ErrorKind::OneOf)))
//     }
// }

named!(parse_token<CompleteByteSlice, Token>,
    alt!(
        new_line |
        hex_literal |
        dec_literal |
        string_literal |
        op |
        string_identifier |
        integer_identifier |
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
        assert_eq!(Lexer::lex_tokens(&b"as"[..]), Ok((Token::As, &b""[..])));
        assert_eq!(
            Lexer::lex_tokens(&b"break"[..]),
            Ok((Token::Break, &b""[..]))
        );
        assert_eq!(Lexer::lex_tokens(&b"dim"[..]), Ok((Token::Dim, &b""[..])));
        assert_eq!(Lexer::lex_tokens(&b"else"[..]), Ok((Token::Else, &b""[..])));
        assert_eq!(
            Lexer::lex_tokens(&b"elseif"[..]),
            Ok((Token::ElseIf, &b""[..]))
        );
        assert_eq!(Lexer::lex_tokens(&b"end"[..]), Ok((Token::End, &b""[..])));
        assert_eq!(
            Lexer::lex_tokens(&b"endif"[..]),
            Ok((Token::EndIf, &b""[..]))
        );
        assert_eq!(
            Lexer::lex_tokens(&b"false"[..]),
            Ok((Token::BoolLiteral(false), &b""[..]))
        );
        assert_eq!(Lexer::lex_tokens(&b"for"[..]), Ok((Token::For, &b""[..])));
        assert_eq!(
            Lexer::lex_tokens(&b"gosub"[..]),
            Ok((Token::Gosub, &b""[..]))
        );
        assert_eq!(Lexer::lex_tokens(&b"goto"[..]), Ok((Token::Goto, &b""[..])));
        assert_eq!(Lexer::lex_tokens(&b"if"[..]), Ok((Token::If, &b""[..])));
        assert_eq!(
            Lexer::lex_tokens(&b"input"[..]),
            Ok((Token::Input, &b""[..]))
        );
        assert_eq!(
            Lexer::lex_tokens(&b"integer"[..]),
            Ok((Token::Integer, &b""[..]))
        );
        assert_eq!(Lexer::lex_tokens(&b"let"[..]), Ok((Token::Let, &b""[..])));
        assert_eq!(Lexer::lex_tokens(&b"next"[..]), Ok((Token::Next, &b""[..])));
        assert_eq!(
            Lexer::lex_tokens(&b"print"[..]),
            Ok((Token::Print, &b""[..]))
        );
        assert_eq!(
            Lexer::lex_tokens(&b"return"[..]),
            Ok((Token::Return, &b""[..]))
        );
        assert_eq!(Lexer::lex_tokens(&b"then"[..]), Ok((Token::Then, &b""[..])));
        assert_eq!(
            Lexer::lex_tokens(&b"true"[..]),
            Ok((Token::BoolLiteral(true), &b""[..]))
        );
    }

    #[test]
    fn identifiers() {
        assert_eq!(
            Lexer::lex_tokens(&b"x"[..]),
            Ok((Token::Identifier(&b"x"[..]), &b""[..]))
        );
        assert_eq!(
            Lexer::lex_tokens(&b"lets"[..]),
            Ok((Token::Identifier(&b"lets"[..]), &b""[..]))
        );
        assert_eq!(
            Lexer::lex_tokens(&b"x==123"[..]),
            Ok((Token::Identifier(&b"x"[..]), &b"==123"[..]))
        );
        assert_eq!(
            Lexer::lex_tokens(&b"Abc"[..]),
            Ok((Token::Identifier(&b"Abc"[..]), &b""[..]))
        );
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

    fn compare(source: &[u8], expected_tokens: &[Token]) {
        let mut buffer = &source[..];
        for (idx, expected_token) in expected_tokens.iter().enumerate() {
            let (token, remainder) = Lexer::lex_tokens(buffer).expect(&format!(
                "Error parsing tokens: expected {:?} in {:?}",
                expected_token, buffer
            ));
            buffer = remainder;
            if token != *expected_token {
                panic!(
                    "Token {} is {:?}, but expected {:?}",
                    idx, token, *expected_token
                );
            }
        }
        assert_eq!(buffer.len(), 0);

        let tokens: Result<Vec<Token>, Error<'_>> = Lexer::iterate(source).collect();
        let tokens = tokens.unwrap();
        assert_eq!(&tokens[..], &expected_tokens[..]);
    }

    #[test]
    fn old_fashioned() {
        let source = br#"10 X%=0
20 PRINT "HELLO, WORLD"
30 GOSUB 1000
40 GOTO 10
1000 X%=X%+1
1010 RETURN
        "#;
        let expected_tokens = [
            Token::DecimalIntLiteral(10),
            Token::IntegerIdentifier(b"X"),
            Token::Assign,
            Token::DecimalIntLiteral(0),
            Token::NewLine,
            Token::DecimalIntLiteral(20),
            Token::Print,
            Token::StringLiteral(b"HELLO, WORLD"),
            Token::NewLine,
            Token::DecimalIntLiteral(30),
            Token::Gosub,
            Token::DecimalIntLiteral(1000),
            Token::NewLine,
            Token::DecimalIntLiteral(40),
            Token::Goto,
            Token::DecimalIntLiteral(10),
            Token::NewLine,
            Token::DecimalIntLiteral(1000),
            Token::IntegerIdentifier(b"X"),
            Token::Assign,
            Token::IntegerIdentifier(b"X"),
            Token::Plus,
            Token::DecimalIntLiteral(1),
            Token::NewLine,
            Token::DecimalIntLiteral(1010),
            Token::Return,
            Token::NewLine,
            Token::EOF,
        ];
        compare(source, &expected_tokens);
    }

    #[test]
    fn complicated_source() {
        let source = br#"x = 123
Dim x as Integer
y = foo(x)
input A$
if y > x then
    baz(true)
else
    bar(false)
endif
return 0x200
        "#;
        let expected_tokens = [
            Token::Identifier(b"x"),
            Token::Assign,
            Token::DecimalIntLiteral(123),
            Token::NewLine,
            Token::Dim,
            Token::Identifier(b"x"),
            Token::As,
            Token::Integer,
            Token::NewLine,
            Token::Identifier(b"y"),
            Token::Assign,
            Token::Identifier(b"foo"),
            Token::LeftRoundBracket,
            Token::Identifier(b"x"),
            Token::RightRoundBracket,
            Token::NewLine,
            Token::Input,
            Token::StringIdentifier(b"A"),
            Token::NewLine,
            Token::If,
            Token::Identifier(b"y"),
            Token::GreaterThan,
            Token::Identifier(b"x"),
            Token::Then,
            Token::NewLine,
            Token::Identifier(b"baz"),
            Token::LeftRoundBracket,
            Token::BoolLiteral(true),
            Token::RightRoundBracket,
            Token::NewLine,
            Token::Else,
            Token::NewLine,
            Token::Identifier(b"bar"),
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
