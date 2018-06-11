//! This is a lexer for the monotronian language.
//!
//! It can handle the keywords, typical punctuation for a curly bracket
//! language, integers, identifiers and strings. It's like C, but using the
//! `let` keyword, and without any type information.
//!
//! There's also no concept of declaring a function because each function is
//! handled as a stand-alone unit to save memory.

pub mod token;
pub use self::token::Token;

pub struct Lexer;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Error {
    SyntaxError,
}

enum Following {
    CharsAllowed,
    NoCharsAllowed
}

impl Lexer {
    pub fn lex_tokens(input: &str) -> Result<(Token, &str), Error> {
        let input = input.trim_left();
        if input.len() == 0 {
            Ok((Token::EOF, ""))
        } else if let Some((num, res)) = Self::read_hex(input)? {
            Ok((Token::HexIntLiteral(num), res))
        } else if let Some((num, res)) = Self::read_decimal(input)? {
            Ok((Token::DecimalIntLiteral(num), res))
        } else if let Some((string, res)) = Self::read_string(input)? {
            Ok((Token::StringLiteral(string), res))
        } else if let Some(res) = Self::matches(input, "if", Following::NoCharsAllowed) {
            Ok((Token::If, res))
        } else if let Some(res) = Self::matches(input, "else", Following::NoCharsAllowed) {
            Ok((Token::Else, res))
        } else if let Some(res) = Self::matches(input, "let", Following::NoCharsAllowed) {
            Ok((Token::Let, res))
        } else if let Some(res) = Self::matches(input, "for", Following::NoCharsAllowed) {
            Ok((Token::For, res))
        } else if let Some(res) = Self::matches(input, "while", Following::NoCharsAllowed) {
            Ok((Token::While, res))
        } else if let Some(res) = Self::matches(input, "break", Following::NoCharsAllowed) {
            Ok((Token::Break, res))
        } else if let Some(res) = Self::matches(input, "return", Following::NoCharsAllowed) {
            Ok((Token::Return, res))
        } else if let Some(res) = Self::matches(input, "==", Following::CharsAllowed) {
            Ok((Token::Equal, res))
        } else if let Some(res) = Self::matches(input, "=", Following::CharsAllowed) {
            Ok((Token::Assign, res))
        } else if let Some(res) = Self::matches(input, "+", Following::CharsAllowed) {
            Ok((Token::Plus, res))
        } else if let Some(res) = Self::matches(input, "-", Following::CharsAllowed) {
            Ok((Token::Minus, res))
        } else if let Some(res) = Self::matches(input, "/", Following::CharsAllowed) {
            Ok((Token::Divide, res))
        } else if let Some(res) = Self::matches(input, "*", Following::CharsAllowed) {
            Ok((Token::Multiply, res))
        } else if let Some(res) = Self::matches(input, "!=", Following::CharsAllowed) {
            Ok((Token::NotEqual, res))
        } else if let Some(res) = Self::matches(input, ">=", Following::CharsAllowed) {
            Ok((Token::GreaterThanEqual, res))
        } else if let Some(res) = Self::matches(input, ">", Following::CharsAllowed) {
            Ok((Token::GreaterThan, res))
        } else if let Some(res) = Self::matches(input, "<=", Following::CharsAllowed) {
            Ok((Token::LessThanEqual, res))
        } else if let Some(res) = Self::matches(input, "<", Following::CharsAllowed) {
            Ok((Token::LessThan, res))
        } else if let Some(res) = Self::matches(input, "!", Following::CharsAllowed) {
            Ok((Token::Not, res))
        } else if let Some(res) = Self::matches(input, ",", Following::CharsAllowed) {
            Ok((Token::Comma, res))
        } else if let Some(res) = Self::matches(input, ":", Following::CharsAllowed) {
            Ok((Token::Colon, res))
        } else if let Some(res) = Self::matches(input, ";", Following::CharsAllowed) {
            Ok((Token::SemiColon, res))
        } else if let Some(res) = Self::matches(input, "(", Following::CharsAllowed) {
            Ok((Token::LeftRoundBracket, res))
        } else if let Some(res) = Self::matches(input, ")", Following::CharsAllowed) {
            Ok((Token::RightRoundBracket, res))
        } else if let Some(res) = Self::matches(input, "{", Following::CharsAllowed) {
            Ok((Token::LeftCurlyBracket, res))
        } else if let Some(res) = Self::matches(input, "}", Following::CharsAllowed) {
            Ok((Token::RightCurlyBracket, res))
        } else if let Some(res) = Self::matches(input, "[", Following::CharsAllowed) {
            Ok((Token::LeftSquareBracket, res))
        } else if let Some(res) = Self::matches(input, "]", Following::CharsAllowed) {
            Ok((Token::RightSquareBracket, res))
        } else if let Some((ident, res)) = Self::read_identifier(input)? {
            Ok((Token::Identifier(ident), res))
        } else {
            Err(Error::SyntaxError)
        }
    }

    /// Return true if given string is Some, and the first character in the
    /// given string is alphanumeric. Return false otherwise.
    fn starts_with_alphanumeric(following: Option<&str>) -> bool {
        match following {
            None => false,
            Some(s) => {
                match s.chars().next() {
                    None => false,
                    Some(c) => c.is_alphabetic()
                }
            }
        }
    }

    fn matches<'a>(input: &'a str, word: &'_ str, following: Following) -> Option<&'a str> {
        let word_len = word.len();
        if input.len() < word_len {
            None
        } else if input.get(0..word_len) == Some(word) {
            match following {
                Following::NoCharsAllowed  => {
                    if Self::starts_with_alphanumeric(input.get(word_len..)) {
                        None
                    } else {
                        Some(&input[word_len..])
                    }
                }
                Following::CharsAllowed => {
                    Some(&input[word_len..])
                }
            }
        } else {
            None
        }
    }

    fn read_string<'a>(input: &'a str) -> Result<Option<(&'a str, &'a str)>, Error> {
        if let Some(res) = Self::matches(input, "\"", Following::CharsAllowed) {
            let mut escaped = false;
            let mut byte_count = 0;
            for ch in res.chars() {
                if ch == '\\' {
                    escaped = true;
                } else if ch == '"' && !escaped {
                    return Ok(
                            // It's OK, these are guaranteed to be on UTF-8
                            // character boundaries
                            Some((
                                unsafe { res.slice_unchecked(0, byte_count) },
                                unsafe { res.slice_unchecked(byte_count + 1, res.len()) }
                            ))
                        );
                } else {
                    escaped = false;
                }
                byte_count = byte_count + ch.len_utf8();
            }
            // Oh dear - an unterminated string.
            Err(Error::SyntaxError)
        } else {
            Ok(None)
        }
    }

    fn read_identifier<'a>(input: &'a str) -> Result<Option<(&'a str, &'a str)>, Error> {
        let mut byte_count = 0;
        for ch in input.chars() {
            if byte_count == 0 {
                match ch {
                    'a'...'z' => {},
                    'A'...'Z' => {},
                    '_' => {},
                    _ => break,
                }
            } else {
                match ch {
                    'a'...'z' => {},
                    'A'...'Z' => {},
                    '0'...'9' => {},
                    '_' => {},
                    _ => break,
                }
            }
            byte_count += ch.len_utf8();
        }
        if byte_count > 0 {
            // Got some chars
            Ok(Some((&input[0..byte_count], &input[byte_count..])))
        } else {
            Ok(None)
        }
    }

    fn read_decimal<'a>(input: &'a str) -> Result<Option<(i64, &'a str)>, Error> {
        let mut input = input;
        let mut result = 0i64;
        let mut negative = false;
        let mut valid = false;
        if let Some(res) = Self::matches(input, "-", Following::CharsAllowed) {
            negative = true;
            input = res;
        }
        if let Some(_) = Self::matches(input, "0x", Following::CharsAllowed) {
            // This is a hex literal - bail out as this routine only handles
            // decimal
            return Ok(None);
        }

        let mut byte_count = 0;
        for ch in input.chars() {
            match ch {
                '0' => { result *= 10; result += 0; }
                '1' => { result *= 10; result += 1; }
                '2' => { result *= 10; result += 2; }
                '3' => { result *= 10; result += 3; }
                '4' => { result *= 10; result += 4; }
                '5' => { result *= 10; result += 5; }
                '6' => { result *= 10; result += 6; }
                '7' => { result *= 10; result += 7; }
                '8' => { result *= 10; result += 8; }
                '9' => { result *= 10; result += 9; }
                '_' if valid => {}, // ignore underscores except at the start
                'a' ... 'z' if valid => {
                    // Numbers should not have letters in them
                    return Err(Error::SyntaxError)
                },
                'A' ... 'Z' if valid => {
                    // Numbers should not have letters in them
                    return Err(Error::SyntaxError)
                },
                // Anything else means end of number
                _ => break,
            }
            valid = true;
            byte_count += ch.len_utf8();
        }
        if valid {
            if negative {
                result = -result;
            }
            Ok(Some((result, &input[byte_count..])))
        } else {
            Ok(None)
        }
    }

    fn read_hex<'a>(input: &'a str) -> Result<Option<(i64, &'a str)>, Error> {
        let mut input = input;
        let mut result = 0i64;
        let mut valid = false;
        if let Some(res) = Self::matches(input, "0x", Following::CharsAllowed) {
            input = res;
        } else {
            // Not a hex literal
            return Ok(None);
        }

        let mut byte_count = 0;
        for ch in input.chars() {
            match ch {
                '0' => { result *= 16; result += 0; }
                '1' => { result *= 16; result += 1; }
                '2' => { result *= 16; result += 2; }
                '3' => { result *= 16; result += 3; }
                '4' => { result *= 16; result += 4; }
                '5' => { result *= 16; result += 5; }
                '6' => { result *= 16; result += 6; }
                '7' => { result *= 16; result += 7; }
                '8' => { result *= 16; result += 8; }
                '9' => { result *= 16; result += 9; }
                '_' if valid => {}, // ignore underscores
                'A' | 'a' => { result *= 16; result += 10; }
                'B' | 'b' => { result *= 16; result += 11; }
                'C' | 'c' => { result *= 16; result += 12; }
                'D' | 'd' => { result *= 16; result += 13; }
                'E' | 'e' => { result *= 16; result += 14; }
                'F' | 'f' => { result *= 16; result += 15; }
                'g' ... 'z' if valid => {
                    // Numbers should not have letters in them
                    return Err(Error::SyntaxError)
                },
                'G' ... 'Z' if valid => {
                    // Numbers should not have letters in them
                    return Err(Error::SyntaxError)
                },
                // Anything else means end of number
                _ => break,
            }
            valid = true;
            byte_count += ch.len_utf8();
        }
        if valid {
            Ok(Some((result, &input[byte_count..])))
        } else {
            Ok(None)
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn int_literal() {
        assert_eq!(Lexer::lex_tokens("123"), Ok((Token::DecimalIntLiteral(123), "")));
        assert_eq!(Lexer::lex_tokens(" 123"), Ok((Token::DecimalIntLiteral(123), "")));
        assert_eq!(Lexer::lex_tokens("123 "), Ok((Token::DecimalIntLiteral(123), " ")));
        assert_eq!(Lexer::lex_tokens(" 123 "), Ok((Token::DecimalIntLiteral(123), " ")));
        assert_eq!(Lexer::lex_tokens("0x100"), Ok((Token::HexIntLiteral(256), "")));
        assert_eq!(Lexer::lex_tokens("0x8000_0000"), Ok((Token::HexIntLiteral(1 << 31), "")));
        assert_eq!(Lexer::lex_tokens("-567"), Ok((Token::DecimalIntLiteral(-567), "")));
    }

    #[test]
    fn keywords() {
        assert_eq!(Lexer::lex_tokens(" if"), Ok((Token::If, "")));
        assert_eq!(Lexer::lex_tokens("if€"), Ok((Token::If, "€")));
        assert_eq!(Lexer::lex_tokens("let"), Ok((Token::Let, "")));
        assert_eq!(Lexer::lex_tokens("return"), Ok((Token::Return, "")));
        assert_eq!(Lexer::lex_tokens("while"), Ok((Token::While, "")));
        assert_eq!(Lexer::lex_tokens("for"), Ok((Token::For, "")));
        assert_eq!(Lexer::lex_tokens("break"), Ok((Token::Break, "")));
    }

    #[test]
    fn identifiers() {
        assert_eq!(Lexer::lex_tokens("x"), Ok((Token::Identifier("x"), "")));
        assert_eq!(Lexer::lex_tokens("lets"), Ok((Token::Identifier("lets"), "")));
        assert_eq!(Lexer::lex_tokens("x==123"), Ok((Token::Identifier("x"), "==123")));
        assert_eq!(Lexer::lex_tokens("Abc"), Ok((Token::Identifier("Abc"), "")));
        assert_eq!(Lexer::lex_tokens("Abc0"), Ok((Token::Identifier("Abc0"), "")));
        assert_eq!(Lexer::lex_tokens("a_bc0"), Ok((Token::Identifier("a_bc0"), "")));
        assert_eq!(Lexer::lex_tokens("_abc"), Ok((Token::Identifier("_abc"), "")));
        assert_eq!(Lexer::lex_tokens("0abc"), Err(Error::SyntaxError));
    }

    #[test]
    fn strings() {
        assert_eq!(Lexer::lex_tokens("\"test\""), Ok((Token::StringLiteral("test"), "")));
        assert_eq!(Lexer::lex_tokens(" \"test\""), Ok((Token::StringLiteral("test"), "")));
        assert_eq!(Lexer::lex_tokens("\"test\" "), Ok((Token::StringLiteral("test"), " ")));
        // The lexer doesn't re-write strings to remove the escapes. That
        // would require memory allocation.
        assert_eq!(Lexer::lex_tokens("\"te\\\"st \" "), Ok((Token::StringLiteral("te\\\"st "), " ")));
    }

    #[test]
    fn complicated_source() {
        let source = r#"
let x = 123;
let y = foo(x);
if y > x {
    baz();
} else {
    bar();
}
return 0x200;
        "#;
        let expected_tokens = [
            Token::Let,
            Token::Identifier("x"),
            Token::Assign,
            Token::DecimalIntLiteral(123),
            Token::SemiColon,
            Token::Let,
            Token::Identifier("y"),
            Token::Assign,
            Token::Identifier("foo"),
            Token::LeftRoundBracket,
            Token::Identifier("x"),
            Token::RightRoundBracket,
            Token::SemiColon,
            Token::If,
            Token::Identifier("y"),
            Token::GreaterThan,
            Token::Identifier("x"),
            Token::LeftCurlyBracket,
            Token::Identifier("baz"),
            Token::LeftRoundBracket,
            Token::RightRoundBracket,
            Token::SemiColon,
            Token::RightCurlyBracket,
            Token::Else,
            Token::LeftCurlyBracket,
            Token::Identifier("bar"),
            Token::LeftRoundBracket,
            Token::RightRoundBracket,
            Token::SemiColon,
            Token::RightCurlyBracket,
            Token::Return,
            Token::HexIntLiteral(512),
            Token::SemiColon,
            Token::EOF
        ];
        let mut buffer = source;
        for expected_token in expected_tokens.iter() {
            let (token, remainder) = Lexer::lex_tokens(buffer).unwrap();
            buffer = remainder;
            assert_eq!(token, *expected_token);
        }
        assert_eq!(buffer.len(), 0);
    }
}