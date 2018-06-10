pub mod token;
pub use self::token::Token;

pub struct Lexer;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Error {
    SyntaxError,
    Incomplete
}

impl Lexer {
    pub fn lex_tokens(input: &str) -> Result<(Token, &str), Error> {
        let input = input.trim_left();
        if input.len() == 0 {
            Ok((Token::EOF, ""))
        } else if let Some((num, res)) = Self::read_number(input)? {
            Ok((Token::IntLiteral(num), res))
        } else if let Some((string, res)) = Self::read_string(input)? {
            Ok((Token::StringLiteral(string), res))
        } else if let Some(res) = Self::matches(input, "if") {
            Ok((Token::If, res))
        } else if let Some(res) = Self::matches(input, "==") {
            Ok((Token::Equal, res))
        } else if let Some(res) = Self::matches(input, "=") {
            Ok((Token::Assign, res))
        } else if let Some(res) = Self::matches(input, "else") {
            Ok((Token::Else, res))
        } else if let Some(res) = Self::matches(input, "+") {
            Ok((Token::Plus, res))
        } else if let Some(res) = Self::matches(input, "-") {
            Ok((Token::Minus, res))
        } else if let Some(res) = Self::matches(input, "/") {
            Ok((Token::Divide, res))
        } else if let Some(res) = Self::matches(input, "*") {
            Ok((Token::Multiply, res))
        } else if let Some(res) = Self::matches(input, "!=") {
            Ok((Token::NotEqual, res))
        } else if let Some(res) = Self::matches(input, ">=") {
            Ok((Token::GreaterThanEqual, res))
        } else if let Some(res) = Self::matches(input, ">") {
            Ok((Token::GreaterThan, res))
        } else if let Some(res) = Self::matches(input, "<=") {
            Ok((Token::LessThanEqual, res))
        } else if let Some(res) = Self::matches(input, "<") {
            Ok((Token::LessThan, res))
        } else if let Some(res) = Self::matches(input, "!") {
            Ok((Token::Not, res))
        } else if let Some(res) = Self::matches(input, "let") {
            Ok((Token::Let, res))
        } else if let Some(res) = Self::matches(input, "return") {
            Ok((Token::Return, res))
        } else if let Some(res) = Self::matches(input, ",") {
            Ok((Token::Comma, res))
        } else if let Some(res) = Self::matches(input, ":") {
            Ok((Token::Colon, res))
        } else if let Some(res) = Self::matches(input, ";") {
            Ok((Token::SemiColon, res))
        } else if let Some(res) = Self::matches(input, "(") {
            Ok((Token::LParen, res))
        } else if let Some(res) = Self::matches(input, ")") {
            Ok((Token::RParen, res))
        } else if let Some(res) = Self::matches(input, "{") {
            Ok((Token::LBrace, res))
        } else if let Some(res) = Self::matches(input, "}") {
            Ok((Token::RBrace, res))
        } else if let Some(res) = Self::matches(input, "[") {
            Ok((Token::LBracket, res))
        } else if let Some(res) = Self::matches(input, "]") {
            Ok((Token::RBracket, res))
        } else {
            Err(Error::SyntaxError)
        }
    }

    fn matches<'a>(input: &'a str, word: &'_ str) -> Option<&'a str> {
        let word_len = word.len();
        if word_len > input.len() {
            None
        } else if &input[0..word_len] == word {
            Some(&input[word_len..])
        } else {
            None
        }
    }

    fn read_string<'a>(input: &'a str) -> Result<Option<(&'a str, &'a str)>, Error> {
        if let Some(res) = Self::matches(input, "\"") {
            let mut escaped = false;
            for (idx, ch) in res.bytes().enumerate() {
                if ch == b'\\' {
                    escaped = true;
                } else if ch == b'"' && !escaped {
                    return Ok(
                            Some((
                                unsafe { res.slice_unchecked(0, idx) },
                                unsafe { res.slice_unchecked(idx + 1, res.len()) }
                            ))
                        );
                } else {
                    escaped = false;
                }
            }
            // Oh dear - an unterminated string.
            Err(Error::SyntaxError)
        } else {
            Ok(None)
        }
    }

    fn read_number<'a>(input: &'a str) -> Result<Option<(i64, &'a str)>, Error> {
        let mut input = input;
        let mut result = 0i64;
        let mut negative = false;
        let mut radix = 10;
        let mut valid = false;
        if let Some(res) = Self::matches(input, "-") {
            negative = true;
            input = res;
        }
        if let Some(res) = Self::matches(input, "0x") {
            radix = 16;
            input = res;
        }

        loop {
            if let Some(ch) = input.chars().next() {
                match ch {
                    '0' => { result *= radix; result += 0; }
                    '1' => { result *= radix; result += 1; }
                    '2' => { result *= radix; result += 2; }
                    '3' => { result *= radix; result += 3; }
                    '4' => { result *= radix; result += 4; }
                    '5' => { result *= radix; result += 5; }
                    '6' => { result *= radix; result += 6; }
                    '7' => { result *= radix; result += 7; }
                    '8' => { result *= radix; result += 8; }
                    '9' => { result *= radix; result += 9; }
                    'A' | 'a' if (radix == 16) => { result *= radix; result += 10; }
                    'B' | 'b' if (radix == 16) => { result *= radix; result += 11; }
                    'C' | 'c' if (radix == 16) => { result *= radix; result += 12; }
                    'D' | 'd' if (radix == 16) => { result *= radix; result += 13; }
                    'E' | 'e' if (radix == 16) => { result *= radix; result += 14; }
                    'F' | 'f' if (radix == 16) => { result *= radix; result += 15; }
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
            }
            valid = true;
            input = &input[1..];
            if input.len() == 0 {
                break;
            }
        }
        if valid {
            if negative {
                result = -result;
            }
            Ok(Some((result, input)))
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
        assert_eq!(Lexer::lex_tokens("123"), Ok((Token::IntLiteral(123), "")));
        assert_eq!(Lexer::lex_tokens(" 123"), Ok((Token::IntLiteral(123), "")));
        assert_eq!(Lexer::lex_tokens("123 "), Ok((Token::IntLiteral(123), " ")));
        assert_eq!(Lexer::lex_tokens(" 123 "), Ok((Token::IntLiteral(123), " ")));
        assert_eq!(Lexer::lex_tokens("0x100"), Ok((Token::IntLiteral(256), "")));
        assert_eq!(Lexer::lex_tokens("-0x100"), Ok((Token::IntLiteral(-256), "")));
        assert_eq!(Lexer::lex_tokens("-567"), Ok((Token::IntLiteral(-567), "")));
    }

    #[test]
    fn keywords() {
        assert_eq!(Lexer::lex_tokens(" if"), Ok((Token::If, "")));
        assert_eq!(Lexer::lex_tokens("let"), Ok((Token::Let, "")));
        assert_eq!(Lexer::lex_tokens("return"), Ok((Token::Return, "")));
    }

    #[test]
    fn strings() {
        assert_eq!(Lexer::lex_tokens("\"test\""), Ok((Token::StringLiteral("test"), "")));
        assert_eq!(Lexer::lex_tokens(" \"test\""), Ok((Token::StringLiteral("test"), "")));
        assert_eq!(Lexer::lex_tokens("\"test\" "), Ok((Token::StringLiteral("test"), " ")));
        // The lexer doesn't re-write strings to remove the escapes.
        assert_eq!(Lexer::lex_tokens("\"te\\\"st \" "), Ok((Token::StringLiteral("te\\\"st "), " ")));
    }

}