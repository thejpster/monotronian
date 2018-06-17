//! This is a lexer for the monotronian language.
//!
//! It can handle the keywords, typical punctuation for a curly bracket
//! language, integers, identifiers and strings. It's like C, but using the
//! `let` keyword, and without any type information.
//!
//! There's also no concept of declaring a function because each function is
//! handled as a stand-alone unit to save memory.

use nom;

pub use self::token::Token;
pub mod token;

pub struct Lexer;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Error {
    SyntaxError,
    BadNumber,
}

pub struct TokenIterator<'a> {
    input: &'a str,
}

impl<'a> Iterator for TokenIterator<'a> {
    type Item = Result<Token<'a>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.input == "" {
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
fn parse_reserved(word: nom::types::CompleteStr) -> Option<Token> {
    match word.0 {
        "break" => Some(Token::Break),
        "else" => Some(Token::Else),
        "false" => Some(Token::BoolLiteral(false)),
        "for" => Some(Token::For),
        "if" => Some(Token::If),
        "let" => Some(Token::Let),
        "return" => Some(Token::Return),
        "while" => Some(Token::While),
        "true" => Some(Token::BoolLiteral(true)),
        w => match w.chars().next() {
            Some('0'...'9') => None,
            _ => Some(Token::Identifier(word.0)),
        },
    }
}

named!(keyword_or_identifier<nom::types::CompleteStr, Token>,
    do_parse!(
        id: map_opt!(take_while1!(|c: char| c.is_alphanumeric() || c == '_'), parse_reserved) >>
        (id)
    )
);

named!(
    op_equal<&str, Token>,
    do_parse!(tag!("==") >> (Token::Equal))
);
named!(
    op_assign<&str, Token>,
    do_parse!(tag!("=") >> (Token::Assign))
);
named!(
    op_plus<&str, Token>,
    do_parse!(tag!("+") >> (Token::Plus))
);
named!(
    op_minus<&str, Token>,
    do_parse!(tag!("-") >> (Token::Minus))
);
named!(
    op_exclamationmark<&str, Token>,
    do_parse!(tag!("!") >> (Token::ExclamationMark))
);
named!(
    op_slash<&str, Token>,
    do_parse!(tag!("/") >> (Token::Slash))
);
named!(
    op_star<&str, Token>,
    do_parse!(tag!("*") >> (Token::Star))
);
named!(
    op_notequal<&str, Token>,
    do_parse!(tag!("!=") >> (Token::NotEqual))
);
named!(
    op_greaterthanequal<&str, Token>,
    do_parse!(tag!(">=") >> (Token::GreaterThanEqual))
);
named!(
    op_greaterthan<&str, Token>,
    do_parse!(tag!(">") >> (Token::GreaterThan))
);
named!(
    op_lessthanequal<&str, Token>,
    do_parse!(tag!("<=") >> (Token::LessThanEqual))
);
named!(
    op_lessthan<&str, Token>,
    do_parse!(tag!("<") >> (Token::LessThan))
);
named!(
    op_comma<&str, Token>,
    do_parse!(tag!(",") >> (Token::Comma))
);
named!(
    op_colon<&str, Token>,
    do_parse!(tag!(":") >> (Token::Colon))
);
named!(
    op_semicolon<&str, Token>,
    do_parse!(tag!(";") >> (Token::SemiColon))
);
named!(
    op_leftroundbracket<&str, Token>,
    do_parse!(tag!("(") >> (Token::LeftRoundBracket))
);
named!(
    op_rightroundbracket<&str, Token>,
    do_parse!(tag!(")") >> (Token::RightRoundBracket))
);
named!(
    op_leftcurlybracket<&str, Token>,
    do_parse!(tag!("{") >> (Token::LeftCurlyBracket))
);
named!(
    op_rightcurlybracket<&str, Token>,
    do_parse!(tag!("}") >> (Token::RightCurlyBracket))
);
named!(
    op_leftsquarebracket<&str, Token>,
    do_parse!(tag!("[") >> (Token::LeftSquareBracket))
);
named!(
    op_rightsquarebracket<&str, Token>,
    do_parse!(tag!("]") >> (Token::RightSquareBracket))
);

named!(
    op<&str, Token>,
    alt!(op_equal | op_assign | op_plus | op_minus | op_exclamationmark | op_slash | op_star | op_notequal | op_greaterthanequal | op_greaterthan | op_lessthanequal | op_lessthan | op_comma | op_colon | op_semicolon | op_leftroundbracket | op_rightroundbracket | op_leftcurlybracket | op_rightcurlybracket | op_leftsquarebracket | op_rightsquarebracket)
);

named!(
    hex_literal<nom::types::CompleteStr, Token>,
    do_parse!(
        tag!("0x") >>
        num: map_res!(take_while1!(|c: char| c.is_alphanumeric() || c == '_'), |s| Lexer::convert_digits(s, 16)) >>
        (Token::HexIntLiteral(num))
    )
);

named!(
    dec_literal<nom::types::CompleteStr, Token>,
    do_parse!(
        neg: opt!(tag!("-")) >>
        num: map_res!(take_while1!(|c: char| c.is_alphanumeric() || c == '_'), |s| Lexer::convert_digits(s, 10)) >>
        (Token::DecimalIntLiteral(if neg.is_some() { -num } else { num }))
    )
);

named!(
    string_literal<nom::types::CompleteStr, Token>,
    do_parse!(
        s: delimited!(
            tag!("\""),
            escaped!(take_until_either!("\"\\"), '\\', one_of!("\"\\")),
            // take_until_either!("\""),
            tag!("\"")
        ) >>
        (Token::StringLiteral(s.0))
    )
);

impl Lexer {
    pub fn iterate<'a>(input: &'a str) -> TokenIterator<'a> {
        TokenIterator { input }
    }

    pub fn lex_tokens(input: &str) -> Result<(Token, &str), Error> {
        let input = input.trim_left();
        if input.len() == 0 {
            Ok((Token::EOF, ""))
        } else if let Ok((res, tok)) = string_literal(nom::types::CompleteStr(input)) {
            Ok((tok, res.0))
        } else if let Ok((res, tok)) = hex_literal(nom::types::CompleteStr(input)) {
            Ok((tok, res.0))
        } else if let Ok((res, tok)) = dec_literal(nom::types::CompleteStr(input)) {
            Ok((tok, res.0))
        } else if let Ok((res, tok)) = keyword_or_identifier(nom::types::CompleteStr(input)) {
            Ok((tok, res.0))
        } else if let Ok((res, tok)) = op(input) {
            Ok((tok, res))
        } else {
            Err(Error::SyntaxError)
        }
    }

    /// Like i64::from_str_radix but ignores underscores.
    fn convert_digits<'a>(input: nom::types::CompleteStr, radix: u32) -> Result<i64, Error> {
        let mut result = 0i64;
        let mut valid = false;

        for ch in input.0.chars() {
            match ch {
                '_' => {
                    // ignore underscores except at the start
                    if !valid {
                        return Err(Error::BadNumber);
                    }
                }
                _ => {
                    result *= i64::from(radix);
                    result += ch.to_digit(radix).ok_or(Error::BadNumber)? as i64;
                }
            }
            valid = true;
        }
        if valid {
            Ok(result)
        } else {
            Err(Error::BadNumber)
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn bool_literal() {
        assert_eq!(
            Lexer::lex_tokens(" false "),
            Ok((Token::BoolLiteral(false), " "))
        );
        assert_eq!(
            Lexer::lex_tokens(" true "),
            Ok((Token::BoolLiteral(true), " "))
        );
    }

    #[test]
    fn int_literal() {
        assert_eq!(
            Lexer::lex_tokens("123"),
            Ok((Token::DecimalIntLiteral(123), ""))
        );
        assert_eq!(
            Lexer::lex_tokens(" 123"),
            Ok((Token::DecimalIntLiteral(123), ""))
        );
        assert_eq!(
            Lexer::lex_tokens("123 "),
            Ok((Token::DecimalIntLiteral(123), " "))
        );
        assert_eq!(
            Lexer::lex_tokens(" 123 "),
            Ok((Token::DecimalIntLiteral(123), " "))
        );
        assert_eq!(
            Lexer::lex_tokens("0x100"),
            Ok((Token::HexIntLiteral(256), ""))
        );
        assert_eq!(
            Lexer::lex_tokens("0x8000_0000"),
            Ok((Token::HexIntLiteral(1 << 31), ""))
        );
        assert_eq!(
            Lexer::lex_tokens("-567"),
            Ok((Token::DecimalIntLiteral(-567), ""))
        );
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
        assert_eq!(
            Lexer::lex_tokens("lets"),
            Ok((Token::Identifier("lets"), ""))
        );
        assert_eq!(
            Lexer::lex_tokens("x==123"),
            Ok((Token::Identifier("x"), "==123"))
        );
        assert_eq!(Lexer::lex_tokens("Abc"), Ok((Token::Identifier("Abc"), "")));
        assert_eq!(
            Lexer::lex_tokens("Abc0"),
            Ok((Token::Identifier("Abc0"), ""))
        );
        assert_eq!(
            Lexer::lex_tokens("a_bc0"),
            Ok((Token::Identifier("a_bc0"), ""))
        );
        assert_eq!(
            Lexer::lex_tokens("_abc"),
            Ok((Token::Identifier("_abc"), ""))
        );
        assert_eq!(Lexer::lex_tokens("0abc"), Err(Error::SyntaxError));
    }

    #[test]
    fn strings() {
        assert_eq!(
            Lexer::lex_tokens("\"test\""),
            Ok((Token::StringLiteral("test"), ""))
        );
        assert_eq!(
            Lexer::lex_tokens(" \"test\""),
            Ok((Token::StringLiteral("test"), ""))
        );
        assert_eq!(
            Lexer::lex_tokens("\"test\" "),
            Ok((Token::StringLiteral("test"), " "))
        );
        // The lexer doesn't re-write strings to remove the escapes. That
        // would require memory allocation.
        assert_eq!(
            Lexer::lex_tokens("\"te\\\"st \" "),
            Ok((Token::StringLiteral("te\\\"st "), " "))
        );
    }

    #[test]
    fn complicated_source() {
        let source = r#"
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
            Token::BoolLiteral(true),
            Token::RightRoundBracket,
            Token::SemiColon,
            Token::RightCurlyBracket,
            Token::Else,
            Token::LeftCurlyBracket,
            Token::Identifier("bar"),
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
        let mut buffer = source;
        for expected_token in expected_tokens.iter() {
            let (token, remainder) = Lexer::lex_tokens(buffer).unwrap();
            buffer = remainder;
            assert_eq!(token, *expected_token);
        }
        assert_eq!(buffer.len(), 0);

        let tokens: Result<Vec<Token>, Error> = Lexer::iterate(source).collect();
        let tokens = tokens.unwrap();
        assert_eq!(&tokens[..], &expected_tokens[..]);
    }
}
