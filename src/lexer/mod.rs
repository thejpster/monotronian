pub mod token;
pub use self::token::Token;

pub struct Lexer;

#[derive(Debug)]
pub enum Error {
    SyntaxError,
    Incomplete
}

impl Lexer {
    pub fn lex_tokens(input: &str) -> Result<(Token, &str), Error> {
        let input = input.trim_left();
        if input.len() == 0 {
            Ok((Token::EOF, ""))
        } else if let Some(res) = Self::matches(input, "if") {
            Ok((Token::If, res))
        } else {
            Err(Error::SyntaxError)
        }
    }

    fn matches<'a>(input: &'a str, word: &'_ str) -> Option<&'a str> {
        let word_len = word.len();
        if &input[0..word_len] == word {
            Some(&input[word_len..])
        } else {
            None
        }
    }
}