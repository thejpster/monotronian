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
pub use self::token::Token;
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
        if check_case_insensitive(word.0, b"abs") {
            return Some(Token::Abs);
        }
        if check_case_insensitive(word.0, b"acs") {
            return Some(Token::Acs);
        }
        if check_case_insensitive(word.0, b"as") {
            return Some(Token::As);
        }
        if check_case_insensitive(word.0, b"asc") {
            return Some(Token::Asc);
        }
        if check_case_insensitive(word.0, b"asn") {
            return Some(Token::Asn);
        }
        if check_case_insensitive(word.0, b"atn") {
            return Some(Token::Atn);
        }
        if check_case_insensitive(word.0, b"auto") {
            return Some(Token::Auto);
        }
        if check_case_insensitive(word.0, b"beep") {
            return Some(Token::Beep);
        }
        if check_case_insensitive(word.0, b"bget#") {
            return Some(Token::BGetHash);
        }
        if check_case_insensitive(word.0, b"true") {
            return Some(Token::BoolLiteral(true));
        }
        if check_case_insensitive(word.0, b"bput#") {
            return Some(Token::BPutHash);
        }
        if check_case_insensitive(word.0, b"break") {
            return Some(Token::Break);
        }
        if check_case_insensitive(word.0, b"by") {
            return Some(Token::By);
        }
        if check_case_insensitive(word.0, b"call") {
            return Some(Token::Call);
        }
        if check_case_insensitive(word.0, b"caret") {
            return Some(Token::Caret);
        }
        if check_case_insensitive(word.0, b"case") {
            return Some(Token::Case);
        }
        if check_case_insensitive(word.0, b"chr$") {
            return Some(Token::ChrDollar);
        }
        if check_case_insensitive(word.0, b"circle") {
            return Some(Token::Circle);
        }
        if check_case_insensitive(word.0, b"clear") {
            return Some(Token::Clear);
        }
        if check_case_insensitive(word.0, b"clg") {
            return Some(Token::Clg);
        }
        if check_case_insensitive(word.0, b"close") {
            return Some(Token::Close);
        }
        if check_case_insensitive(word.0, b"cls") {
            return Some(Token::Cls);
        }
        if check_case_insensitive(word.0, b"colon") {
            return Some(Token::Colon);
        }
        if check_case_insensitive(word.0, b"colour") {
            return Some(Token::Colour);
        }
        if check_case_insensitive(word.0, b"comma") {
            return Some(Token::Comma);
        }
        if check_case_insensitive(word.0, b"cos") {
            return Some(Token::Cos);
        }
        if check_case_insensitive(word.0, b"count") {
            return Some(Token::Count);
        }
        if check_case_insensitive(word.0, b"data") {
            return Some(Token::Data);
        }
        if check_case_insensitive(word.0, b"def") {
            return Some(Token::Def);
        }
        if check_case_insensitive(word.0, b"deg") {
            return Some(Token::Deg);
        }
        if check_case_insensitive(word.0, b"dim") {
            return Some(Token::Dim);
        }
        if check_case_insensitive(word.0, b"div") {
            return Some(Token::Div);
        }
        if check_case_insensitive(word.0, b"draw") {
            return Some(Token::Draw);
        }
        if check_case_insensitive(word.0, b"edit") {
            return Some(Token::Edit);
        }
        if check_case_insensitive(word.0, b"ellipse") {
            return Some(Token::Ellipse);
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
        if check_case_insensitive(word.0, b"endcase") {
            return Some(Token::EndCase);
        }
        if check_case_insensitive(word.0, b"endfn") {
            return Some(Token::EndFunc);
        }
        if check_case_insensitive(word.0, b"endif") {
            return Some(Token::EndIf);
        }
        if check_case_insensitive(word.0, b"endproc") {
            return Some(Token::EndProc);
        }
        if check_case_insensitive(word.0, b"endwhile") {
            return Some(Token::EndWhile);
        }
        if check_case_insensitive(word.0, b"eof#") {
            return Some(Token::EofHash);
        }
        if check_case_insensitive(word.0, b"eor") {
            return Some(Token::Eor);
        }
        if check_case_insensitive(word.0, b"erl") {
            return Some(Token::Erl);
        }
        if check_case_insensitive(word.0, b"error") {
            return Some(Token::Error);
        }
        if check_case_insensitive(word.0, b"exp") {
            return Some(Token::Exp);
        }
        if check_case_insensitive(word.0, b"ext") {
            return Some(Token::Ext);
        }
        if check_case_insensitive(word.0, b"false") {
            return Some(Token::BoolLiteral(false));
        }
        if check_case_insensitive(word.0, b"fill") {
            return Some(Token::Fill);
        }
        if check_case_insensitive(word.0, b"for") {
            return Some(Token::For);
        }
        if check_case_insensitive(word.0, b"fn") {
            return Some(Token::Func);
        }
        if check_case_insensitive(word.0, b"gcol") {
            return Some(Token::GCol);
        }
        if check_case_insensitive(word.0, b"get") {
            return Some(Token::Get);
        }
        if check_case_insensitive(word.0, b"get$") {
            return Some(Token::GetDollar);
        }
        if check_case_insensitive(word.0, b"get#") {
            return Some(Token::GetHash);
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
        if check_case_insensitive(word.0, b"inkey") {
            return Some(Token::InKey);
        }
        if check_case_insensitive(word.0, b"inkey$") {
            return Some(Token::InkeyDollar);
        }
        if check_case_insensitive(word.0, b"input") {
            return Some(Token::Input);
        }
        if check_case_insensitive(word.0, b"input#") {
            return Some(Token::InputHash);
        }
        if check_case_insensitive(word.0, b"insert") {
            return Some(Token::Insert);
        }
        if check_case_insensitive(word.0, b"instr") {
            return Some(Token::InStr);
        }
        if check_case_insensitive(word.0, b"int") {
            return Some(Token::Int);
        }
        if check_case_insensitive(word.0, b"integer") {
            return Some(Token::Integer);
        }
        if check_case_insensitive(word.0, b"left$") {
            return Some(Token::LeftDollar);
        }
        if check_case_insensitive(word.0, b"len") {
            return Some(Token::Len);
        }
        if check_case_insensitive(word.0, b"let") {
            return Some(Token::Let);
        }
        if check_case_insensitive(word.0, b"line") {
            return Some(Token::Line);
        }
        if check_case_insensitive(word.0, b"list") {
            return Some(Token::List);
        }
        if check_case_insensitive(word.0, b"ln") {
            return Some(Token::Ln);
        }
        if check_case_insensitive(word.0, b"load") {
            return Some(Token::Load);
        }
        if check_case_insensitive(word.0, b"local") {
            return Some(Token::Local);
        }
        if check_case_insensitive(word.0, b"log") {
            return Some(Token::Log);
        }
        if check_case_insensitive(word.0, b"mid$") {
            return Some(Token::MidDollar);
        }
        if check_case_insensitive(word.0, b"minus") {
            return Some(Token::Minus);
        }
        if check_case_insensitive(word.0, b"mod") {
            return Some(Token::Mod);
        }
        if check_case_insensitive(word.0, b"mode") {
            return Some(Token::Mode);
        }
        if check_case_insensitive(word.0, b"move") {
            return Some(Token::Move);
        }
        if check_case_insensitive(word.0, b"new") {
            return Some(Token::New);
        }
        if check_case_insensitive(word.0, b"next") {
            return Some(Token::Next);
        }
        if check_case_insensitive(word.0, b"not") {
            return Some(Token::Not);
        }
        if check_case_insensitive(word.0, b"notequal") {
            return Some(Token::NotEqual);
        }
        if check_case_insensitive(word.0, b"of") {
            return Some(Token::Of);
        }
        if check_case_insensitive(word.0, b"off") {
            return Some(Token::Off);
        }
        if check_case_insensitive(word.0, b"on") {
            return Some(Token::On);
        }
        if check_case_insensitive(word.0, b"open") {
            return Some(Token::Open);
        }
        if check_case_insensitive(word.0, b"or") {
            return Some(Token::Or);
        }
        if check_case_insensitive(word.0, b"origin") {
            return Some(Token::Origin);
        }
        if check_case_insensitive(word.0, b"otherwise") {
            return Some(Token::Otherwise);
        }
        if check_case_insensitive(word.0, b"plus") {
            return Some(Token::Plus);
        }
        if check_case_insensitive(word.0, b"point") {
            return Some(Token::Point);
        }
        if check_case_insensitive(word.0, b"poke") {
            return Some(Token::Poke);
        }
        if check_case_insensitive(word.0, b"pos") {
            return Some(Token::Pos);
        }
        if check_case_insensitive(word.0, b"print") {
            return Some(Token::Print);
        }
        if check_case_insensitive(word.0, b"print#") {
            return Some(Token::PrintHash);
        }
        if check_case_insensitive(word.0, b"private") {
            return Some(Token::Private);
        }
        if check_case_insensitive(word.0, b"quit") {
            return Some(Token::Quit);
        }
        if check_case_insensitive(word.0, b"rad") {
            return Some(Token::Rad);
        }
        if check_case_insensitive(word.0, b"read") {
            return Some(Token::Read);
        }
        if check_case_insensitive(word.0, b"rectangle") {
            return Some(Token::Rectangle);
        }
        if check_case_insensitive(word.0, b"rem") {
            return Some(Token::Rem);
        }
        if check_case_insensitive(word.0, b"repeat") {
            return Some(Token::Repeat);
        }
        if check_case_insensitive(word.0, b"restore") {
            return Some(Token::Restore);
        }
        if check_case_insensitive(word.0, b"return") {
            return Some(Token::Return);
        }
        if check_case_insensitive(word.0, b"right$") {
            return Some(Token::RightDollar);
        }
        if check_case_insensitive(word.0, b"rnd") {
            return Some(Token::Rnd);
        }
        if check_case_insensitive(word.0, b"run") {
            return Some(Token::Run);
        }
        if check_case_insensitive(word.0, b"save") {
            return Some(Token::Save);
        }
        if check_case_insensitive(word.0, b"semicolon") {
            return Some(Token::SemiColon);
        }
        if check_case_insensitive(word.0, b"sgn") {
            return Some(Token::Sgn);
        }
        if check_case_insensitive(word.0, b"sin") {
            return Some(Token::Sin);
        }
        if check_case_insensitive(word.0, b"slash") {
            return Some(Token::Slash);
        }
        if check_case_insensitive(word.0, b"sound") {
            return Some(Token::Sound);
        }
        if check_case_insensitive(word.0, b"spc") {
            return Some(Token::Spc);
        }
        if check_case_insensitive(word.0, b"sqr") {
            return Some(Token::Sqr);
        }
        if check_case_insensitive(word.0, b"star") {
            return Some(Token::Star);
        }
        if check_case_insensitive(word.0, b"step") {
            return Some(Token::Step);
        }
        if check_case_insensitive(word.0, b"stop") {
            return Some(Token::Stop);
        }
        if check_case_insensitive(word.0, b"str$") {
            return Some(Token::StrDollar);
        }
        if check_case_insensitive(word.0, b"string$") {
            return Some(Token::StringDollar);
        }
        if check_case_insensitive(word.0, b"stroke") {
            return Some(Token::Stroke);
        }
        if check_case_insensitive(word.0, b"sum") {
            return Some(Token::Sum);
        }
        if check_case_insensitive(word.0, b"swap") {
            return Some(Token::Swap);
        }
        if check_case_insensitive(word.0, b"swi") {
            return Some(Token::Swi);
        }
        if check_case_insensitive(word.0, b"sys") {
            return Some(Token::Sys);
        }
        if check_case_insensitive(word.0, b"tab") {
            return Some(Token::Tab);
        }
        if check_case_insensitive(word.0, b"tan") {
            return Some(Token::Tan);
        }
        if check_case_insensitive(word.0, b"then") {
            return Some(Token::Then);
        }
        if check_case_insensitive(word.0, b"time") {
            return Some(Token::Time);
        }
        if check_case_insensitive(word.0, b"tint") {
            return Some(Token::Tint);
        }
        if check_case_insensitive(word.0, b"to") {
            return Some(Token::To);
        }
        if check_case_insensitive(word.0, b"trace") {
            return Some(Token::Trace);
        }
        if check_case_insensitive(word.0, b"until") {
            return Some(Token::Until);
        }
        if check_case_insensitive(word.0, b"usr") {
            return Some(Token::Usr);
        }
        if check_case_insensitive(word.0, b"val") {
            return Some(Token::Val);
        }
        if check_case_insensitive(word.0, b"verify") {
            return Some(Token::Verify);
        }
        if check_case_insensitive(word.0, b"vpos") {
            return Some(Token::VPos);
        }
        if check_case_insensitive(word.0, b"wait") {
            return Some(Token::Wait);
        }
        if check_case_insensitive(word.0, b"while") {
            return Some(Token::While);
        }
        if check_case_insensitive(word.0, b"width") {
            return Some(Token::Width);
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

/// Check if a string is a reserved word or an identifier.
fn parse_reserved_inverse(word: CompleteByteSlice) -> Option<&[u8]> {
    match parse_reserved(word) {
        Some(Token::Identifier(x)) => Some(x.0),
        _ => None
    }
}

named!(keyword_or_identifier<CompleteByteSlice, Token>,
    do_parse!(
        id: map_opt!(take_while1!(|c: u8| is_alphanumeric(c) || c == b'_' || c == b'$' || c == b'#'), parse_reserved) >>
        (id)
    )
);

named!(
    string_identifier<CompleteByteSlice, Token>,
    do_parse!(
        id: map_opt!(take_while1!(|c: u8| is_alphanumeric(c) || c == b'_'), parse_reserved_inverse) >>
        tag!("$") >>
        (Token::StringIdentifier(ByteString(id)))
    )
);

named!(
    integer_identifier<CompleteByteSlice, Token>,
    do_parse!(
        id: map_opt!(take_while1!(|c: u8| is_alphanumeric(c) || c == b'_'), parse_reserved_inverse) >>
        tag!("%") >>
        (Token::IntegerIdentifier(ByteString(id)))
    )
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
        op_assign |
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
        integer_identifier |
        keyword_or_identifier |
        string_identifier
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
        check_keyword(b"Abs", Token::Abs);
        check_keyword(b"Acs", Token::Acs);
        check_keyword(b"As", Token::As);
        check_keyword(b"Asc", Token::Asc);
        check_keyword(b"Asn", Token::Asn);
        check_keyword(b"=", Token::Assign);
        check_keyword(b"Atn", Token::Atn);
        check_keyword(b"Auto", Token::Auto);
        check_keyword(b"Beep", Token::Beep);
        check_keyword(b"BGet#", Token::BGetHash);
        check_keyword(b"True", Token::BoolLiteral(true));
        check_keyword(b"False", Token::BoolLiteral(false));
        check_keyword(b"BPut#", Token::BPutHash);
        check_keyword(b"Break", Token::Break);
        check_keyword(b"By", Token::By);
        check_keyword(b"Call", Token::Call);
        check_keyword(b"Caret", Token::Caret);
        check_keyword(b"Case", Token::Case);
        check_keyword(b"Chr$", Token::ChrDollar);
        check_keyword(b"Circle", Token::Circle);
        check_keyword(b"Clear", Token::Clear);
        check_keyword(b"Clg", Token::Clg);
        check_keyword(b"Close", Token::Close);
        check_keyword(b"Cls", Token::Cls);
        check_keyword(b"Colon", Token::Colon);
        check_keyword(b"Colour", Token::Colour);
        check_keyword(b"Comma", Token::Comma);
        check_keyword(b"Cos", Token::Cos);
        check_keyword(b"Count", Token::Count);
        check_keyword(b"Data", Token::Data);
        check_keyword(b"1.0", Token::DecimalFloatLiteral(1.0));
        check_keyword(b"1_000.0", Token::DecimalFloatLiteral(1_000.0));
        check_keyword(b"123", Token::DecimalIntLiteral(123));
        check_keyword(b"1_000", Token::DecimalIntLiteral(1_000));
        check_keyword(b"Def", Token::Def);
        check_keyword(b"Deg", Token::Deg);
        check_keyword(b"Dim", Token::Dim);
        check_keyword(b"Div", Token::Div);
        check_keyword(b"Draw", Token::Draw);
        check_keyword(b"Edit", Token::Edit);
        check_keyword(b"Ellipse", Token::Ellipse);
        check_keyword(b"Else", Token::Else);
        check_keyword(b"ElseIf", Token::ElseIf);
        check_keyword(b"End", Token::End);
        check_keyword(b"EndCase", Token::EndCase);
        check_keyword(b"EndFn", Token::EndFunc);
        check_keyword(b"EndIf", Token::EndIf);
        check_keyword(b"EndProc", Token::EndProc);
        check_keyword(b"EndWhile", Token::EndWhile);
        check_keyword(b"EOF#", Token::EofHash);
        check_keyword(b"Eor", Token::Eor);
        check_keyword(b"Erl", Token::Erl);
        check_keyword(b"Error", Token::Error);
        check_keyword(b"Exp", Token::Exp);
        check_keyword(b"Ext", Token::Ext);
        check_keyword(b"Fill", Token::Fill);
        check_keyword(b"For", Token::For);
        check_keyword(b"GCol", Token::GCol);
        check_keyword(b"Get", Token::Get);
        check_keyword(b"Get$", Token::GetDollar);
        check_keyword(b"Get#", Token::GetHash);
        check_keyword(b"Gosub", Token::Gosub);
        check_keyword(b"Goto", Token::Goto);
        check_keyword(b">", Token::GreaterThan);
        check_keyword(b">=", Token::GreaterThanEqual);
        check_keyword(b"0x100", Token::HexIntLiteral(0x100));
        check_keyword(b"0x1_00", Token::HexIntLiteral(0x100));
        check_keyword(b"X", Token::Identifier(ByteString(b"X")));
        check_keyword(b"If", Token::If);
        check_keyword(b"InKey", Token::InKey);
        check_keyword(b"Inkey$", Token::InkeyDollar);
        check_keyword(b"Input", Token::Input);
        check_keyword(b"Input#", Token::InputHash);
        check_keyword(b"Insert", Token::Insert);
        check_keyword(b"Fn", Token::Func);
        check_keyword(b"InStr", Token::InStr);
        check_keyword(b"Int", Token::Int);
        check_keyword(b"Integer", Token::Integer);
        check_keyword(b"X%", Token::IntegerIdentifier(ByteString(b"X")));
        check_keyword(b"Left$", Token::LeftDollar);
        check_keyword(b"(", Token::LeftRoundBracket);
        check_keyword(b"Len", Token::Len);
        check_keyword(b"<", Token::LessThan);
        check_keyword(b"<=", Token::LessThanEqual);
        check_keyword(b"Let", Token::Let);
        check_keyword(b"Line", Token::Line);
        check_keyword(b"List", Token::List);
        check_keyword(b"Ln", Token::Ln);
        check_keyword(b"Load", Token::Load);
        check_keyword(b"Local", Token::Local);
        check_keyword(b"Log", Token::Log);
        check_keyword(b"Mid$", Token::MidDollar);
        check_keyword(b"Minus", Token::Minus);
        check_keyword(b"Mod", Token::Mod);
        check_keyword(b"Mode", Token::Mode);
        check_keyword(b"Move", Token::Move);
        check_keyword(b"New", Token::New);
        check_keyword(b"\n", Token::NewLine);
        check_keyword(b"Next", Token::Next);
        check_keyword(b"Not", Token::Not);
        check_keyword(b"NotEqual", Token::NotEqual);
        check_keyword(b"Of", Token::Of);
        check_keyword(b"Off", Token::Off);
        check_keyword(b"On", Token::On);
        check_keyword(b"Open", Token::Open);
        check_keyword(b"Or", Token::Or);
        check_keyword(b"Origin", Token::Origin);
        check_keyword(b"Otherwise", Token::Otherwise);
        check_keyword(b"Plus", Token::Plus);
        check_keyword(b"Point", Token::Point);
        check_keyword(b"Poke", Token::Poke);
        check_keyword(b"Pos", Token::Pos);
        check_keyword(b"Print", Token::Print);
        check_keyword(b"Print#", Token::PrintHash);
        check_keyword(b"Private", Token::Private);
        check_keyword(b"Quit", Token::Quit);
        check_keyword(b"Rad", Token::Rad);
        check_keyword(b"Read", Token::Read);
        check_keyword(b"Rectangle", Token::Rectangle);
        check_keyword(b"Rem", Token::Rem);
        check_keyword(b"Repeat", Token::Repeat);
        check_keyword(b"Restore", Token::Restore);
        check_keyword(b"Return", Token::Return);
        check_keyword(b"Right$", Token::RightDollar);
        check_keyword(b")", Token::RightRoundBracket);
        check_keyword(b"Rnd", Token::Rnd);
        check_keyword(b"Run", Token::Run);
        check_keyword(b"Save", Token::Save);
        check_keyword(b"SemiColon", Token::SemiColon);
        check_keyword(b"Sgn", Token::Sgn);
        check_keyword(b"Sin", Token::Sin);
        check_keyword(b"Slash", Token::Slash);
        check_keyword(b"Sound", Token::Sound);
        check_keyword(b"Spc", Token::Spc);
        check_keyword(b"Sqr", Token::Sqr);
        check_keyword(b"Star", Token::Star);
        check_keyword(b"Step", Token::Step);
        check_keyword(b"Stop", Token::Stop);
        check_keyword(b"Str$", Token::StrDollar);
        check_keyword(b"String$", Token::StringDollar);
        check_keyword(b"X$", Token::StringIdentifier(ByteString(b"X")));
        check_keyword(b"\"X\"", Token::StringLiteral(ByteString(b"X")));
        check_keyword(b"Stroke", Token::Stroke);
        check_keyword(b"Sum", Token::Sum);
        check_keyword(b"Swap", Token::Swap);
        check_keyword(b"Swi", Token::Swi);
        check_keyword(b"Sys", Token::Sys);
        check_keyword(b"Tab", Token::Tab);
        check_keyword(b"Tan", Token::Tan);
        check_keyword(b"Then", Token::Then);
        check_keyword(b"Time", Token::Time);
        check_keyword(b"Tint", Token::Tint);
        check_keyword(b"To", Token::To);
        check_keyword(b"Trace", Token::Trace);
        check_keyword(b"Until", Token::Until);
        check_keyword(b"Usr", Token::Usr);
        check_keyword(b"Val", Token::Val);
        check_keyword(b"Verify", Token::Verify);
        check_keyword(b"VPos", Token::VPos);
        check_keyword(b"Wait", Token::Wait);
        check_keyword(b"While", Token::While);
        check_keyword(b"Width", Token::Width);
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
    fn old_fashioned() {
        let source = br#"10 X%=0:Y=-1.23
20 PRINT "HELLO, WORLD"
30 GOSUB 1000
40 GOTO 10
1000 X%=X%+1
1010 RETURN
        "#;
        let expected_tokens = [
            Token::DecimalIntLiteral(10),
            Token::IntegerIdentifier(ByteString(b"X")),
            Token::Assign,
            Token::DecimalIntLiteral(0),
            Token::Colon,
            Token::Identifier(ByteString(b"Y")),
            Token::Assign,
            Token::DecimalFloatLiteral(-1.23),
            Token::NewLine,
            Token::DecimalIntLiteral(20),
            Token::Print,
            Token::StringLiteral(ByteString(b"HELLO, WORLD")),
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
            Token::IntegerIdentifier(ByteString(b"X")),
            Token::Assign,
            Token::IntegerIdentifier(ByteString(b"X")),
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
    fn basic_example_from_readme() {

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
            Token::Identifier(ByteString(b"x")),
            Token::Assign,
            Token::DecimalIntLiteral(123),
            Token::NewLine,
            Token::Dim,
            Token::Identifier(ByteString(b"x")),
            Token::As,
            Token::Integer,
            Token::NewLine,
            Token::Identifier(ByteString(b"y")),
            Token::Assign,
            Token::Identifier(ByteString(b"foo")),
            Token::LeftRoundBracket,
            Token::Identifier(ByteString(b"x")),
            Token::RightRoundBracket,
            Token::NewLine,
            Token::Input,
            Token::StringIdentifier(ByteString(b"A")),
            Token::NewLine,
            Token::If,
            Token::Identifier(ByteString(b"y")),
            Token::GreaterThan,
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
