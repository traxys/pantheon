use std::collections::VecDeque;

use crate::{
    Source,
    parser::span::{Location, SpannedValue},
};

#[derive(Debug)]
pub struct UnexpectedToken {
    got: String,
    expected: Vec<TokenKind>,
    location: Location,
    position: String,
}

impl std::fmt::Display for UnexpectedToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Unexpected token {} for {}, expected ",
            self.got, self.position
        )?;

        if self.expected.len() == 1 {
            write!(f, "{}", self.expected[0])?;
        } else {
            for e in self.expected.iter().take(self.expected.len() - 2) {
                write!(f, "{e}, ")?;
            }

            write!(
                f,
                "{} or {}",
                self.expected[self.expected.len() - 2],
                self.expected[self.expected.len() - 1],
            )?;
        }

        writeln!(f, "\n")?;

        self.location.render_context(1, f)
    }
}

impl UnexpectedToken {
    pub fn at(token: Token, expected: Vec<TokenKind>, position: String) -> Self {
        Self {
            got: token.to_string(),
            expected,
            location: token.value.span(),
            position,
        }
    }
}

#[derive(Debug)]
pub enum TokenError {
    Unexpected(UnexpectedToken),
    EndOfInput(Source),
    EscapeSequence { c: char, at: Location },
    UnterminatedString(Source),
    Invalid { value: String, at: Location },
}

impl std::fmt::Display for TokenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenError::Unexpected(unexpected_token) => write!(f, "{unexpected_token}"),
            TokenError::EndOfInput(source) => write!(
                f,
                "Unexpected end of input in '{}'",
                source.path.to_string_lossy()
            ),
            TokenError::EscapeSequence { c, at } => write!(
                f,
                "Invalid escape sequence `{}` in '{}'",
                c,
                at.source.path.to_string_lossy()
            ),
            TokenError::UnterminatedString(source) => write!(
                f,
                "Unterminated string in '{}'",
                source.path.to_string_lossy()
            ),
            TokenError::Invalid { value, at } => write!(
                f,
                "Invalid token `{}` in '{}'",
                value,
                at.source.path.to_string_lossy()
            ),
        }
    }
}

impl std::error::Error for TokenError {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    String,
    Module,
    Identifier,
    Semicolon,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Comma,
    Colon,
    Equal,
    PathSep,
    Directive,
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::String => write!(f, "string litteral"),
            TokenKind::Module => write!(f, "`mod`"),
            TokenKind::Identifier => write!(f, "identifier"),
            TokenKind::Semicolon => write!(f, "`;`"),
            TokenKind::LBrace => write!(f, "`{{`"),
            TokenKind::RBrace => write!(f, "`}}`"),
            TokenKind::LBracket => write!(f, "`[`"),
            TokenKind::RBracket => write!(f, "`]`"),
            TokenKind::Comma => write!(f, "`,`"),
            TokenKind::Colon => write!(f, "`:`"),
            TokenKind::Equal => write!(f, "`=`"),
            TokenKind::PathSep => write!(f, "`::`"),
            TokenKind::Directive => write!(f, "directive"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub value: SpannedValue<&'a str>,
}

impl std::fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            TokenKind::Module
            | TokenKind::Comma
            | TokenKind::Colon
            | TokenKind::PathSep
            | TokenKind::Semicolon
            | TokenKind::LBrace
            | TokenKind::LBracket
            | TokenKind::RBracket
            | TokenKind::Equal
            | TokenKind::RBrace
            | TokenKind::Directive => write!(f, "{}", self.value.v),
            TokenKind::Identifier => write!(f, "identifier:'{}'", self.value.v),
            TokenKind::String => write!(f, "\"{}\"", self.value.v),
        }
    }
}

impl Token<'_> {
    pub fn escaped_string(&self) -> String {
        assert_eq!(self.kind, TokenKind::String);

        let mut value = String::new();

        let mut escape = false;
        for c in self.value.v.chars().skip(1) {
            if escape {
                escape = false;
            } else if c == '\\' {
                escape = true;
                continue;
            }

            value.push(c);
        }

        assert_eq!(value.pop(), Some('\''));

        value
    }
}

pub struct TokenParser<'a> {
    source: &'a Source,
    /// Should never point at whitespace or a comment
    offset: usize,
    consumed_offset: usize,
    last_token_end: Option<usize>,
    peeked: VecDeque<Token<'a>>,
}

pub struct TokenPeek<'t, 'a> {
    current: usize,
    parser: &'t mut TokenParser<'a>,
}

impl<'t, 'a> TokenPeek<'t, 'a> {
    pub fn peek(&mut self) -> Result<TokenKind, TokenError> {
        if self.parser.peeked.len() == self.current {
            let next = self.parser.next_token()?;
            self.parser.peeked.push_back(next);
        }

        let elem = &self.parser.peeked[self.current];
        self.current += 1;
        Ok(elem.kind)
    }
}

impl<'a> TokenParser<'a> {
    pub fn parse_seq<T, F, E>(&mut self, f: F) -> Result<SpannedValue<T>, E>
    where
        F: FnOnce(&mut TokenParser<'a>) -> Result<T, E>,
    {
        let start = self.consumed_offset;
        let value = f(self)?;
        let end = self.last_token_end.unwrap();

        Ok(SpannedValue {
            location: Location {
                start,
                end,
                source: self.source.clone(),
            },
            v: value,
        })
    }

    fn tail(&self) -> &'a str {
        &self.source.data[self.offset..]
    }

    fn consume(&mut self, length: usize) -> SpannedValue<&'a str> {
        let tail = self.tail();
        let start = self.offset;
        self.offset += length;
        self.last_token_end = Some(start + length);
        self.advance_to_next();
        SpannedValue {
            location: Location {
                start,
                end: self.last_token_end.unwrap(),
                source: self.source.clone(),
            },
            v: &tail[..length],
        }
    }

    fn advance_to_next(&mut self) {
        loop {
            self.offset += self
                .tail()
                .find(|c: char| !c.is_whitespace())
                .unwrap_or(self.tail().len());

            match self.tail().starts_with('#') {
                true => match self.tail().find('\n') {
                    None => self.offset = self.source.data.len(),
                    Some(n) => self.offset += n,
                },
                false => break,
            }
        }
    }

    pub fn new(source: &'a Source) -> Self {
        let mut this = Self {
            offset: 0,
            consumed_offset: 0,
            last_token_end: None,
            source,
            peeked: VecDeque::new(),
        };
        this.advance_to_next();
        this
    }

    pub fn is_empty(&self) -> bool {
        self.peeked.is_empty() && self.tail().is_empty()
    }

    pub fn next_identifier(&mut self, position: &str) -> Result<SpannedValue<&'a str>, TokenError> {
        let token = self.next()?;

        match token.kind {
            TokenKind::Identifier => Ok(token.value),
            _ => Err(TokenError::Unexpected(UnexpectedToken::at(
                token,
                vec![TokenKind::Identifier],
                position.to_owned(),
            ))),
        }
    }

    pub fn assert_next(&mut self, expected: TokenKind, position: &str) -> Result<(), TokenError> {
        let token = self.next()?;

        if token.kind == expected {
            Ok(())
        } else {
            Err(TokenError::Unexpected(UnexpectedToken::at(
                token,
                vec![expected],
                position.to_owned(),
            )))
        }
    }

    pub fn peek_cursor<'t>(&'t mut self) -> TokenPeek<'t, 'a> {
        TokenPeek {
            current: 0,
            parser: self,
        }
    }

    pub fn next(&mut self) -> Result<Token<'a>, TokenError> {
        let token = match self.peeked.pop_front() {
            Some(token) => token,
            None => self.next_token()?,
        };

        self.consumed_offset = token.value.location.end;

        Ok(token)
    }

    fn next_token(&mut self) -> Result<Token<'a>, TokenError> {
        if self.is_empty() {
            return Err(TokenError::EndOfInput(self.source.clone()));
        }

        const SYMBOLS: &[(&str, TokenKind)] = &[
            ("::", TokenKind::PathSep),
            ("{", TokenKind::LBrace),
            ("}", TokenKind::RBrace),
            (";", TokenKind::Semicolon),
            (",", TokenKind::Comma),
            (":", TokenKind::Colon),
            ("=", TokenKind::Equal),
            ("[", TokenKind::LBracket),
            ("]", TokenKind::RBracket),
        ];

        for &(symbol, kind) in SYMBOLS {
            if self.tail().starts_with(symbol) {
                return Ok(Token {
                    kind,
                    value: self.consume(symbol.len()),
                });
            }
        }

        if self.tail().starts_with('\'') {
            let mut escape = false;
            let mut end = None;

            for (i, c) in self.tail().char_indices().skip(1) {
                if escape {
                    if c != '\\' && c != '\'' {
                        return Err(TokenError::EscapeSequence {
                            c,
                            at: Location {
                                start: self.offset + i,
                                end: self.offset + i + 1,
                                source: self.source.clone(),
                            },
                        });
                    }

                    escape = false;
                } else if c == '\\' {
                    escape = true;
                } else if c == '\'' {
                    end = Some(i);
                    break;
                }
            }

            return match end {
                Some(idx) => Ok(Token {
                    kind: TokenKind::String,
                    value: self.consume(idx + 1),
                }),
                None => Err(TokenError::UnterminatedString(self.source.clone())),
            };
        }

        let end = SYMBOLS
            .iter()
            .filter_map(|(s, _)| self.tail().find(s))
            .min()
            .unwrap_or(self.tail().len())
            .min(
                self.tail()
                    .find(char::is_whitespace)
                    .unwrap_or(self.tail().len()),
            );

        let word = &self.tail()[0..end];

        if word.starts_with('@')
            && word.len() > 1
            && word.as_bytes()[1..]
                .iter()
                .all(|&b| b.is_ascii_alphanumeric() || b == b'-')
        {
            return Ok(Token {
                kind: TokenKind::Directive,
                value: self.consume(word.len()),
            });
        }

        let keywords = [("mod", TokenKind::Module)];

        for (key, kind) in keywords {
            if key == word {
                return Ok(Token {
                    kind,
                    value: self.consume(key.len()),
                });
            }
        }

        if word
            .as_bytes()
            .iter()
            .all(|&b| b.is_ascii_alphanumeric() || b == b'_' || b == b'-')
            && word.as_bytes()[0].is_ascii_alphabetic()
        {
            return Ok(Token {
                kind: TokenKind::Identifier,
                value: self.consume(word.len()),
            });
        }

        let err = Err(TokenError::Invalid {
            value: word.to_owned(),
            at: Location {
                start: self.offset,
                end: self.offset + word.len(),
                source: self.source.clone(),
            },
        });
        self.offset = self.source.data.len();
        err
    }
}
