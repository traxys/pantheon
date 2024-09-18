use crate::lex::ShortArgument;

#[non_exhaustive]
#[derive(Debug)]
pub struct Argument<'a, N> {
    pub name: N,
    pub short: Option<char>,
    pub long: Option<&'a str>,
    pub takes_value: bool,
}

impl<'a, N> Argument<'a, N> {
    pub fn new_short(name: N, c: char) -> Self {
        Self {
            long: None,
            short: Some(c),
            name,
            takes_value: false,
        }
    }

    pub fn new_long(name: N, label: &'a str) -> Self {
        Self {
            long: Some(label),
            short: None,
            name,
            takes_value: false,
        }
    }

    pub fn takes_value(self) -> Self {
        Self {
            takes_value: true,
            ..self
        }
    }

    pub fn short(self, c: char) -> Self {
        Self {
            short: Some(c),
            ..self
        }
    }

    pub fn long(self, label: &'a str) -> Self {
        Self {
            long: Some(label),
            ..self
        }
    }
}

pub struct Arguments<'a, 'd, T, N> {
    raw: crate::lex::Arguments<'a, T>,
    cursor: crate::lex::ArgCursor,
    arguments: &'d [Argument<'d, N>],
    current_short: Option<ShortArgument<'a>>,
    opt_ended: bool,
}

impl<'a, 'd, T, N> Arguments<'a, 'd, T, N> {
    pub fn new(arguments: &'a [T], description: &'d [Argument<'d, N>]) -> Self {
        let raw = crate::lex::Arguments::new(arguments);
        let cursor = raw.cursor();
        Self::from_raw(raw, cursor, description)
    }

    pub fn from_raw(
        raw: crate::lex::Arguments<'a, T>,
        cursor: crate::lex::ArgCursor,
        description: &'d [Argument<'d, N>],
    ) -> Self {
        Self {
            raw,
            cursor,
            arguments: description,
            current_short: None,
            opt_ended: false,
        }
    }

    pub fn to_raw(self) -> (crate::lex::Arguments<'a, T>, crate::lex::ArgCursor) {
        (self.raw, self.cursor)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ParsedArgument<'a, N> {
    Positional(&'a str),
    Flag(N),
    Option(N, &'a str),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Error<'a, N> {
    Missing(N),
    SuperfluousValue(N),
    UnknownShort(char),
    UnknownLong(&'a str),
}

impl<'a, N: core::fmt::Display> core::fmt::Display for Error<'a, N> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Error::Missing(o) => write!(f, "missing value for `{o}`"),
            Error::SuperfluousValue(o) => write!(f, "unexpected value for `{o}`"),
            Error::UnknownShort(c) => write!(f, "option `-{c}` is unknown"),
            Error::UnknownLong(l) => write!(f, "option `--{l}` is unknown"),
        }
    }
}

impl<'a, 'd, T, N> Iterator for Arguments<'a, 'd, T, N>
where
    N: Clone,
    N: core::fmt::Debug,
    T: AsRef<str>,
{
    type Item = Result<ParsedArgument<'a, N>, Error<'a, N>>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.opt_ended {
                return self
                    .raw
                    .next_arg(&mut self.cursor)
                    .map(|a| Ok(ParsedArgument::Positional(a.as_value())));
            }

            let v = self.current_short.as_mut().map(|s| {
                let arg = s
                    .next()
                    .expect("self.current_short must always contain at least one argument");

                let desc = self
                    .arguments
                    .iter()
                    .find(|v| v.short == Some(arg))
                    .ok_or(Error::UnknownShort::<N>(arg))?;

                match desc.takes_value {
                    true => match s.remaining() {
                        None => {
                            let missing = || Error::Missing(desc.name.clone());
                            let next = self.raw.peek_arg(&self.cursor).ok_or_else(missing)?;

                            if next.is_long() || next.is_short() || next.is_opt_end() {
                                return Err(missing());
                            }

                            self.raw.advance(&mut self.cursor);
                            Ok(ParsedArgument::Option(desc.name.clone(), next.as_value()))
                        }
                        Some(v) => Ok(ParsedArgument::Option(desc.name.clone(), v)),
                    },
                    false => Ok(ParsedArgument::Flag(desc.name.clone())),
                }
            });

            if let Some(short) = &self.current_short {
                if short.is_empty() {
                    self.current_short = None;
                }
            }

            if v.is_some() {
                return v;
            }

            let arg = self.raw.next_arg(&mut self.cursor)?;

            if arg.is_opt_end() {
                self.opt_ended = true;
                continue;
            }

            if let Some((opt, value)) = arg.as_long() {
                let desc = match self.arguments.iter().find(|v| v.long == Some(opt)) {
                    Some(d) => d,
                    None => return Some(Err(Error::UnknownLong(opt))),
                };

                match desc.takes_value {
                    true => match value {
                        Some(v) => return Some(Ok(ParsedArgument::Option(desc.name.clone(), v))),
                        None => {
                            let missing = || Error::Missing(desc.name.clone());
                            let next = match self.raw.peek_arg(&self.cursor) {
                                Some(v) => v,
                                None => return Some(Err(missing())),
                            };

                            if next.is_long() || next.is_short() || next.is_opt_end() {
                                return Some(Err(missing()));
                            }

                            self.raw.advance(&mut self.cursor);
                            return Some(Ok(ParsedArgument::Option(
                                desc.name.clone(),
                                next.as_value(),
                            )));
                        }
                    },
                    false => match value.is_none() {
                        true => return Some(Ok(ParsedArgument::Flag(desc.name.clone()))),
                        false => return Some(Err(Error::SuperfluousValue(desc.name.clone()))),
                    },
                }
            } else if let Some(short) = arg.as_short() {
                self.current_short = Some(short);
                continue;
            } else {
                return Some(Ok(ParsedArgument::Positional(arg.as_value())));
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::{Argument, Arguments, Error, ParsedArgument};

    #[derive(Debug, PartialEq, Eq, Clone, Copy)]
    enum Name {
        Hades,
        Ymir,
    }

    use Name::*;

    #[test]
    fn empty() {
        let values = &[];
        let arg = &[Argument::new_short(Hades, 'h')];

        let parsed: Vec<_> = Arguments::<&str, _>::new(values, arg).collect();
        assert_eq!(parsed, vec![]);
    }

    #[test]
    fn opt_end() {
        let arg = &[Argument::new_short(Hades, 'h')];
        let values = &["--", "-h", "-y"];

        let parsed: Vec<_> = Arguments::new(values, arg).collect();
        assert_eq!(
            parsed,
            vec![
                Ok(ParsedArgument::Positional("-h")),
                Ok(ParsedArgument::Positional("-y"))
            ]
        );
    }

    #[test]
    fn short() {
        let arg = &[Argument::new_short(Hades, 'h')];
        let values = &["-h"];

        let parsed: Vec<_> = Arguments::new(values, arg).collect();
        assert_eq!(parsed, vec![Ok(ParsedArgument::Flag(Hades))]);
    }

    #[test]
    fn short_and_long() {
        let arg = &[Argument::new_short(Hades, 'h').long("hades")];
        let values = &["-h", "--hades"];

        let parsed: Vec<_> = Arguments::new(values, arg).collect();
        assert_eq!(
            parsed,
            vec![
                Ok(ParsedArgument::Flag(Hades)),
                Ok(ParsedArgument::Flag(Hades))
            ]
        );
    }

    #[test]
    fn long() {
        let arg = &[Argument::new_long(Hades, "hades")];
        let values = &["--hades"];

        let parsed: Vec<_> = Arguments::new(values, arg).collect();
        assert_eq!(parsed, vec![Ok(ParsedArgument::Flag(Hades))]);
    }

    #[test]
    fn long_and_short() {
        let arg = &[Argument::new_long(Hades, "hades").short('h')];
        let values = &["--hades", "-h"];

        let parsed: Vec<_> = Arguments::new(values, arg).collect();
        assert_eq!(
            parsed,
            vec![
                Ok(ParsedArgument::Flag(Hades)),
                Ok(ParsedArgument::Flag(Hades))
            ]
        );
    }

    #[test]
    fn short_value_nospace() {
        let arg = &[Argument::new_short(Hades, 'h').takes_value()];
        let values = &["-htest"];

        let parsed: Vec<_> = Arguments::new(values, arg).collect();
        assert_eq!(parsed, vec![Ok(ParsedArgument::Option(Hades, "test"))]);
    }

    #[test]
    fn short_value_space() {
        let arg = &[Argument::new_short(Hades, 'h').takes_value()];
        let values = &["-h", "test"];

        let parsed: Vec<_> = Arguments::new(values, arg).collect();
        assert_eq!(parsed, vec![Ok(ParsedArgument::Option(Hades, "test"))]);
    }

    #[test]
    fn short_value_no_val_end() {
        let arg = &[Argument::new_short(Hades, 'h').takes_value()];
        let values = &["-h"];

        let parsed: Vec<_> = Arguments::new(values, arg).collect();
        assert_eq!(parsed, vec![Err(Error::Missing(Hades))]);
    }

    #[test]
    fn short_value_no_val_next_opt_end() {
        let arg = &[Argument::new_short(Hades, 'h').takes_value()];
        let values = &["-h", "--"];

        let parsed: Vec<_> = Arguments::new(values, arg).collect();
        assert_eq!(parsed, vec![Err(Error::Missing(Hades))]);
    }

    #[test]
    fn unknown_short() {
        let arg = &[Argument::new_short(Hades, 'h')];
        let values = &["-y"];

        let parsed: Vec<_> = Arguments::new(values, arg).collect();
        assert_eq!(parsed, vec![Err(Error::UnknownShort('y'))]);
    }

    #[test]
    fn multiple_short() {
        let arg = &[
            Argument::new_short(Hades, 'h'),
            Argument::new_short(Ymir, 'y'),
        ];
        let values = &["-hyh"];

        let parsed: Vec<_> = Arguments::new(values, arg).collect();
        assert_eq!(
            parsed,
            vec![
                Ok(ParsedArgument::Flag(Hades)),
                Ok(ParsedArgument::Flag(Ymir)),
                Ok(ParsedArgument::Flag(Hades))
            ]
        );
    }

    #[test]
    fn short_value_no_val_next_short() {
        let arg = &[
            Argument::new_short(Hades, 'h').takes_value(),
            Argument::new_short(Ymir, 'y'),
        ];
        let values = &["-h", "-y"];

        let parsed: Vec<_> = Arguments::new(values, arg).collect();
        assert_eq!(
            parsed,
            vec![Err(Error::Missing(Hades)), Ok(ParsedArgument::Flag(Ymir))]
        );
    }

    #[test]
    fn short_value_no_val_next_long() {
        let arg = &[
            Argument::new_short(Hades, 'h').takes_value(),
            Argument::new_long(Ymir, "ymir"),
        ];
        let values = &["-h", "--ymir"];

        let parsed: Vec<_> = Arguments::new(values, arg).collect();
        assert_eq!(
            parsed,
            vec![Err(Error::Missing(Hades)), Ok(ParsedArgument::Flag(Ymir))]
        );
    }

    #[test]
    fn unknown_long() {
        let arg = &[Argument::new_long(Hades, "hades")];
        let values = &["--ymir"];

        let parsed: Vec<_> = Arguments::new(values, arg).collect();
        assert_eq!(parsed, vec![Err(Error::UnknownLong("ymir"))]);
    }

    #[test]
    fn long_value_space() {
        let arg = &[Argument::new_long(Hades, "hades").takes_value()];
        let values = &["--hades", "foo"];

        let parsed: Vec<_> = Arguments::new(values, arg).collect();
        assert_eq!(parsed, vec![Ok(ParsedArgument::Option(Hades, "foo"))]);
    }

    #[test]
    fn long_value_equal() {
        let arg = &[Argument::new_long(Hades, "hades").takes_value()];
        let values = &["--hades=foo"];

        let parsed: Vec<_> = Arguments::new(values, arg).collect();
        assert_eq!(parsed, vec![Ok(ParsedArgument::Option(Hades, "foo"))]);
    }

    #[test]
    fn long_value_no_val_end() {
        let arg = &[Argument::new_long(Hades, "hades").takes_value()];
        let values = &["--hades"];

        let parsed: Vec<_> = Arguments::new(values, arg).collect();
        assert_eq!(parsed, vec![Err(Error::Missing(Hades))]);
    }

    #[test]
    fn long_value_no_val_next_opt_end() {
        let arg = &[Argument::new_long(Hades, "hades").takes_value()];
        let values = &["--hades", "--"];

        let parsed: Vec<_> = Arguments::new(values, arg).collect();
        assert_eq!(parsed, vec![Err(Error::Missing(Hades))]);
    }

    #[test]
    fn long_value_no_val_next_short() {
        let arg = &[
            Argument::new_long(Hades, "hades").takes_value(),
            Argument::new_short(Ymir, 'y'),
        ];
        let values = &["--hades", "-y"];

        let parsed: Vec<_> = Arguments::new(values, arg).collect();
        assert_eq!(
            parsed,
            vec![Err(Error::Missing(Hades)), Ok(ParsedArgument::Flag(Ymir))]
        );
    }

    #[test]
    fn long_value_no_val_next_long() {
        let arg = &[
            Argument::new_long(Hades, "hades").takes_value(),
            Argument::new_long(Ymir, "ymir"),
        ];
        let values = &["--hades", "--ymir"];

        let parsed: Vec<_> = Arguments::new(values, arg).collect();
        assert_eq!(
            parsed,
            vec![Err(Error::Missing(Hades)), Ok(ParsedArgument::Flag(Ymir))]
        );
    }

    #[test]
    fn long_unexpected_value() {
        let arg = &[Argument::new_long(Hades, "hades")];
        let values = &["--hades=foo"];

        let parsed: Vec<_> = Arguments::new(values, arg).collect();
        assert_eq!(parsed, vec![Err(Error::SuperfluousValue(Hades))]);
    }

    #[test]
    fn positional() {
        let arg = &[Argument::new_long(Hades, "hades")];
        let values = &["foo"];

        let parsed: Vec<_> = Arguments::new(values, arg).collect();
        assert_eq!(parsed, vec![Ok(ParsedArgument::Positional("foo"))]);
    }

    #[test]
    fn after_opt_positional() {
        let arg = &[Argument::new_long(Hades, "hades")];
        let values = &["--hades", "foo"];

        let parsed: Vec<_> = Arguments::new(values, arg).collect();
        assert_eq!(
            parsed,
            vec![
                Ok(ParsedArgument::Flag(Hades)),
                Ok(ParsedArgument::Positional("foo"))
            ]
        );
    }
}
