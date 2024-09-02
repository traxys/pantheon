#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Arguments<'a, T>(&'a [T]);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ArgCursor(usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ParsedArgument<'a>(&'a str);

#[derive(Clone, Debug)]
pub struct ShortArgument<'a> {
    value: &'a str,
    prefix: core::str::CharIndices<'a>,
}

impl<'a, T> Arguments<'a, T> {
    pub fn new(items: &'a [T]) -> Self {
        Self(items)
    }

    pub fn cursor(&self) -> ArgCursor {
        ArgCursor(0)
    }
}

impl<'a, T> Arguments<'a, T>
where
    T: AsRef<str>,
{
    pub fn next_arg(&self, cursor: &mut ArgCursor) -> Option<ParsedArgument<'a>> {
        let arg = self.peek_arg(cursor)?;
        self.advance(cursor);
        Some(arg)
    }

    pub fn peek_arg(&self, cursor: &ArgCursor) -> Option<ParsedArgument<'a>> {
        self.0.get(cursor.0).map(|v| ParsedArgument(v.as_ref()))
    }

    pub fn advance(&self, cursor: &mut ArgCursor) {
        cursor.0 = cursor.0.saturating_add(1);
    }
}

impl<'a> ParsedArgument<'a> {
    pub fn is_opt_end(&self) -> bool {
        self.0 == "--"
    }

    pub fn is_long(&self) -> bool {
        self.0.len() > 2 && self.0.starts_with("--")
    }

    pub fn is_short(&self) -> bool {
        self.0.len() > 1 && self.0.starts_with("-") && !self.0.starts_with("--")
    }

    pub fn as_long(&self) -> Option<(&'a str, Option<&'a str>)> {
        let arg = self.0.strip_prefix("--")?;

        if arg.is_empty() {
            return None;
        }

        match arg.split_once('=') {
            Some((name, value)) => Some((name, Some(value))),
            None => Some((arg, None)),
        }
    }

    pub fn as_short(&self) -> Option<ShortArgument<'a>> {
        let arg = self.0.strip_prefix('-')?;

        if arg.is_empty() || arg.starts_with('-') {
            return None;
        }

        Some(ShortArgument {
            prefix: arg.char_indices(),
            value: arg,
        })
    }

    pub fn as_value(&self) -> &'a str {
        self.0
    }
}

impl<'a> Iterator for ShortArgument<'a> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        self.prefix.next().map(|(_, v)| v)
    }
}

impl<'a> ShortArgument<'a> {
    pub fn remaining(&mut self) -> Option<&'a str> {
        let (next, _) = self.prefix.next()?;

        self.prefix = "".char_indices();
        Some(&self.value[next..])
    }

    pub fn is_empty(&self) -> bool {
        self.prefix.as_str().is_empty()
    }
}

#[cfg(test)]
mod short {
    use super::Arguments;

    #[test]
    pub fn arg_end() {
        let args = Arguments::new(&["--"]);

        let mut cursor = args.cursor();

        let parsed = args.next_arg(&mut cursor).unwrap();

        assert!(!parsed.is_short());
        assert!(parsed.as_short().is_none());
    }

    #[test]
    pub fn is() {
        let args = Arguments::new(&["-short"]);

        let mut cursor = args.cursor();

        let parsed = args.next_arg(&mut cursor).unwrap();

        assert!(parsed.is_short());
    }

    #[test]
    pub fn stdio() {
        let args = Arguments::new(&["-"]);

        let mut cursor = args.cursor();

        let parsed = args.next_arg(&mut cursor).unwrap();

        assert!(parsed.as_short().is_none());
        assert!(!parsed.is_short());
    }

    #[test]
    pub fn many() {
        let args = Arguments::new(&["-short"]);

        let mut cursor = args.cursor();

        let parsed = args.next_arg(&mut cursor).unwrap();
        let mut short = parsed.as_short().unwrap();

        for flag in "short".chars() {
            assert_eq!(short.next(), Some(flag));
        }
        assert_eq!(short.next(), None);
    }

    #[test]
    pub fn value() {
        let args = Arguments::new(&["-short"]);

        let mut cursor = args.cursor();

        let parsed = args.next_arg(&mut cursor).unwrap();
        let mut short = parsed.as_short().unwrap();

        assert_eq!(short.next(), Some('s'));
        assert_eq!(short.remaining(), Some("hort"));
        assert_eq!(short.remaining(), None);
        assert_eq!(short.next(), None);
    }

    #[test]
    pub fn non_empty() {
        let args = Arguments::new(&["-short"]);

        let mut cursor = args.cursor();

        let parsed = args.next_arg(&mut cursor).unwrap();
        let short = parsed.as_short().unwrap();

        assert!(!short.is_empty());
    }

    #[test]
    pub fn is_empty() {
        let args = Arguments::new(&["-short"]);

        let mut cursor = args.cursor();

        let parsed = args.next_arg(&mut cursor).unwrap();
        let mut short = parsed.as_short().unwrap();

        short.by_ref().for_each(|_| ());

        assert!(short.is_empty());
    }

    #[test]
    pub fn empty() {
        let args = Arguments::new(&[""]);

        let mut cursor = args.cursor();

        let parsed = args.next_arg(&mut cursor).unwrap();

        assert!(parsed.as_short().is_none());
        assert!(!parsed.is_short());
    }

    #[test]
    pub fn long_as_short() {
        let args = Arguments::new(&["--long"]);

        let mut cursor = args.cursor();

        let parsed = args.next_arg(&mut cursor).unwrap();

        assert!(parsed.as_short().is_none());
        assert!(!parsed.is_short());
    }
}

#[cfg(test)]
mod long {
    use super::Arguments;

    #[test]
    pub fn is() {
        let args = Arguments::new(&["--long"]);

        let mut cursor = args.cursor();

        let parsed = args.next_arg(&mut cursor).unwrap();

        assert!(parsed.is_long());
    }

    #[test]
    pub fn flag() {
        let args = Arguments::new(&["--long"]);

        let mut cursor = args.cursor();

        let parsed = args.next_arg(&mut cursor).unwrap();
        let (arg, value) = parsed.as_long().unwrap();

        assert_eq!(arg, "long");
        assert_eq!(value, None);
    }

    #[test]
    pub fn value() {
        let args = Arguments::new(&["--long=value"]);

        let mut cursor = args.cursor();

        let parsed = args.next_arg(&mut cursor).unwrap();
        let (arg, value) = parsed.as_long().unwrap();

        assert_eq!(arg, "long");
        assert_eq!(value, Some("value"));
    }

    #[test]
    pub fn arg_end() {
        let args = Arguments::new(&["--"]);

        let mut cursor = args.cursor();

        let parsed = args.next_arg(&mut cursor).unwrap();

        assert_eq!(parsed.as_long(), None);
        assert!(!parsed.is_long());
        assert!(parsed.is_opt_end());
    }
}
