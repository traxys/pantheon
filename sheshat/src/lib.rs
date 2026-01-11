#![doc = include_str!("../README.md")]
#![cfg_attr(not(test), no_std)]
#![feature(coverage_attribute)]

mod args;
pub mod lex;

pub use args::{Argument, Arguments, Error as ArgError, ParsedArgument};
pub use sheshat_derive::{Sheshat, SheshatSubCommand};

#[derive(Debug)]
pub enum Error<'a, E, N> {
    Parsing(E),
    InvalidArgument(args::Error<'a, N>),
    TooManyPositional,
    MissingPositional(&'static str),
    MissingArgument(N),
}

impl<'a, E: core::fmt::Display, N: core::fmt::Display> core::fmt::Display for Error<'a, E, N> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Error::Parsing(_) => write!(f, "could not parse value"),
            Error::InvalidArgument(_) => write!(f, "invalid argument"),
            Error::TooManyPositional => write!(f, "too many positional arguments"),
            Error::MissingPositional(p) => write!(f, "missing positional argument `{p}`"),
            Error::MissingArgument(n) => write!(f, "missing option `{n}`"),
        }
    }
}

impl<E, N> core::error::Error for Error<'static, E, N>
where
    E: core::error::Error + 'static,
    N: core::fmt::Display + core::fmt::Debug + 'static,
{
    fn source(&self) -> Option<&(dyn core::error::Error + 'static)> {
        match self {
            Error::Parsing(e) => Some(e),
            Error::InvalidArgument(e) => Some(e),
            Error::TooManyPositional => None,
            Error::MissingPositional(_) => None,
            Error::MissingArgument(_) => None,
        }
    }
}

pub trait SheshatMetadataHandler {
    fn help<'a, T: Sheshat<'a>>(command_name: &CommandName);
}

/// Default handler discarding the metadata
pub struct NoMetadataHandler;

impl SheshatMetadataHandler for NoMetadataHandler {
    fn help<'a, T: Sheshat<'a>>(_: &CommandName) {}
}

#[derive(Debug)]
pub struct CommandName<'n, 's> {
    pub prev: Option<&'s CommandName<'n, 's>>,
    pub name: &'n str,
}

impl<'n, 's> From<&'n str> for CommandName<'n, 's> {
    fn from(name: &'n str) -> Self {
        Self { prev: None, name }
    }
}

impl<'n, 's> core::fmt::Display for CommandName<'n, 's> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        if let Some(p) = self.prev {
            write!(f, "{p} ")?;
        }
        write!(f, "{}", self.name)
    }
}

#[derive(Debug)]
pub enum Void {}

impl core::fmt::Display for Void {
    fn fmt(&self, _: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match *self {}
    }
}

impl<'a, E, N> From<args::Error<'a, N>> for Error<'a, E, N> {
    fn from(value: args::Error<'a, N>) -> Self {
        Self::InvalidArgument(value)
    }
}

pub trait Sheshat<'a>: Sized {
    type ParseErr;
    type Name;

    /// Returns Ok(None) if sheshat handled special arguments itself (help, version, ...)
    fn parse_arguments<T>(
        args: &'a [T],
    ) -> Result<Option<Self>, Error<'a, Self::ParseErr, Self::Name>>
    where
        T: AsRef<str>,
    {
        let raw_args = lex::Arguments::new(args);
        let cursor = raw_args.cursor();

        Self::parse_raw::<T, NoMetadataHandler>(&Self::name().into(), raw_args, cursor)
    }

    fn parse_arguments_metadata<T, M: SheshatMetadataHandler>(
        args: &'a [T],
    ) -> Result<Option<Self>, Error<'a, Self::ParseErr, Self::Name>>
    where
        T: AsRef<str>,
    {
        let raw_args = lex::Arguments::new(args);
        let cursor = raw_args.cursor();

        Self::parse_raw::<T, M>(&Self::name().into(), raw_args, cursor)
    }

    fn write_usage<F: core::fmt::Write>(_command_name: &CommandName, _: F) -> core::fmt::Result {
        Ok(())
    }

    fn parse_raw<'n, 's, T, M: SheshatMetadataHandler>(
        command_name: &'s CommandName<'n, 's>,
        args: lex::Arguments<'a, T>,
        cursor: lex::ArgCursor,
    ) -> Result<Option<Self>, Error<'a, Self::ParseErr, Self::Name>>
    where
        T: AsRef<str>;

    fn name() -> &'static str;

    fn desc() -> Option<&'static str> {
        None
    }
}

#[derive(Debug)]
pub enum SubCommandError<'a, E> {
    Parsing(E),
    UnknownSubCommand(&'a str),
}

impl<'a, E: core::fmt::Display> core::fmt::Display for SubCommandError<'a, E> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            SubCommandError::Parsing(e) => write!(f, "{e}"),
            SubCommandError::UnknownSubCommand(e) => write!(f, "unknown subcommand `{e}`"),
        }
    }
}

impl<'a, E> core::error::Error for SubCommandError<'a, E>
where
    E: core::error::Error + 'static,
{
    fn source(&self) -> Option<&(dyn core::error::Error + 'static)> {
        match self {
            SubCommandError::Parsing(err) => Some(err),
            SubCommandError::UnknownSubCommand(_) => None,
        }
    }
}

pub trait SheshatSubCommand<'a>: Sized {
    type SubCommandErr;

    /// Returns Ok(None) if sheshat handled special arguments itself (help, version, ...)
    fn parse_subcommand<'s, 'n, T: AsRef<str>, M: SheshatMetadataHandler>(
        prev: &'s CommandName<'s, 'n>,
        subcommand: &'a str,
        args: lex::Arguments<'a, T>,
        cursor: lex::ArgCursor,
    ) -> Result<Option<Self>, SubCommandError<'a, Self::SubCommandErr>>;

    fn write_usage<F: core::fmt::Write>(_: F) -> core::fmt::Result {
        Ok(())
    }
}

#[doc(hidden)]
pub mod _derive {
    use core::{marker::PhantomData, str::FromStr};

    use crate::{Argument, Void};

    pub struct ArgContext {
        pub short: bool,
        pub long: bool,
        pub takes_value: bool,
    }

    pub struct FlagArg;
    impl FlagArg {
        pub fn init(&self) -> bool {
            false
        }

        pub fn handle_arg_desc<N>(&self, arg: Argument<'static, N>) -> Argument<'static, N> {
            arg
        }

        pub fn set_flag(&self, value: &mut bool) {
            *value = true;
        }

        #[coverage(off)]
        pub fn set_value(&self, _value: &str, _stored: &mut bool) -> Result<(), Void> {
            unreachable!()
        }

        pub fn get_value_named<'a, E, N>(
            &self,
            value: bool,
            _name: N,
        ) -> Result<bool, super::Error<'a, E, N>> {
            Ok(value)
        }

        pub fn write_opt_value<F: core::fmt::Write>(
            &self,
            _name: &str,
            _f: F,
        ) -> core::fmt::Result {
            Ok(())
        }
    }

    pub struct IdOptArg<'a>(PhantomData<&'a ()>);
    impl<'a> IdOptArg<'a> {
        pub fn init(&self) -> Option<&'a str> {
            None
        }

        pub fn handle_arg_desc<N>(&self, arg: Argument<'static, N>) -> Argument<'static, N> {
            arg.takes_value()
        }

        #[coverage(off)]
        pub fn set_flag(&self, _: &mut Option<&str>) {
            unreachable!()
        }

        pub fn set_value(&self, value: &'a str, stored: &mut Option<&'a str>) -> Result<(), Void> {
            *stored = Some(value);
            Ok(())
        }

        pub fn get_value_named<'e, E, N>(
            &self,
            value: Option<&'a str>,
            name: N,
        ) -> Result<&'a str, super::Error<'e, E, N>> {
            value.ok_or(super::Error::MissingArgument(name))
        }

        pub fn get_value_ident<'e, E, N>(
            &self,
            value: Option<&'a str>,
            name: &'static str,
        ) -> Result<&'a str, super::Error<'e, E, N>> {
            value.ok_or(super::Error::MissingPositional(name))
        }

        pub fn next_positional_field(&self, idx: &mut usize) {
            *idx += 1;
        }

        pub fn positional_usage<F: core::fmt::Write>(
            &self,
            mut f: F,
            name: &str,
        ) -> core::fmt::Result {
            write!(f, "<{name}>")
        }

        pub fn write_opt_value<F: core::fmt::Write>(
            &self,
            name: &str,
            mut f: F,
        ) -> core::fmt::Result {
            write!(f, " <{name}>")
        }
    }

    pub struct ParseOptArg<T>(PhantomData<T>);
    impl<T> ParseOptArg<T>
    where
        T: FromStr,
    {
        pub fn init(&self) -> Option<T> {
            None
        }

        pub fn handle_arg_desc<N>(&self, arg: Argument<'static, N>) -> Argument<'static, N> {
            arg.takes_value()
        }

        #[coverage(off)]
        pub fn set_flag(&self, _: &mut Option<T>) {
            unreachable!()
        }

        pub fn set_value(&self, value: &str, stored: &mut Option<T>) -> Result<(), T::Err> {
            *stored = Some(value.parse()?);
            Ok(())
        }

        pub fn get_value_named<'e, E, N>(
            &self,
            value: Option<T>,
            name: N,
        ) -> Result<T, super::Error<'e, E, N>> {
            value.ok_or(super::Error::MissingArgument(name))
        }

        pub fn get_value_ident<'e, E, N>(
            &self,
            value: Option<T>,
            name: &'static str,
        ) -> Result<T, super::Error<'e, E, N>> {
            value.ok_or(super::Error::MissingPositional(name))
        }

        pub fn next_positional_field(&self, idx: &mut usize) {
            *idx += 1;
        }

        pub fn positional_usage<F: core::fmt::Write>(
            &self,
            mut f: F,
            name: &str,
        ) -> core::fmt::Result {
            write!(f, "<{name}>")
        }

        pub fn write_opt_value<F: core::fmt::Write>(
            &self,
            name: &str,
            mut f: F,
        ) -> core::fmt::Result {
            write!(f, " <{name}>")
        }
    }

    pub struct OptionalIdOptArg<'a>(PhantomData<&'a ()>);
    impl<'a> OptionalIdOptArg<'a> {
        pub fn init(&self) -> Option<&'a str> {
            None
        }

        pub fn handle_arg_desc<N>(&self, arg: Argument<'static, N>) -> Argument<'static, N> {
            arg.takes_value()
        }

        #[coverage(off)]
        pub fn set_flag(&self, _: &mut Option<&str>) {
            unreachable!()
        }

        pub fn set_value(&self, value: &'a str, stored: &mut Option<&'a str>) -> Result<(), Void> {
            *stored = Some(value);
            Ok(())
        }

        pub fn get_value_named<'e, E, N>(
            &self,
            value: Option<&'a str>,
            _: N,
        ) -> Result<Option<&'a str>, super::Error<'e, E, N>> {
            Ok(value)
        }

        pub fn get_value_ident<'e, E, N>(
            &self,
            value: Option<&'a str>,
            _: &'static str,
        ) -> Result<Option<&'a str>, super::Error<'e, E, N>> {
            Ok(value)
        }

        pub fn next_positional_field(&self, idx: &mut usize) {
            *idx += 1;
        }

        pub fn positional_usage<F: core::fmt::Write>(
            &self,
            mut f: F,
            name: &str,
        ) -> core::fmt::Result {
            write!(f, "[{name}]")
        }

        pub fn write_opt_value<F: core::fmt::Write>(
            &self,
            name: &str,
            mut f: F,
        ) -> core::fmt::Result {
            write!(f, " <{name}>")
        }
    }

    pub struct OptionalParseOptArg<T>(PhantomData<T>);
    impl<T> OptionalParseOptArg<Option<T>>
    where
        T: FromStr,
    {
        pub fn init(&self) -> Option<T> {
            None
        }

        pub fn handle_arg_desc<N>(&self, arg: Argument<'static, N>) -> Argument<'static, N> {
            arg.takes_value()
        }

        #[coverage(off)]
        pub fn set_flag(&self, _: &mut Option<T>) {
            unreachable!()
        }

        pub fn set_value(&self, value: &str, stored: &mut Option<T>) -> Result<(), T::Err> {
            *stored = Some(value.parse()?);
            Ok(())
        }

        pub fn get_value_named<'e, E, N>(
            &self,
            value: Option<T>,
            _: N,
        ) -> Result<Option<T>, super::Error<'e, E, N>> {
            Ok(value)
        }

        pub fn get_value_ident<'e, E, N>(
            &self,
            value: Option<T>,
            _: &'static str,
        ) -> Result<Option<T>, super::Error<'e, E, N>> {
            Ok(value)
        }

        pub fn next_positional_field(&self, idx: &mut usize) {
            *idx += 1;
        }

        pub fn positional_usage<F: core::fmt::Write>(
            &self,
            mut f: F,
            name: &str,
        ) -> core::fmt::Result {
            write!(f, "[{name}]")
        }

        pub fn write_opt_value<F: core::fmt::Write>(
            &self,
            name: &str,
            mut f: F,
        ) -> core::fmt::Result {
            write!(f, " <{name}>")
        }
    }

    pub struct SequenceIdArg<'a, S>(PhantomData<(&'a (), S)>);
    impl<'a, S> SequenceIdArg<'a, S>
    where
        S: Extend<&'a str> + Default,
    {
        pub fn init(&self) -> S {
            Default::default()
        }

        pub fn handle_arg_desc<N>(&self, arg: Argument<'static, N>) -> Argument<'static, N> {
            arg.takes_value()
        }

        #[coverage(off)]
        pub fn set_flag(&self, _: &mut S) {
            unreachable!()
        }

        pub fn set_value(&self, value: &'a str, stored: &mut S) -> Result<(), Void> {
            stored.extend(core::iter::once(value));
            Ok(())
        }

        pub fn get_value_named<'e, E, N>(
            &self,
            value: S,
            _: N,
        ) -> Result<S, super::Error<'e, E, N>> {
            Ok(value)
        }

        pub fn get_value_ident<'e, E, N>(
            &self,
            value: S,
            _: &'static str,
        ) -> Result<S, super::Error<'e, E, N>> {
            Ok(value)
        }

        pub fn next_positional_field(&self, _: &mut usize) {}

        pub fn positional_usage<F: core::fmt::Write>(
            &self,
            mut f: F,
            name: &str,
        ) -> core::fmt::Result {
            write!(f, "[{name}...]")
        }

        pub fn write_opt_value<F: core::fmt::Write>(
            &self,
            name: &str,
            mut f: F,
        ) -> core::fmt::Result {
            write!(f, " <{name}>")
        }
    }

    pub trait AppendParse<T: FromStr> {
        fn append(&mut self, v: &str) -> Result<(), T::Err>;
    }

    impl<S, T> AppendParse<T> for S
    where
        T: FromStr,
        S: Extend<T>,
    {
        fn append(&mut self, v: &str) -> Result<(), T::Err> {
            self.extend(core::iter::once(v.parse()?));
            Ok(())
        }
    }

    pub struct SequenceParseArg<S, T>(PhantomData<(S, T)>);
    impl<S, T> SequenceParseArg<S, T>
    where
        T: FromStr,
        S: AppendParse<T> + Default,
    {
        pub fn init(&self) -> S {
            Default::default()
        }

        pub fn handle_arg_desc<N>(&self, arg: Argument<'static, N>) -> Argument<'static, N> {
            arg.takes_value()
        }

        #[coverage(off)]
        pub fn set_flag(&self, _: &mut S) {
            unreachable!()
        }

        pub fn set_value(&self, value: &str, stored: &mut S) -> Result<(), T::Err> {
            stored.append(value)
        }

        pub fn get_value_named<'e, E, N>(
            &self,
            value: S,
            _: N,
        ) -> Result<S, super::Error<'e, E, N>> {
            Ok(value)
        }

        pub fn get_value_ident<'e, E, N>(
            &self,
            value: S,
            _: &'static str,
        ) -> Result<S, super::Error<'e, E, N>> {
            Ok(value)
        }

        pub fn next_positional_field(&self, _: &mut usize) {}

        pub fn positional_usage<F: core::fmt::Write>(
            &self,
            mut f: F,
            name: &str,
        ) -> core::fmt::Result {
            write!(f, "[{name}...]")
        }

        pub fn write_opt_value<F: core::fmt::Write>(
            &self,
            name: &str,
            mut f: F,
        ) -> core::fmt::Result {
            write!(f, " <{name}>")
        }
    }

    pub struct To<T>(pub PhantomData<T>);

    pub trait ViaFlagArg {
        fn _arg<T>(&self) -> FlagArg {
            FlagArg
        }
    }
    impl ViaFlagArg for &&&&&&To<bool> {}

    pub trait ViaIdOptArg {
        fn _arg<'a, T: 'a>(&self) -> IdOptArg<'a> {
            IdOptArg(Default::default())
        }
    }
    impl<'a> ViaIdOptArg for &&&&&To<&'a str> {}

    pub trait ViaOptionalIdOptArg {
        fn _arg<'a, T: 'a>(&self) -> OptionalIdOptArg<'a> {
            OptionalIdOptArg(Default::default())
        }
    }
    impl<'a> ViaOptionalIdOptArg for &&&&To<Option<&'a str>> {}

    pub trait ViaOptionalParseOptArg {
        fn _arg<T>(&self) -> OptionalParseOptArg<T> {
            OptionalParseOptArg(Default::default())
        }
    }
    impl<T> ViaOptionalParseOptArg for &&&To<Option<T>> where T: FromStr {}

    pub trait ViaSequenceIdArg {
        fn _arg<'a, S>(&self) -> SequenceIdArg<'a, S> {
            SequenceIdArg(Default::default())
        }
    }
    impl<'a, S> ViaSequenceIdArg for &&To<S> where S: Extend<&'a str> + Default {}

    pub trait Id<S> {}
    impl<S> Id<S> for S {}

    pub trait ViaSequenceParseArg<T, S> {
        fn _arg<Sp: Id<S>>(&self) -> SequenceParseArg<S, T> {
            SequenceParseArg(Default::default())
        }
    }
    impl<T, S> ViaSequenceParseArg<T, S> for &To<S>
    where
        S: IntoIterator<Item = T>,
        S: AppendParse<T>,
        T: FromStr,
    {
    }

    pub trait ViaParseOptArg {
        fn _arg<T>(&self) -> ParseOptArg<T> {
            ParseOptArg(Default::default())
        }
    }
    impl<T> ViaParseOptArg for To<T> where T: FromStr {}
}
