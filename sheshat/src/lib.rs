#![doc = include_str!("../README.md")]
#![cfg_attr(not(test), no_std)]
#![feature(coverage_attribute)]

mod args;
pub mod lex;

pub use args::{Argument, Arguments, Error as ArgError, ParsedArgument};
pub use sheshat_derive::Sheshat;

#[derive(Debug)]
pub enum Error<'a, E, N> {
    Parsing(E),
    InvalidArgument(args::Error<'a, N>),
    TooManyPositional,
    MissingPositional(&'static str),
    MissingArgument(N),
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

pub trait SheshatParse: Sized {
    type Err;

    fn parse(s: &str) -> Result<Self, Self::Err>;
}

pub trait Sheshat<'a>: Sized {
    type ParseErr;
    type Name;

    fn parse_arguments<T>(args: &'a [T]) -> Result<Self, Error<'a, Self::ParseErr, Self::Name>>
    where
        T: AsRef<str>;
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
    }

    pub struct To<T>(pub PhantomData<T>);

    pub trait ViaFlagArg {
        fn _arg<T>(&self) -> FlagArg {
            FlagArg
        }
    }
    impl ViaFlagArg for &&To<bool> {}

    pub trait ViaIdOptArg {
        fn _arg<'a, T: 'a>(&self) -> IdOptArg<'a> {
            IdOptArg(Default::default())
        }
    }
    impl<'a> ViaIdOptArg for &To<&'a str> {}

    pub trait ViaParseOptArg {
        fn _arg<T>(&self) -> ParseOptArg<T> {
            ParseOptArg(Default::default())
        }
    }
    impl<T> ViaParseOptArg for To<T> where T: FromStr {}
}
