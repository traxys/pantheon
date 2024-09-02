#![doc = include_str!("../README.md")]
#![cfg_attr(not(test), no_std)]

pub mod lex;
mod args;

pub use args::{Argument, Arguments, ParsedArgument};
