use sheshat::Sheshat;

#[expect(unused)]
mod parsing {
    use sheshat_derive::Sheshat;
    use std::{marker::PhantomData, str::FromStr};

    enum GenericTest<A, B> {
        A(A),
        B(B),
    }

    impl<A, B> FromStr for GenericTest<A, B> {
        type Err = ();

        fn from_str(_: &str) -> Result<Self, Self::Err> {
            todo!()
        }
    }

    #[derive(Sheshat)]
    struct Marker;

    #[derive(Sheshat)]
    struct Empty {}

    #[derive(Sheshat)]
    pub struct Pub {}

    #[derive(Sheshat)]
    pub(crate) struct PubCrate {}

    #[derive(Sheshat)]
    pub(in crate::parsing) struct PubIn {}

    #[derive(Sheshat)]
    #[sheshat(borrow('a))]
    struct Attr<'a> {
        data: &'a str,
    }

    #[derive(Sheshat)]
    #[rustfmt::skip]
    struct NoComma { f: u64 }

    #[derive(Sheshat)]
    #[sheshat(borrow('a), borrow('a))]
    struct MultiAttr<'a> {
        data: &'a str,
    }

    #[derive(Sheshat)]
    #[cfg_attr(test, derive(Debug))]
    #[sheshat(borrow('a))]
    struct UnrelatedAttr<'a> {
        data: &'a str,
    }

    #[derive(Sheshat)]
    struct Field {
        f: u64,
    }

    #[derive(Sheshat)]
    struct UnrelatedFieldAttr {
        #[allow(unused)]
        a: u64,
    }

    #[derive(Sheshat)]
    struct GenericField {
        f: GenericTest<u64, u32>,
    }

    #[derive(Sheshat)]
    struct NestedGenericField {
        f: GenericTest<GenericTest<(), GenericTest<(), ()>>, ()>,
    }

    #[derive(Sheshat)]
    struct MultipleFields<'a> {
        f: GenericTest<&'a (), u32>,
        g: u64,
    }
}

#[test]
fn long_and_short() {
    #[derive(Sheshat, PartialEq, Eq, Debug)]
    #[sheshat(borrow('a))]
    struct Args<'a> {
        #[sheshat(short, long)]
        foo: &'a str,
        #[sheshat(long, short)]
        bar: &'a str,
    }

    assert_eq!(
        Args::parse_arguments(&["-f42", "--bar", "43"]).unwrap(),
        Args {
            foo: "42",
            bar: "43"
        }
    );

    assert_eq!(
        Args::parse_arguments(&["--foo=42", "-b", "43"]).unwrap(),
        Args {
            foo: "42",
            bar: "43"
        }
    );
}

mod subcommand {
    use sheshat::{Sheshat, SheshatSubCommand};

    #[test]
    fn missing() {
        #[derive(Sheshat, PartialEq, Eq, Debug)]
        #[sheshat(borrow('a))]
        struct Args<'a> {
            #[sheshat(subcommand)]
            sub_command: SubCommand<'a>,
        }

        #[derive(SheshatSubCommand, PartialEq, Eq, Debug)]
        #[sheshat(borrow('a))]
        enum SubCommand<'a> {
            SubCommand(SubCommandFields<'a>),
        }

        #[derive(Sheshat, PartialEq, Eq, Debug)]
        #[sheshat(borrow('a))]
        struct SubCommandFields<'a> {
            value: &'a str,
        }

        let got = Args::parse_arguments::<&str>(&[]).unwrap_err();
        assert!(
            matches!(got, sheshat::Error::MissingPositional("sub_command")),
            "Unexpected error: {got:#?}"
        );
    }

    #[test]
    fn unknown() {
        #[derive(Sheshat, PartialEq, Eq, Debug)]
        struct Args {
            #[sheshat(subcommand)]
            sub_command: SubCommand,
        }

        #[derive(SheshatSubCommand, PartialEq, Eq, Debug)]
        enum SubCommand {}

        let got = Args::parse_arguments::<&str>(&["unknown"]).unwrap_err();
        assert!(
            matches!(
                got,
                sheshat::Error::Parsing(ArgsParseErr::SubCommandsub_command(
                    sheshat::SubCommandError::UnknownSubCommand("unknown")
                ))
            ),
            "Unexpected error: {got:#?}"
        );
    }

    #[test]
    fn subcommand() {
        #[derive(Sheshat, PartialEq, Eq, Debug)]
        #[sheshat(borrow('a))]
        struct Args<'a> {
            #[sheshat(subcommand)]
            sub_command: SubCommand<'a>,
        }

        #[derive(SheshatSubCommand, PartialEq, Eq, Debug)]
        #[sheshat(borrow('a))]
        enum SubCommand<'a> {
            SubCommand(SubCommandFields<'a>),
        }

        #[derive(Sheshat, PartialEq, Eq, Debug)]
        #[sheshat(borrow('a))]
        struct SubCommandFields<'a> {
            #[sheshat(long)]
            long: &'a str,
        }

        assert_eq!(
            Args::parse_arguments(&["sub-command", "--long", "value"]).unwrap(),
            Args {
                sub_command: SubCommand::SubCommand(SubCommandFields { long: "value" })
            }
        );
    }
}

mod short {
    use sheshat::Sheshat;

    #[test]
    fn val_str() {
        #[derive(Sheshat, PartialEq, Eq, Debug)]
        #[sheshat(borrow('a))]
        struct Args<'a> {
            #[sheshat(short)]
            short: &'a str,
        }

        assert_eq!(
            Args::parse_arguments(&["-s42"]).unwrap(),
            Args { short: "42" }
        );
    }

    #[test]
    fn val_str_missing_value() {
        #[derive(Sheshat, PartialEq, Eq, Debug)]
        #[sheshat(borrow('a))]
        struct Args<'a> {
            #[sheshat(short)]
            short: &'a str,
        }

        assert!(matches!(
            Args::parse_arguments(&["-s"]).unwrap_err(),
            sheshat::Error::InvalidArgument(sheshat::ArgError::Missing(ArgsFields::short))
        ));
    }

    #[test]
    fn val_parse() {
        #[derive(Sheshat, PartialEq, Eq, Debug)]
        struct Args {
            #[sheshat(short)]
            short: u64,
        }

        assert_eq!(
            Args::parse_arguments(&["-s42"]).unwrap(),
            Args { short: 42 }
        );
    }

    #[test]
    fn val_parse_missing() {
        #[derive(Sheshat, PartialEq, Eq, Debug)]
        struct Args {
            #[sheshat(short)]
            short: u64,
        }

        let got = Args::parse_arguments::<&str>(&[]).unwrap_err();

        assert!(
            matches!(got, sheshat::Error::MissingArgument(ArgsFields::short),),
            "Unexpected error: {got:#?}"
        );
    }

    #[test]
    fn bool() {
        #[derive(Sheshat, PartialEq, Eq, Debug)]
        struct Args {
            #[sheshat(short)]
            short: bool,
        }

        assert_eq!(
            Args::parse_arguments(&["-s"]).unwrap(),
            Args { short: true }
        );
    }

    #[test]
    fn optional_id() {
        #[derive(Sheshat, PartialEq, Eq, Debug)]
        #[sheshat(borrow('a))]
        struct Args<'a> {
            #[sheshat(short)]
            arg: Option<&'a str>,
        }

        assert_eq!(
            Args::parse_arguments::<&str>(&[]).unwrap(),
            Args { arg: None }
        );
        assert_eq!(
            Args::parse_arguments::<&str>(&["-a42"]).unwrap(),
            Args { arg: Some("42") }
        );
    }

    #[test]
    fn optional_parse() {
        #[derive(Sheshat, PartialEq, Eq, Debug)]
        struct Args {
            #[sheshat(short)]
            arg: Option<u64>,
        }

        assert_eq!(
            Args::parse_arguments::<&str>(&[]).unwrap(),
            Args { arg: None }
        );
        assert_eq!(
            Args::parse_arguments::<&str>(&["-a42"]).unwrap(),
            Args { arg: Some(42) }
        );
    }
}

mod long {
    use sheshat::Sheshat;

    #[test]
    fn val_str() {
        #[derive(Sheshat, PartialEq, Eq, Debug)]
        #[sheshat(borrow('a))]
        struct Args<'a> {
            #[sheshat(long)]
            long: &'a str,
        }

        assert_eq!(
            Args::parse_arguments(&["--long=42"]).unwrap(),
            Args { long: "42" }
        );
    }

    #[test]
    fn val_parse() {
        #[derive(Sheshat, PartialEq, Eq, Debug)]
        struct Args {
            #[sheshat(long)]
            long: u64,
        }

        assert_eq!(
            Args::parse_arguments(&["--long=42"]).unwrap(),
            Args { long: 42 }
        );
    }

    #[test]
    fn bool() {
        #[derive(Sheshat, PartialEq, Eq, Debug)]
        struct Args {
            #[sheshat(long)]
            long_value: bool,
        }

        assert_eq!(
            Args::parse_arguments(&["--long-value"]).unwrap(),
            Args { long_value: true }
        );
    }
}

mod positional {
    use sheshat::Sheshat;

    #[test]
    fn val_str() {
        #[derive(Sheshat, PartialEq, Eq, Debug)]
        #[sheshat(borrow('a))]
        struct Args<'a> {
            pos: &'a str,
        }

        assert_eq!(Args::parse_arguments(&["42"]).unwrap(), Args { pos: "42" });
    }

    #[test]
    fn val_parse() {
        #[derive(Sheshat, PartialEq, Eq, Debug)]
        struct Args {
            pos: u64,
        }

        assert_eq!(Args::parse_arguments(&["42"]).unwrap(), Args { pos: 42 });
    }

    #[test]
    fn val_parse_invalid() {
        #[derive(Sheshat, PartialEq, Eq, Debug)]
        struct Args {
            pos: u64,
        }

        assert!(matches!(
            Args::parse_arguments(&["42x"]).unwrap_err(),
            sheshat::Error::Parsing(ArgsParseErr::Parsing { from: "pos" })
        ));
    }

    #[test]
    fn val_bool() {
        #[derive(Sheshat, PartialEq, Eq, Debug)]
        struct Args {
            pos: bool,
        }

        assert_eq!(
            Args::parse_arguments(&["true"]).unwrap(),
            Args { pos: true }
        );
    }

    #[test]
    fn too_many() {
        #[derive(Sheshat, PartialEq, Eq, Debug)]
        struct Args {}

        assert!(matches!(
            Args::parse_arguments(&["extra"]).unwrap_err(),
            sheshat::Error::TooManyPositional,
        ));
    }

    #[test]
    fn val_parse_missing() {
        #[derive(Sheshat, PartialEq, Eq, Debug)]
        struct Args {
            pos: u64,
        }

        let got = Args::parse_arguments::<&str>(&[]).unwrap_err();

        assert!(
            matches!(got, sheshat::Error::MissingPositional("pos")),
            "Unexpected error: {got:#?}"
        );
    }
}
