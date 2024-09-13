#![feature(coverage_attribute)]

use std::{fmt::Write, iter::Peekable};

use proc_macro::{Delimiter, Ident, TokenStream, TokenTree};

#[coverage(off)]
fn assert_ident(token: &TokenTree) -> &Ident {
    match token {
        TokenTree::Ident(i) => i,
        _ => panic!("Expected an ident"),
    }
}

fn skip_vis(input: &mut Peekable<impl Iterator<Item = TokenTree>>) {
    let i = assert_ident(input.peek().unwrap());

    if i.to_string() == "pub" {
        input.next();

        // Match visibility modifier
        if let Some(TokenTree::Group(g)) = input.peek() {
            assert!(g.delimiter() == Delimiter::Parenthesis);
            input.next();
        }
    }
}

fn get_generics(input: &mut Peekable<impl Iterator<Item = TokenTree>>) -> Option<Vec<TokenTree>> {
    match input.peek() {
        Some(TokenTree::Punct(p)) if p.as_char() == '<' => {
            input.next();
            Some(
                input
                    .take_while(|t| !matches!(t, TokenTree::Punct(p) if p.as_char() == '>'))
                    .collect(),
            )
        }
        _ => None,
    }
}

struct SheshatContext {
    borrow: Option<TokenStream>,
}

fn handle_global_attrs(source: impl Iterator<Item = TokenTree>, context: &mut SheshatContext) {
    let mut source = source.peekable();
    loop {
        let ident = match source.next() {
            None => break,
            Some(TokenTree::Ident(i)) => i.to_string(),
            Some(v) => panic!("Unexpected value in sheshat attribute: {v}"),
        };

        match ident.as_str() {
            "borrow" => match source.next() {
                Some(TokenTree::Group(g)) if g.delimiter() == Delimiter::Parenthesis => {
                    context.borrow = Some(g.stream());
                }
                Some(t) => panic!("Unexpected token for borrow: {t}"),
                None => panic!("Unexpected end for borrow"),
            },
            _ => panic!("Unknown sheshat attribute: {ident}"),
        }

        match source.peek() {
            Some(TokenTree::Punct(p)) if p.as_char() == ',' => {
                source.next();
            }
            _ => (),
        }
    }
}

fn get_sheshat_attribute(
    input: &mut Peekable<impl Iterator<Item = TokenTree>>,
) -> (bool, Option<TokenStream>) {
    match input.peek() {
        Some(TokenTree::Punct(p)) if p.as_char() == '#' => {
            input.next();
            let mut attr = match input.next().expect("Unexpected end of macro") {
                TokenTree::Group(g) if g.delimiter() == Delimiter::Bracket => {
                    g.stream().into_iter()
                }
                _ => panic!("Unexpected token in struct attributes"),
            };

            match attr.next() {
                Some(TokenTree::Ident(i)) if i.to_string() == "sheshat" => {
                    match attr.next().expect("Unexpected end of sheshat attributes") {
                        TokenTree::Group(g) if g.delimiter() == Delimiter::Parenthesis => {
                            (true, Some(g.stream()))
                        }
                        _ => panic!("Unexpected token in sheshat attribute"),
                    }
                }
                _ => (true, None),
            }
        }
        _ => (false, None),
    }
}

#[derive(Debug)]
struct SheshatArgument {
    short: bool,
    long: bool,
}

fn handle_field_attr(source: impl Iterator<Item = TokenTree>, context: &mut SheshatArgument) {
    let mut source = source.peekable();
    loop {
        let ident = match source.next() {
            None => break,
            Some(TokenTree::Ident(i)) => i.to_string(),
            Some(v) => panic!("Unexpected value in sheshat attribute: {v}"),
        };

        match ident.as_str() {
            "short" => context.short = true,
            "long" => context.long = true,
            _ => panic!("Unknown sheshat attribute: {ident}"),
        }

        match source.peek() {
            Some(TokenTree::Punct(p)) if p.as_char() == ',' => {
                source.next();
            }
            _ => (),
        }
    }
}

#[proc_macro_derive(Sheshat, attributes(sheshat))]
pub fn sheshat(input: TokenStream) -> TokenStream {
    let mut source = input.into_iter().peekable();

    let mut context = SheshatContext { borrow: None };

    // Parse attributes on the struct
    loop {
        match get_sheshat_attribute(&mut source) {
            (true, None) => (),
            (true, Some(attr)) => handle_global_attrs(attr.into_iter(), &mut context),
            (false, _) => break,
        }
    }

    skip_vis(&mut source);
    assert_eq!(
        source
            .next()
            .expect("Missing the `struct` definition")
            .to_string(),
        "struct"
    );

    let name = source.next().expect("Missing the struct name");
    let generics = get_generics(&mut source);

    let mut fields = match source.next().expect("Unexpected end of struct") {
        TokenTree::Group(g) if g.delimiter() == Delimiter::Brace => {
            g.stream().into_iter().peekable()
        }
        TokenTree::Group(g) if g.delimiter() == Delimiter::Bracket => {
            panic!("Only struct with named fields are supported")
        }
        TokenTree::Punct(p) if p.as_char() == ';' => TokenStream::new().into_iter().peekable(),
        _ => unreachable!(),
    };

    let mut positional = Vec::new();
    let mut options = Vec::new();

    // Loop over each field in the struct
    loop {
        let mut argument = SheshatArgument {
            short: false,
            long: false,
        };

        loop {
            match get_sheshat_attribute(&mut fields) {
                (true, None) => (),
                (true, Some(attr)) => handle_field_attr(attr.into_iter(), &mut argument),
                (false, _) => break,
            }
        }

        let field = match fields.next() {
            None => break,
            Some(TokenTree::Ident(field)) => field,
            Some(token) => panic!("Unknown token in macro: {token}"),
        };
        assert_eq!(
            fields.next().expect("Unexpected end of field").to_string(),
            ":"
        );
        let mut generic_depth = 0;

        let mut ty = TokenStream::new();
        loop {
            let next = fields.next();
            match &next {
                None => break,
                Some(TokenTree::Punct(p)) if p.as_char() == ',' && generic_depth == 0 => break,
                Some(TokenTree::Punct(p)) if p.as_char() == '<' => generic_depth += 1,
                Some(TokenTree::Punct(p)) if p.as_char() == '>' => generic_depth -= 1,
                _ => (),
            }
            ty.extend([next.unwrap()]);
        }

        if argument.short || argument.long {
            options.push((field, argument, ty))
        } else {
            positional.push((field, ty));
        }
    }

    assert!(source.peek().is_none(), "Unexpected tokens after struct");

    let struct_generics = match generics.clone() {
        Some(v) => v.into_iter().collect(),
        None => TokenStream::new(),
    };

    let struct_generics_wrapped = if generics.is_some() {
        format!("<{struct_generics}>")
    } else {
        "".into()
    };

    let option_field_names: String = options
        .iter()
        .map(|(i, _, _)| i.to_string() + ",")
        .collect();

    let options_fields_init: String = options.iter().fold(String::new(), |mut cur, (i, _, ty)| {
        let _ = writeln!(
            cur,
            "let {i}_token = (&&To::<{ty}>(Default::default()))._arg::<{ty}>();
             let mut {i}_value = {i}_token.init();"
        );
        cur
    });

    // This uses one less & in `To`, in order to skip the boolean specialization
    let positional_fields_init: String =
        positional.iter().fold(String::new(), |mut cur, (i, ty)| {
            let _ = writeln!(
                cur,
                "let {i}_token = (&To::<{ty}>(Default::default()))._arg::<{ty}>();
                 let mut {i}_value = {i}_token.init();"
            );
            cur
        });

    let fields_opt_desc: String = options.iter().fold(String::new(), |mut s, (i, arg, _)| {
        let _ = write!(
            s,
            "{i}_token.handle_arg_desc(::sheshat::Argument::<'static, Self::Name>::new_"
        );
        let long_arg = format!("\"{i}\"");
        let short_arg = format!("'{}'", i.to_string().chars().next().unwrap());

        assert!(arg.long || arg.short);
        match arg.long {
            true => {
                let _ = write!(s, "long({name}Fields::{i}, {long_arg})");
                if arg.short {
                    let _ = write!(s, ".short({short_arg})");
                }
            }
            false => {
                let _ = write!(s, "short({name}Fields::{i}, {short_arg})");
                // short && long is handled in the first arm
            }
        }

        let _ = write!(s, "),");

        s
    });

    let for_each_option = |name_var: &str, arm: fn(&str) -> String| -> String {
        format!("match {name_var} {{")
            + &options.iter().fold(String::new(), |mut s, (i, _, _)| {
                let _ = write!(s, "Self::Name::{i} => {},", arm(&i.to_string()));
                s
            })
            + "}"
    };

    let match_flag = for_each_option("name", |ident| {
        format!("{ident}_token.set_flag(&mut {ident}_value)")
    });

    let match_opt = for_each_option("name", |ident| {
        format!(
            "
            {ident}_token.set_value(value, &mut {ident}_value)
                .map_err(|e| ::sheshat::Error::Parsing(Self::ParseErr::sheshat_err(\"{ident}\", e)))?
            "
        )
    });

    let get_args = options.iter().fold(String::new(), |mut s, (i, _, _)| {
        let _ = write!(
            s,
            "{i}: {i}_token.get_value_named({i}_value, Self::Name::{i})?,"
        );
        s
    });

    let get_pos = positional.iter().fold(String::new(), |mut s, (i, _)| {
        let _ = write!(
            s,
            "{i}: {i}_token.get_value_ident({i}_value, stringify!({i}))?,"
        );
        s
    });

    let borrow = context
        .borrow
        .map(|s| ":".to_string() + &s.to_string())
        .unwrap_or_default();

    let match_pos_arms = positional.iter().enumerate().fold(String::new(), |mut s, (idx, (id, _))| {
        let _ = write!(s, "
            {idx} => {id}_token.set_value(value, &mut {id}_value)
                        .map_err(|e|  ::sheshat::Error::Parsing(Self::ParseErr::sheshat_err(\"{id}\", e)))?,"
        );
        s
    });

    let output = format!(
        r#"
            #[derive(Debug, Clone, Copy)]
            pub enum {name}Fields {{
                {option_field_names}
            }}

            #[derive(Debug)]
            pub struct {name}ParseErr {{
                pub from: &'static str,
            }}

            impl {name}ParseErr {{
                fn sheshat_err<E>(from: &'static str, _: E) -> Self {{
                    Self {{ from }}
                }}
            }}

            #[automatically_derived]
            impl<'xxx{borrow}, {struct_generics}> ::sheshat::Sheshat<'xxx> for {name}{struct_generics_wrapped} {{
                type Name = {name}Fields;
                type ParseErr = {name}ParseErr;

                fn parse_arguments<XXX: AsRef<str>>(args: &'xxx [XXX]) -> 
                    Result<Self, ::sheshat::Error<'xxx, Self::ParseErr, Self::Name>> 
                {{
                    pub use ::sheshat::_derive::*;

                    {options_fields_init}
                    {positional_fields_init}

                    let mut positional_index = 0;

                    let desc: &[::sheshat::Argument<'static, Self::Name>] = &[{fields_opt_desc}];
                    for arg in ::sheshat::Arguments::new(args, desc) {{
                        // In case we have no positional arguments
                        #[allow(unreachable_code)]
                        match arg? {{
                            ::sheshat::ParsedArgument::Positional(value) => {{
                                match positional_index {{
                                    {match_pos_arms}
                                    _ => return Err(::sheshat::Error::TooManyPositional),
                                }};
                                positional_index += 1;
                            }},
                            ::sheshat::ParsedArgument::Flag(name) => {match_flag},
                            ::sheshat::ParsedArgument::Option(name, value) => {match_opt},
                        }}
                    }}

                    Ok(Self {{
                        {get_args}
                        {get_pos}
                    }})
                }}
            }}
        "#
    );

    output.parse().unwrap()
}
