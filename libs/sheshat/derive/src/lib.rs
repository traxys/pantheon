#![feature(coverage_attribute)]
#![recursion_limit = "256"]

use std::iter::Peekable;

use proc_macro::{Delimiter, Ident, Literal, Span, TokenStream, TokenTree};

use itzamna_quote::quote;

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

enum Attribute {
    Sheshat(TokenStream),
    Doc(Literal),
}

fn get_sheshat_attribute(
    input: &mut Peekable<impl Iterator<Item = TokenTree>>,
) -> (bool, Option<Attribute>) {
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
                            (true, Some(Attribute::Sheshat(g.stream())))
                        }
                        _ => panic!("Unexpected token in sheshat attribute"),
                    }
                }
                Some(TokenTree::Ident(i)) if i.to_string() == "doc" => {
                    match attr.next().expect("malformed doc attribute") {
                        TokenTree::Punct(p) if p.as_char() == '=' => (),
                        // Not a description for the doc attribute
                        _ => return (true, None),
                    }
                    match attr.next().expect("No value for doc attribute") {
                        TokenTree::Literal(l) => (true, Some(Attribute::Doc(l))),
                        // Malformed attribute most likely, but skip it
                        _ => (true, None),
                    }
                }
                _ => (true, None),
            }
        }
        _ => (false, None),
    }
}

struct SheshatSubCommandContext {
    borrow: Option<TokenStream>,
}

fn handle_global_subcommand_attrs(source: Attribute, context: &mut SheshatSubCommandContext) {
    match source {
        Attribute::Sheshat(source) => {
            let mut source = source.into_iter().peekable();

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
        // Ignore global documentation attributes on sub commands
        Attribute::Doc(_) => (),
    }
}

fn camel_case_to_snake_case(s: &Ident) -> String {
    let s = s.to_string();
    let mut out = String::new();
    let mut chars = s.chars();
    out.push_str(&chars.next().unwrap().to_lowercase().to_string());
    for char in chars {
        if char.is_uppercase() {
            out.push('-')
        }
        out.push_str(&char.to_lowercase().to_string());
    }
    out
}

#[proc_macro_derive(SheshatSubCommand, attributes(sheshat))]
pub fn sheshat_sub_command(input: TokenStream) -> TokenStream {
    let mut source = input.into_iter().peekable();

    let mut context = SheshatSubCommandContext { borrow: None };

    loop {
        match get_sheshat_attribute(&mut source) {
            (true, None) => (),
            (true, Some(attr)) => handle_global_subcommand_attrs(attr, &mut context),
            (false, _) => break,
        }
    }
    skip_vis(&mut source);

    assert_eq!(
        source
            .next()
            .expect("Missing the `enum` definition")
            .to_string(),
        "enum"
    );

    let name = source.next().expect("Missing the enum name");
    let generics = get_generics(&mut source);
    let enum_generics = match generics.clone() {
        Some(v) => v.into_iter().collect(),
        None => TokenStream::new(),
    };

    let borrow = context
        .borrow
        .as_ref()
        .map(|s| quote!(: #s))
        .unwrap_or_default();

    let enum_generics_use = if generics.is_some() {
        quote!(< #enum_generics >)
    } else {
        quote!()
    };

    let enum_generics_declare = quote!(< 'xxx #borrow, #enum_generics >);

    let mut variants = match source.next().expect("Unexpected end of enum") {
        TokenTree::Group(g) if g.delimiter() == Delimiter::Brace => {
            g.stream().into_iter().peekable()
        }
        _ => unreachable!(),
    };

    let mut subcommands = vec![];

    loop {
        loop {
            match get_sheshat_attribute(&mut variants) {
                (true, None) => (),
                (true, Some(attr)) => match attr {
                    Attribute::Sheshat(_) => {
                        panic!("No sheshat arguments are supported by sub commands")
                    }
                    Attribute::Doc(_) => (),
                },
                (false, _) => break,
            }
        }

        let variant = match variants.next() {
            None => break,
            Some(TokenTree::Ident(field)) => field,
            Some(token) => panic!("Unknown token in variant: {token}"),
        };

        let ty = match variants.next() {
            Some(TokenTree::Group(g)) if g.delimiter() == Delimiter::Parenthesis => g.stream(),
            _ => panic!("Only enum with tuple variants are supported"),
        };

        subcommands.push((variant, ty));

        match variants.next() {
            None => break,
            Some(TokenTree::Punct(p)) if p.as_char() == ',' => continue,
            Some(t) => panic!("Unknown token after variant: {t}"),
        }
    }

    let (error_generics_declare, error_generics_use, xxx_static) = if subcommands.is_empty() {
        (TokenStream::new(), TokenStream::new(), TokenStream::new())
    } else {
        (
            enum_generics_declare.clone(),
            quote!(< 'xxx, #enum_generics >),
            quote!(where 'xxx: 'static),
        )
    };

    let (error_display, error_source) = if subcommands.is_empty() {
        let unreachable = quote!(_ => unreachable!());
        (unreachable.clone(), unreachable)
    } else {
        subcommands.iter().fold(
            (TokenStream::new(), TokenStream::new()),
            |(mut disp, mut src), (i, _)| {
                let name = camel_case_to_snake_case(i);
                disp.extend(
                    quote!(Self::#i(_) => write!(f, "Could not parse subcommand `{}`", #name),),
                );
                src.extend(quote!(Self::#i(e) => Some(e),));
                (disp, src)
            },
        )
    };

    let errors = subcommands
        .iter()
        .fold(TokenStream::new(), |mut s, (i, ty)| {
            s.extend(quote!(
                #i (
                    ::sheshat::Error <
                        'xxx,
                        <#ty as ::sheshat::Sheshat<'xxx> >::ParseErr,
                        <#ty as ::sheshat::Sheshat<'xxx> >::Name,
                    >
                ),
            ));
            s
        });

    let match_variants = subcommands
        .iter()
        .fold(TokenStream::new(), |mut s, (i, ty)| {
            let name = camel_case_to_snake_case(i);
            s.extend(quote!(
                #name => <#ty as ::sheshat::Sheshat<'xxx> >::parse_raw::<_, MMM>(&current, args, cursor)
                    .map(|v| v.map(Self::#i))
                    .map_err(Self::SubCommandErr::#i)
                    .map_err(::sheshat::SubCommandError::Parsing),
            ));
            s
        });

    let fields_description = subcommands
        .iter()
        .fold(TokenStream::new(), |mut s, (i, ty)| {
            let name = camel_case_to_snake_case(i);
            s.extend(quote!(
                write!(f, " - {}", #name)?;
                if let Some(d) = <#ty as ::sheshat::Sheshat<'xxx> >::desc() {
                    write!(f, ": {d}")?;
                }
                writeln!(f)?;
            ));
            s
        });

    quote!(
        #[derive(Debug)]
        enum #{#name Error} #error_generics_declare {
            #errors
        }

        impl #error_generics_declare core::fmt::Display for #{#name Error} #error_generics_use {
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                match self {
                    #error_display
                }
            }
        }

        impl #error_generics_declare core::error::Error for #{#name Error} #error_generics_use #xxx_static {
            fn source(&self) -> Option<&(dyn core::error::Error + 'static)> {
                match self {
                    #error_source
                }
            }
        }

        impl #enum_generics_declare ::sheshat::SheshatSubCommand<'xxx> for #name #enum_generics_use {
            type SubCommandErr = #{#name Error} #error_generics_use;

            fn parse_subcommand<'sss, 'nnn, XXX: AsRef<str>, MMM: ::sheshat::SheshatMetadataHandler>(
                prev: &'sss ::sheshat::CommandName<'sss, 'nnn>,
                subcommand: &'xxx str,
                args: ::sheshat::lex::Arguments<'xxx, XXX>,
                cursor: ::sheshat::lex::ArgCursor,
            ) -> Result<Option<Self>, ::sheshat::SubCommandError<'xxx, Self::SubCommandErr>> {
                let current = ::sheshat::CommandName {
                    prev: Some(prev),
                    name: subcommand,
                };
                match subcommand {
                    #match_variants
                    _ => Err(::sheshat::SubCommandError::UnknownSubCommand(subcommand)),
                }
            }

            fn write_usage<F: core::fmt::Write>(mut f: F) -> core::fmt::Result {
                #fields_description
                Ok(())
            }
        }
    )
}

struct SheshatContext {
    borrow: Option<TokenStream>,
    desc: Option<Literal>,
}

fn handle_global_attrs(source: Attribute, context: &mut SheshatContext) {
    match source {
        Attribute::Sheshat(source) => {
            let mut source = source.into_iter().peekable();
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
        Attribute::Doc(l) => {
            context.desc = Some(l);
        }
    }
}

#[derive(Debug)]
struct SheshatArgument {
    short: bool,
    long: bool,
    subcommand: bool,
    desc: Option<Literal>,
}

fn handle_field_attr(source: Attribute, context: &mut SheshatArgument) {
    match source {
        Attribute::Sheshat(source) => {
            let mut source = source.into_iter().peekable();
            loop {
                let ident = match source.next() {
                    None => break,
                    Some(TokenTree::Ident(i)) => i.to_string(),
                    Some(v) => panic!("Unexpected value in sheshat attribute: {v}"),
                };

                match ident.as_str() {
                    "short" => context.short = true,
                    "long" => context.long = true,
                    "subcommand" => context.subcommand = true,
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
        Attribute::Doc(l) => {
            context.desc = Some(l);
        }
    }
}

#[proc_macro_derive(Sheshat, attributes(sheshat))]
pub fn sheshat(input: TokenStream) -> TokenStream {
    let mut source = input.into_iter().peekable();

    let mut context = SheshatContext {
        borrow: None,
        desc: None,
    };

    // Parse attributes on the struct
    loop {
        match get_sheshat_attribute(&mut source) {
            (true, None) => (),
            (true, Some(attr)) => handle_global_attrs(attr, &mut context),
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
            subcommand: false,
            desc: None,
        };

        loop {
            match get_sheshat_attribute(&mut fields) {
                (true, None) => (),
                (true, Some(attr)) => handle_field_attr(attr, &mut argument),
                (false, _) => break,
            }
        }

        if argument.subcommand && (argument.short || argument.long) {
            panic!("Subcommands can't be short or long arguments")
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
            positional.push((field, argument, ty));
        }
    }

    assert!(source.peek().is_none(), "Unexpected tokens after struct");

    let struct_generics = match generics.clone() {
        Some(v) => v.into_iter().collect(),
        None => TokenStream::new(),
    };

    let struct_generics_wrapped = if generics.is_some() {
        quote!(< #struct_generics >)
    } else {
        TokenStream::new()
    };

    let option_field_names: TokenStream = options.iter().map(|(i, _, _)| quote!(#i,)).collect();

    let option_field_names_display =
        options
            .iter()
            .fold(TokenStream::new(), |mut cur, (i, _, _)| {
                cur.extend(quote!(Self::#i => write!(f, "{}", stringify!(#i)),));
                cur
            });

    let options_fields_token = options
        .iter()
        .fold(TokenStream::new(), |mut cur, (i, _, ty)| {
            cur.extend(quote!(
                let #{#i _token} = (&&&&&&To::<#ty>(Default::default()))._arg::<#ty>();
            ));
            cur
        });

    // This uses one less & in `To`, in order to skip the boolean specialization
    let positional_fields_token =
        positional
            .iter()
            .fold(TokenStream::new(), |mut cur, (i, arg, ty)| {
                if !arg.subcommand {
                    cur.extend(quote!(
                        let #{#i _token} = (&&&&&To::<#ty>(Default::default()))._arg::<#ty>();
                    ));
                }
                cur
            });

    let options_field_init_values =
        options
            .iter()
            .fold(TokenStream::new(), |mut cur, (i, _, _)| {
                cur.extend(quote!(
                    let mut #{#i _value} = #{#i _token}.init();
                ));
                cur
            });

    let positional_fields_init =
        positional
            .iter()
            .fold(TokenStream::new(), |mut cur, (i, arg, _)| {
                cur.extend(if arg.subcommand {
                    quote!(let mut #{#i _value} = None;)
                } else {
                    quote!(let mut #{#i _value} = #{#i _token}.init();)
                });
                cur
            });

    let fields_opt_desc = options
        .iter()
        .fold(TokenStream::new(), |mut s, (i, arg, _)| {
            assert!(arg.long || arg.short);

            let long_arg = i.to_string().replace('_', "-");
            let short_arg = i.to_string().chars().next().unwrap();

            let call = match arg.long {
                true => {
                    let mut a = quote!(new_long(#{#name Fields}::#i, #long_arg));
                    if arg.short {
                        a.extend(quote!(.short(#short_arg)));
                    }
                    a
                }
                false => {
                    // short && long is handled in the first arm
                    quote!(new_short(#{#name Fields}::#i, #short_arg))
                }
            };

            s.extend(quote!(
                #{#i _token}.handle_arg_desc(::sheshat::Argument::<'static, Self::Name>:: #call ),
            ));

            s
        });

    let for_each_option = |name_var: Ident, arm: fn(&Ident) -> TokenStream| -> TokenStream {
        let arms = options.iter().fold(TokenStream::new(), |mut s, (i, _, _)| {
            let matched = arm(i);
            s.extend(quote!(
                Self::Name::#i => #matched,
            ));
            s
        });

        quote!(
            match #name_var {
                Self::Name::Help => unreachable!(),
                #arms
            }
        )
    };

    let match_flag = for_each_option(
        Ident::new("name", Span::call_site()),
        |ident| quote!(#{#ident _token}.set_flag(&mut #{#ident _value})),
    );

    let match_opt = for_each_option(Ident::new("name", Span::call_site()), |ident| {
        quote!(
            #{#ident _token}.set_value(value, &mut #{#ident _value})
                .map_err(|e| ::sheshat::Error::Parsing(Self::ParseErr::sheshat_err(stringify!(#ident), e)))?
        )
    });

    let get_args = options.iter().fold(TokenStream::new(), |mut s, (i, _, _)| {
        s.extend(quote!(
            #i: #{#i _token}.get_value_named(#{#i _value}, Self::Name::#i)?,
        ));
        s
    });

    let get_pos = positional
        .iter()
        .fold(TokenStream::new(), |mut s, (i, arg, _)| {
            s.extend(if arg.subcommand {
                quote!(
                    #i: #{#i _value}.ok_or(::sheshat::Error::MissingPositional(stringify!(#i)))?,
                )
            } else {
                quote!(
                    #i: #{#i _token}.get_value_ident(#{#i _value}, stringify!(#i))?,
                )
            });
            s
        });

    let borrow = context
        .borrow
        .as_ref()
        .map(|s| quote!(: #s))
        .unwrap_or_default();

    let borrow_static = context
        .borrow
        .map(|s| quote!(#s: 'static))
        .unwrap_or_default();

    let subcommand_err = positional.iter().filter(|(_, arg, _)| arg.subcommand).fold(
        TokenStream::new(),
        |mut s, (i, _, ty)| {
            s.extend(quote!(
                #{SubCommand #i}(::sheshat::SubCommandError<
                    'xxx,
                    <#ty as ::sheshat::SheshatSubCommand<'xxx>>::SubCommandErr
                >),
            ));
            s
        },
    );

    let (subcommand_display, subcommand_source) =
        positional.iter().filter(|(_, arg, _)| arg.subcommand).fold(
            (TokenStream::new(), TokenStream::new()),
            |(mut disp, mut src), (i, _, _)| {
                disp.extend(quote!(
                    Self::#{SubCommand #i}(_) => write!(f, "Could not parse subcommand {}", stringify!(#i)),
                ));
                src.extend(quote!(
                    Self::#{SubCommand #i}(e) => Some(e),
                ));
                (disp, src)
            },
        );

    let error_phantom =
        positional
            .iter()
            .chain(options.iter())
            .fold(TokenStream::new(), |mut s, (i, _, ty)| {
                s.extend(quote!(#i: core::marker::PhantomData<#ty>,));
                s
            });

    let match_pos_arms = positional.iter().enumerate().fold(TokenStream::new(), |mut s, (idx, (id, arg, ty))| {
        s.extend(if arg.subcommand {
            quote!(
                #idx => {
                    let (sub_args, sub_cursor) = args.to_raw();
                    let Some(v) = <#ty as ::sheshat::SheshatSubCommand<'xxx>>::parse_subcommand::<_, MMM>(
                                        command_name,
                                        value,
                                        sub_args, 
                                        sub_cursor,
                                    ).map_err(|e| ::sheshat::Error::Parsing(Self::ParseErr::#{SubCommand #id}(e)))? 
                      else {
                        return Ok(None);
                    };
                    #{#id _value} = Some(v);
                    break
                }
            )
        } else {
            quote!(
                #idx => {
                    #{#id _token}.set_value(value, &mut #{#id _value})
                            .map_err(|e| ::sheshat::Error::Parsing(Self::ParseErr::sheshat_err(stringify!(#id), e)))?;
                    #{#id _token}.next_positional_field(&mut positional_index);
                }
            )
        });
        s
    });

    let (error_generics_declare, xxx_static) = if subcommand_err.is_empty() {
        (TokenStream::new(), TokenStream::new())
    } else {
        (
            quote!(<'xxx #borrow, #struct_generics>),
            quote!(where 'xxx: 'static, #borrow_static),
        )
    };

    let error_generics_use = if subcommand_err.is_empty() {
        TokenStream::new()
    } else {
        quote!(<'xxx, #struct_generics>)
    };

    let error_phantom_variant = if subcommand_err.is_empty() {
        TokenStream::new()
    } else {
        quote!(
            #[doc(hidden)]
            _Phantom {
                #error_phantom
            }
        )
    };

    let error_phantom_match = if subcommand_err.is_empty() {
        TokenStream::new()
    } else {
        quote!(Self::_Phantom { .. } => unreachable!(),)
    };

    let command_name = match &name {
        TokenTree::Ident(n) => camel_case_to_snake_case(n),
        _ => panic!("Invalid struct name"),
    };

    let mut positional_usage = TokenStream::new();
    let mut subcommand = None;
    for (i, arg, ty) in &positional {
        if arg.subcommand {
            positional_usage.extend(quote!(
                write!(f, " [COMMAND]")?;
            ));
            subcommand = Some(ty);
            break;
        } else {
            positional_usage.extend(quote!(
                write!(f, " ")?;
                #{#i _token}.positional_usage(&mut f, stringify!(#i))?;
            ));
        }
    }

    let subcommand_info = if let Some(ty) = subcommand {
        quote!(
            writeln!(f, "\nCommands:")?;
            <#ty as ::sheshat::SheshatSubCommand<'xxx>>::write_usage(&mut f)?;
        )
    } else {
        TokenStream::new()
    };

    let mut option_details = quote!(
        writeln!(f, "\nOptions:\n  -h, --help\n      Display this message and exit.")?;
    );

    for (i, arg, _) in &options {
        let mut add = |ts| option_details.extend(ts);
        add(quote!(
            write!(f, "  ")?;
        ));
        if arg.short {
            let short_arg = i.to_string().chars().next().unwrap();
            add(quote!(
                write!(f, "-{}", #short_arg)?;
            ))
        }
        if arg.long {
            if arg.short {
                add(quote!(
                    write!(f, ", ")?;
                ))
            }
            let long_arg = i.to_string().replace('_', "-");
            add(quote!(
                write!(f, "--{}", #long_arg)?;
            ))
        }
        add(quote!(
            #{#i _token}.write_opt_value(stringify!(#i), &mut f)?;
            writeln!(f)?;
        ));

        if let Some(d) = &arg.desc {
            add(quote!(
                writeln!(f, "      {}", #d.trim())?;
            ))
        }
    }

    let usage = quote!(
        fn write_usage<F: core::fmt::Write>(command_name: &::sheshat::CommandName, mut f: F)
            -> core::fmt::Result {
            pub use ::sheshat::_derive::*;

            #options_fields_token
            #positional_fields_token

            write!(f, "Usage: {command_name} [OPTIONS]")?;
            #positional_usage
            writeln!(f)?;
            if let Some(d) = Self::desc() {
                writeln!(f, "\n{d}")?;
            }
            #subcommand_info
            #option_details

            Ok(())
        }
    );

    let desc = match context.desc {
        Some(d) => quote!(
            fn desc() -> Option<&'static str> {
                Some(#d.trim())
            }
        ),
        None => TokenStream::new(),
    };

    quote!(
        #[allow(nonstandard_style)]
        #[derive(Debug, Clone, Copy)]
        pub enum #{#name Fields} {
            Help,
            #option_field_names
        }

        impl core::fmt::Display for #{#name Fields} {
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                match *self {
                    Self::Help => write!(f, "help"),
                    #option_field_names_display
                }
            }
        }

        #[derive(Debug)]
        pub enum #{#name ParseErr} #error_generics_declare {
            Parsing {from: &'static str},
            #subcommand_err
            #error_phantom_variant
        }

        impl #error_generics_declare #{#name ParseErr} #error_generics_use {
            fn sheshat_err<E>(from: &'static str, _: E) -> Self {
                Self::Parsing { from }
            }
        }

        impl #error_generics_declare core::fmt::Display for #{#name ParseErr} #error_generics_use {
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                match self {
                    Self::Parsing { from } => write!(f, "error while parsing `{from}`"),
                    #subcommand_display
                    #error_phantom_match
                }
            }
        }

        impl #error_generics_declare core::error::Error for #{#name ParseErr} #error_generics_use #xxx_static {
            fn source(&self) -> Option<&(dyn core::error::Error + 'static)> {
                match self {
                    #subcommand_source
                    #error_phantom_match
                    Self::Parsing {..} => None,
                }
            }
        }

        #[automatically_derived]
        impl<'xxx #borrow, #struct_generics> ::sheshat::Sheshat<'xxx> for #name #struct_generics_wrapped {
            type Name = #{#name Fields};
            type ParseErr = #{#name ParseErr} #error_generics_use;

            fn name() -> &'static str {
                #command_name
            }

            #usage

            #desc

            fn parse_raw<'nnn, 'sss, XXX: AsRef<str>, MMM: ::sheshat::SheshatMetadataHandler>(
                command_name: &'sss ::sheshat::CommandName<'nnn, 'sss>,
                raw_args: ::sheshat::lex::Arguments<'xxx, XXX>,
                raw_cursor: ::sheshat::lex::ArgCursor) ->
                    Result<Option<Self>, ::sheshat::Error<'xxx, Self::ParseErr, Self::Name>>
            {
                pub use ::sheshat::_derive::*;

                #options_fields_token
                #options_field_init_values
                #positional_fields_token
                #positional_fields_init

                let mut positional_index = 0;

                let desc = &[ 
                    ::sheshat::Argument::<'static, Self::Name>::new_long(Self::Name::Help, "help").short('h'),
                    #fields_opt_desc
                ];
                let mut args = ::sheshat::Arguments::from_raw(raw_args, raw_cursor, desc);
                for arg in args.by_ref() {
                    // In case we have no positional arguments
                    #[allow(unreachable_code)]
                    match arg? {
                        ::sheshat::ParsedArgument::Positional(value) => {
                            match positional_index {
                                #match_pos_arms
                                _ => return Err(::sheshat::Error::TooManyPositional),
                            };
                        },
                        ::sheshat::ParsedArgument::Flag(Self::Name::Help) => {{
                            MMM::help::<Self>(command_name);
                            return Ok(None);
                        }},
                        ::sheshat::ParsedArgument::Option(Self::Name::Help, _) => unreachable!(),
                        ::sheshat::ParsedArgument::Flag(name) => #match_flag,
                        ::sheshat::ParsedArgument::Option(name, value) => #match_opt,
                    }
                }

                Ok(Some(Self {
                    #get_args
                    #get_pos
                }))
            }
        }
    )
}
