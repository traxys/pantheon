use proc_macro::{Delimiter, Group, Ident, Punct, Spacing, Span, TokenStream, TokenTree};

fn append_str(ts: &mut TokenStream, s: &str) {
    ts.extend(s.parse::<TokenStream>().unwrap());
}

fn create_ts<I, T>(input: I) -> TokenStream
where
    I: IntoIterator<Item = T>,
    TokenStream: Extend<T>,
{
    let mut ts = TokenStream::new();
    ts.extend(input);
    ts
}

fn paren(ts: TokenStream) -> Group {
    Group::new(Delimiter::Parenthesis, ts)
}

fn stringify<T>(token: T) -> TokenStream
where
    TokenTree: From<T>,
{
    let mut output = TokenStream::new();
    append_str(&mut output, "stringify!");
    output.extend([paren(create_ts([TokenTree::from(token)]))]);
    output
}

fn parse_interpolation(tt: TokenTree, output: &mut TokenStream) {
    let TokenTree::Ident(i) = tt else {
        unreachable!()
    };

    append_str(
        output,
        &format!("::itzamna_quote::_rt::ExtendTokens::add(&{i}, &mut _s);"),
    );
}

fn parse_interpolation_paste(tt: TokenTree, output: &mut TokenStream) {
    let TokenTree::Group(g) = tt else {
        unreachable!()
    };

    let mut output_group = TokenStream::new();

    append_str(&mut output_group, "let mut _i = String::new();");
    let mut group = g.stream().into_iter();

    loop {
        let Some(g) = group.next() else { break };

        append_str(&mut output_group, "_i += ");

        match g {
            TokenTree::Ident(i) => {
                output_group.extend([stringify(i)]);
                output_group.extend([Punct::new(';', Spacing::Joint)]);
            }
            TokenTree::Punct(p) if p.as_char() == '#' => {
                let Some(TokenTree::Ident(i)) = group.next() else {
                    panic!("Invalid interpolation")
                };

                append_str(&mut output_group, &format!("&{i}.to_string();"))
            }
            _ => panic!("Invalid token {g:?}"),
        }
    }

    append_str(
        &mut output_group,
        "_s.extend(::itzamna_quote::_rt::ident(&_i));",
    );

    output.extend([Group::new(Delimiter::Brace, output_group)]);
    output.extend([Punct::new(';', Spacing::Alone)]);
}

#[proc_macro]
pub fn quote(input: TokenStream) -> TokenStream {
    let mut output = TokenStream::new();

    append_str(
        &mut output,
        "let mut _s = ::itzamna_quote::_rt::TokenStream::new();",
    );

    let mut input = input.into_iter().peekable();
    loop {
        let Some(inp) = input.next() else { break };

        let extend: TokenStream = match inp {
            TokenTree::Punct(p) if p.as_char() == '#' => match input.peek() {
                Some(TokenTree::Ident(_)) => {
                    parse_interpolation(input.next().unwrap(), &mut output);
                    continue;
                }
                Some(TokenTree::Group(g)) if g.delimiter() == Delimiter::Brace => {
                    parse_interpolation_paste(input.next().unwrap(), &mut output);
                    continue;
                }
                _ => format!(
                    "::itzamna_quote::_rt::punct('#', {})",
                    p.spacing() == Spacing::Joint
                )
                .parse()
                .unwrap(),
            },
            TokenTree::Punct(p) if p.as_char() == '\'' => format!(
                "::itzamna_quote::_rt::punct('\\'', {})",
                p.spacing() == Spacing::Joint
            )
            .parse()
            .unwrap(),
            TokenTree::Punct(p) => format!(
                "::itzamna_quote::_rt::punct('{}', {})",
                p.as_char(),
                p.spacing() == Spacing::Joint
            )
            .parse()
            .unwrap(),
            TokenTree::Literal(l) => {
                let mut ts = TokenStream::new();
                ts.extend([stringify(l)]);
                append_str(
                    &mut ts,
                    ".parse::<::itzamna_quote::_rt::TokenStream>().unwrap()",
                );
                ts
            }
            TokenTree::Ident(i) => {
                let mut ts = TokenStream::new();
                append_str(&mut ts, "::itzamna_quote::_rt::ident");
                ts.extend([paren(create_ts(stringify(i)))]);
                ts
            }
            TokenTree::Group(g) => {
                let mut ts = TokenStream::new();

                let group_fn = match g.delimiter() {
                    Delimiter::Parenthesis => "group_paren",
                    Delimiter::Brace => "group_brace",
                    Delimiter::Bracket => "group_bracket",
                    Delimiter::None => todo!(),
                };

                append_str(&mut ts, &format!("::itzamna_quote::_rt::{group_fn}"));

                let mut inner: TokenStream = "::itzamna_quote::quote!".parse().unwrap();
                inner.extend([paren(g.stream())]);
                ts.extend([paren(inner)]);

                ts
            }
        };

        append_str(&mut output, "_s.extend");
        output.extend([paren(extend)]);
        output.extend([Punct::new(';', Spacing::Alone)]);
    }

    output.extend([Ident::new("_s", Span::call_site())]);

    create_ts([Group::new(Delimiter::Brace, output)])
}
