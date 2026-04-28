extern crate proc_macro;

pub use itzamna_quote_impl::quote;

#[doc(hidden)]
pub mod _rt {
    use proc_macro::{Group, Literal};
    pub use proc_macro::{Ident, Punct, Spacing, Span, TokenStream, TokenTree};

    pub fn group_paren(inner: TokenStream) -> [Group; 1] {
        [Group::new(proc_macro::Delimiter::Parenthesis, inner)]
    }

    pub fn group_brace(inner: TokenStream) -> [Group; 1] {
        [Group::new(proc_macro::Delimiter::Brace, inner)]
    }

    pub fn group_bracket(inner: TokenStream) -> [Group; 1] {
        [Group::new(proc_macro::Delimiter::Bracket, inner)]
    }

    pub fn ident(i: &str) -> [Ident; 1] {
        [Ident::new(i, Span::call_site())]
    }

    pub fn punct(c: char, joint: bool) -> [Punct; 1] {
        [Punct::new(
            c,
            if joint {
                Spacing::Joint
            } else {
                Spacing::Alone
            },
        )]
    }

    pub fn punct_two(f: char, s: char) -> [Punct; 2] {
        [Punct::new(f, Spacing::Joint), Punct::new(s, Spacing::Alone)]
    }

    pub trait ExtendTokens {
        fn add(&self, tokens: &mut TokenStream);
    }

    impl ExtendTokens for TokenStream {
        fn add(&self, tokens: &mut TokenStream) {
            tokens.extend(self.clone());
        }
    }

    impl ExtendTokens for TokenTree {
        fn add(&self, tokens: &mut TokenStream) {
            tokens.extend([self.clone()]);
        }
    }

    impl ExtendTokens for Ident {
        fn add(&self, tokens: &mut TokenStream) {
            tokens.extend([self.clone()]);
        }
    }

    impl ExtendTokens for Literal {
        fn add(&self, tokens: &mut TokenStream) {
            tokens.extend([self.clone()]);
        }
    }

    impl ExtendTokens for usize {
        fn add(&self, tokens: &mut TokenStream) {
            tokens.extend([Literal::usize_suffixed(*self)]);
        }
    }

    impl ExtendTokens for char {
        fn add(&self, tokens: &mut TokenStream) {
            tokens.extend([Literal::character(*self)]);
        }
    }

    impl ExtendTokens for String {
        fn add(&self, tokens: &mut TokenStream) {
            self.as_str().add(tokens);
        }
    }

    impl ExtendTokens for str {
        fn add(&self, tokens: &mut TokenStream) {
            tokens.extend([Literal::string(self)]);
        }
    }

    impl<T: ?Sized + ExtendTokens> ExtendTokens for &T {
        fn add(&self, tokens: &mut TokenStream) {
            (**self).add(tokens);
        }
    }
}
