use proc_macro::{Delimiter, Group, Punct, Spacing, TokenStream};

#[proc_macro_attribute]
pub fn macro_attr(attr: TokenStream, input: TokenStream) -> TokenStream {
    let mut attr_iter = attr.into_iter();
    let attr = attr_iter.next().expect("Missing macro name");
    if attr_iter.next().is_some() {
        panic!("Unexpected attribute in macro");
    }

    let mut output = TokenStream::new();
    output.extend([attr]);
    output.extend([Punct::new('!', Spacing::Joint)]);
    output.extend([Group::new(Delimiter::Brace, input)]);

    output
}
