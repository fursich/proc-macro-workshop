use proc_macro::TokenStream;
// use quote::quote;
use syn::parse::{Parse, ParseStream};
use syn::{parse_macro_input, Block, LitInt, Result, Token};

#[derive(Debug)]
struct SecMarcoInput {/* ... */}

impl Parse for SecMarcoInput {
    fn parse(input: ParseStream) -> Result<Self> {
        let var = syn::Ident::parse(input)?;
        eprintln!("{:?}", var);
        let _in = <Token![in]>::parse(input)?;
        eprintln!("{:?}", _in);
        let from = <LitInt>::parse(input)?;
        let _dots = <Token![..]>::parse(input)?;
        let to = <LitInt>::parse(input)?;
        eprintln!("{:?}, {:?}, {:?}", from, _dots, to);
        let block = <Block>::parse(input)?;
        eprintln!("{:?}", block);

        Ok(SecMarcoInput {})
    }
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as SecMarcoInput);
    println!("{:?}", ast);

    TokenStream::new()
}
