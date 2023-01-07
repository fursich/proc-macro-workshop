use proc_macro::TokenStream;
use proc_macro2::{Group, Literal, TokenTree};
use quote::quote;
use syn::parse::{Parse, ParseStream};
use syn::{braced, parse_macro_input, Ident, LitInt, Result, Token};

#[derive(Debug)]
struct SecMarcoInput {
    from: usize,
    to: usize,
    ident: Ident,
    ts: proc_macro2::TokenStream,
}

impl Parse for SecMarcoInput {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident = syn::Ident::parse(input)?;
        let _in = <Token![in]>::parse(input)?;
        let from = <LitInt>::parse(input)?;
        let _dots = <Token![..]>::parse(input)?;
        let to = <LitInt>::parse(input)?;
        let content;
        let _braces = braced!(content in input);
        let ts = proc_macro2::TokenStream::parse(&content)?;
        eprintln!("{:?}", ts);

        Ok(SecMarcoInput {
            from: from.base10_parse::<usize>()?,
            to: to.base10_parse::<usize>()?,
            ident,
            ts,
        })
    }
}

impl Into<TokenStream> for SecMarcoInput {
    fn into(self) -> TokenStream {
        let mut out = proc_macro2::TokenStream::new();
        for i in self.from..self.to {
            let expanded = self.expand(self.ts.clone(), i);
            out = quote!(#out #expanded);
        }
        out.into()
    }
}

impl SecMarcoInput {
    fn expand2(&self, tt: proc_macro2::TokenTree, i: usize) -> proc_macro2::TokenTree {
        match tt {
            TokenTree::Group(group) => {
                let mut expanded = Group::new(group.delimiter(), self.expand(group.stream(), i));
                expanded.set_span(group.span());
                TokenTree::Group(expanded)
            }
            TokenTree::Ident(ident) if ident == self.ident => {
                let mut lit = TokenTree::Literal(Literal::usize_unsuffixed(i));
                lit.set_span(ident.span());
                lit
            }
            _ => tt,
        }
    }

    fn expand(&self, stream: proc_macro2::TokenStream, i: usize) -> proc_macro2::TokenStream {
        stream.into_iter().map(|tt| self.expand2(tt, i)).collect()
    }
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let output = parse_macro_input!(input as SecMarcoInput);
    eprintln!("{:?}", output);

    output.into()
}
