use proc_macro::TokenStream;
use proc_macro2::{Group, Literal, TokenTree};
use quote::quote;
use std::iter::Peekable;
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

#[derive(Debug)]
struct ParseResult {
    token: proc_macro2::TokenTree,
    continued: bool,
}

impl SecMarcoInput {
    fn consume(
        &self,
        tt: proc_macro2::TokenTree,
        stream: &mut Peekable<proc_macro2::token_stream::IntoIter>,
        i: usize,
    ) -> ParseResult {
        match tt {
            TokenTree::Group(group) => {
                let mut expanded = Group::new(group.delimiter(), self.expand(group.stream(), i));
                expanded.set_span(group.span());
                ParseResult {
                    token: TokenTree::Group(expanded),
                    continued: false,
                }
            }
            TokenTree::Ident(ident) if ident == self.ident => {
                let mut lit = TokenTree::Literal(Literal::usize_unsuffixed(i));
                lit.set_span(ident.span());
                ParseResult {
                    token: lit,
                    continued: false,
                }
            }
            TokenTree::Punct(ref punct) if punct.as_char() == '~' => {
                if let Some(TokenTree::Ident(_ident)) = stream.clone().peek() {
                    if let Some(tt) = stream.next() {
                        let result = self.consume(tt, stream, i);
                        return ParseResult {
                            token: result.token,
                            continued: true,
                        };
                    }
                }
                ParseResult {
                    token: tt,
                    continued: false,
                }
            }
            _ => ParseResult {
                token: tt,
                continued: false,
            },
        }
    }

    fn expand(&self, stream: proc_macro2::TokenStream, i: usize) -> proc_macro2::TokenStream {
        let mut out: Vec<TokenTree> = Vec::new();
        let mut stream = stream.into_iter().peekable();

        while let Some(tt) = stream.next() {
            let parsed = self.consume(tt, &mut stream, i);
            let mut new_token = parsed.token;

            if parsed.continued {
                let last_token = out.pop().unwrap();
                new_token = concat_idents(last_token, new_token)
            }
            out.push(new_token);
        }

        out.into_iter().collect::<proc_macro2::TokenStream>()
    }
}

fn concat_idents(
    token1: proc_macro2::TokenTree,
    token2: proc_macro2::TokenTree,
) -> proc_macro2::TokenTree {
    let s = token1.to_string() + token2.to_string().as_str();
    TokenTree::Ident(Ident::new(s.as_str(), token1.span()))
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let output = parse_macro_input!(input as SecMarcoInput);
    eprintln!("{:?}", output);

    output.into()
}
