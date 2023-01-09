use proc_macro::TokenStream;
use proc_macro2::{Delimiter, Group, Literal, TokenTree};
use quote::{quote, ToTokens};
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

        let mut margin = 0;
        let dotdot_eq = <Token![..=]>::parse(input);
        if dotdot_eq.is_ok() {
            margin = 1;
        } else {
            <Token![..]>::parse(input)?;
        }

        let to = <LitInt>::parse(input)?;
        let content;
        let _braces = braced!(content in input);
        let ts = proc_macro2::TokenStream::parse(&content)?;
        eprintln!("{:?}", ts);

        Ok(SecMarcoInput {
            from: from.base10_parse::<usize>()?,
            to: to.base10_parse::<usize>()? + margin,
            ident,
            ts,
        })
    }
}

impl Into<TokenStream> for SecMarcoInput {
    fn into(self) -> TokenStream {
        let out = self.replace_all();
        out.into()
    }
}

enum Mode {
    ReplaceIdent(usize),
    ReplaceSequence,
}

impl SecMarcoInput {
    fn expand_ident(
        &self,
        tt: proc_macro2::TokenTree,
        i: usize,
        rest: &mut proc_macro2::token_stream::IntoIter,
    ) -> (proc_macro2::TokenTree, bool) {
        if let TokenTree::Ident(ref ident) = tt.clone() {
            let mut out = tt;
            if ident == &self.ident {
                out = TokenTree::Literal(Literal::usize_unsuffixed(i));
                out.set_span(ident.span());
            }

            let mut peek = rest.clone();
            if let (Some(TokenTree::Punct(ref punct)), Some(TokenTree::Ident(ref _ident))) =
                (peek.next(), peek.next())
            {
                if punct.as_char() == '~' {
                    rest.next(); // throw away, as it must be a tilde ~
                    return (out, true);
                }
            }
            return (out, false);
        } else {
            unreachable!("this must be an Ident")
        }
    }

    fn expand_token_tree(
        &self,
        tt: proc_macro2::TokenTree,
        rest: &mut proc_macro2::token_stream::IntoIter,
        mode: &Mode,
        sequence_detected: &mut bool,
    ) -> proc_macro2::TokenStream {
        match tt.clone() {
            TokenTree::Group(group) => {
                let mut expanded = Group::new(
                    group.delimiter(),
                    self.expand_with_mode(group.stream(), mode, sequence_detected),
                );
                expanded.set_span(group.span());
                TokenTree::Group(expanded).into_token_stream()
            }
            TokenTree::Ident(ref _ident) => {
                if let Mode::ReplaceIdent(i) = mode {
                    let mut out: Vec<proc_macro2::TokenTree> = Vec::new();
                    let mut token = tt;

                    loop {
                        let (new_token, continued) = self.expand_ident(token, *i, rest);
                        out.push(new_token);

                        if !continued {
                            break;
                        }

                        token = rest.next().unwrap();
                    }

                    if out.len() == 1 {
                        return out.pop().unwrap().into_token_stream();
                    }

                    let span = out.first().clone().unwrap().span();
                    let s = out
                        .into_iter()
                        .fold(String::new(), |acc, x| acc + x.to_string().as_str());
                    TokenTree::Ident(Ident::new(s.as_str(), span)).to_token_stream()
                } else {
                    tt.into_token_stream()
                }
            }
            TokenTree::Punct(ref punct) if punct.as_char() == '#' => {
                if let Mode::ReplaceSequence = mode {
                    let mut peek = rest.clone();
                    match (peek.next(), peek.next()) {
                        (Some(TokenTree::Group(group)), Some(TokenTree::Punct(ref punct))) => {
                            if group.delimiter() == Delimiter::Parenthesis && punct.as_char() == '*'
                            {
                                let new_sec = SecMarcoInput {
                                    ident: self.ident.clone(),
                                    from: self.from,
                                    to: self.to,
                                    ts: group.stream(),
                                };

                                *rest = peek;
                                *sequence_detected = true;

                                return new_sec.replace_with_each_ident();
                            }
                            tt.into_token_stream()
                        }
                        (_, _) => tt.into_token_stream(),
                    }
                } else {
                    tt.into_token_stream()
                }
            }
            _ => tt.into_token_stream(),
        }
    }

    fn expand_with_mode(
        &self,
        stream: proc_macro2::TokenStream,
        mode: &Mode,
        sequence_detected: &mut bool,
    ) -> proc_macro2::TokenStream {
        let mut out = proc_macro2::TokenStream::new();
        let mut stream = stream.into_iter();

        while let Some(tt) = stream.next() {
            let parsed = self.expand_token_tree(tt, &mut stream, mode, sequence_detected);

            out.extend(&mut parsed.into_iter());
        }
        out
    }

    fn replace_with_each_ident(&self) -> proc_macro2::TokenStream {
        let mut out = proc_macro2::TokenStream::new();
        let mut sequence_detected = false;

        for i in self.from..self.to {
            let expanded = self.expand_with_mode(
                self.ts.clone(),
                &Mode::ReplaceIdent(i),
                &mut sequence_detected,
            );
            out = quote!(#out #expanded);
        }
        out
    }

    fn replace_all(&self) -> proc_macro2::TokenStream {
        let mut sequence_detected = false;

        let out = self.expand_with_mode(
            self.ts.clone(),
            &Mode::ReplaceSequence,
            &mut sequence_detected,
        );
        if sequence_detected {
            return out;
        }

        self.replace_with_each_ident()
    }
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let output = parse_macro_input!(input as SecMarcoInput);
    eprintln!("{:?}", output);

    output.into()
}
