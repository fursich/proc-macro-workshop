use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::quote;
use syn::{parse_macro_input, Data, DeriveInput, Fields};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = input.ident;
    let ident = Ident::new(&format!("{}Builder", name.to_string()), Span::call_site());

    let data = input.data;
    let fields = match data {
        Data::Struct(data) => match data.fields {
            Fields::Named(_) => data.fields,
            Fields::Unnamed(_) | Fields::Unit => unimplemented!(),
        },
        Data::Enum(_) | Data::Union(_) => unimplemented!(),
    };
    let field_idents = fields
        .iter()
        .map(|field| field.ident.to_owned())
        .collect::<Vec<_>>();
    let field_types = fields
        .iter()
        .map(|field| field.ty.to_owned())
        .collect::<Vec<_>>();

    let expanded = quote! {
        pub struct #ident {
            #(#field_idents: Option<#field_types>,)*
        }
        impl #name {
            pub fn builder() -> #ident {
                #ident {
                    #(#field_idents: None,)*
                }
            }
        }
    };

    TokenStream::from(expanded)
}
