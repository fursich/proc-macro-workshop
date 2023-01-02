use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::quote;
use syn::{parse_macro_input, Data, DeriveInput, Fields};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let base_struct = input.ident;
    let builder = Ident::new(
        &format!("{}Builder", base_struct.to_string()),
        Span::call_site(),
    );

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
    let missing_ident_errors = fields
        .iter()
        .map(|field| format!("{} is missing", field.ident.to_owned().unwrap()))
        .collect::<Vec<_>>();

    let expanded = quote! {
        use std::error::Error;

        pub struct #builder {
            #(#field_idents: Option<#field_types>,)*
        }
        impl #base_struct {
            pub fn builder() -> #builder {
                #builder {
                    #(#field_idents: None,)*
                }
            }
        }
        impl #builder {
            #(
                fn #field_idents(&mut self, #field_idents: #field_types) -> &mut Self {
                    self.#field_idents = Some(#field_idents);
                    self
                }
            )*
            pub fn build(self) -> Result<#base_struct, Box<dyn Error>> {
                let #builder {
                    #(#field_idents,)*
                } = self;
                let built = #base_struct {
                    #(#field_idents: #field_idents.ok_or(Box::<dyn Error>::from(#missing_ident_errors))?,)*
                };
                Ok(built)
            }
        }
    };

    TokenStream::from(expanded)
}
