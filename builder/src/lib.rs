use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::quote;
use syn::{
    parse_macro_input, AngleBracketedGenericArguments, Data, DeriveInput, Field, Fields,
    GenericArgument, Path, PathArguments, Type, TypePath,
};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let base_struct = input.ident;
    let builder = Ident::new(
        &format!("{}Builder", base_struct.to_string()),
        Span::call_site(),
    );

    let data = input.data;
    let fields = retrieve_named_fields(data);
    let (mandatory_fields, optional_fields) = partition_fields_by_optional_types(fields.clone());

    let mandatory_field_idents = mandatory_fields
        .iter()
        .map(|(field, _ty)| field.ident.to_owned())
        .collect::<Vec<_>>();
    let mandatory_field_types = mandatory_fields
        .iter()
        .map(|(_field, ty)| ty.to_owned())
        .collect::<Vec<_>>();
    let missing_ident_errors = mandatory_fields
        .iter()
        .map(|(field, _ty)| format!("{} is missing", field.ident.to_owned().unwrap()))
        .collect::<Vec<_>>();

    let optional_field_idents = optional_fields
        .iter()
        .map(|(field, _ty)| field.ident.to_owned())
        .collect::<Vec<_>>();
    let optional_field_types = optional_fields
        .iter()
        .map(|(_field, ty)| ty.to_owned())
        .collect::<Vec<_>>();

    let expanded = quote! {
        use std::error::Error;

        #[derive(Clone)]
        pub struct #builder {
            #(#mandatory_field_idents: Option<#mandatory_field_types>,)*
            #(#optional_field_idents: Option<#optional_field_types>,)*
        }
        impl #base_struct {
            pub fn builder() -> #builder {
                #builder {
                    #(#mandatory_field_idents: None,)*
                    #(#optional_field_idents: None,)*
                }
            }
        }
        impl #builder {
            #(
                fn #mandatory_field_idents(&mut self, #mandatory_field_idents: #mandatory_field_types) -> &mut Self {
                    self.#mandatory_field_idents = Some(#mandatory_field_idents);
                    self
                }
            )*
            #(
                fn #optional_field_idents(&mut self, #optional_field_idents: #optional_field_types) -> &mut Self {
                    self.#optional_field_idents = Some(#optional_field_idents);
                    self
                }
            )*
            pub fn build(&self) -> Result<#base_struct, Box<dyn Error>> {
                let #builder {
                    #(#mandatory_field_idents,)*
                    #(#optional_field_idents,)*
                } = self.clone();
                let built = #base_struct {
                    #(#mandatory_field_idents: #mandatory_field_idents.ok_or(Box::<dyn Error>::from(#missing_ident_errors))?,)*
                    #(#optional_field_idents)*
                };
                Ok(built)
            }
        }
    };

    TokenStream::from(expanded)
}

fn retrieve_named_fields(data: Data) -> Fields {
    match data {
        Data::Struct(data) => match data.fields {
            Fields::Named(_) => data.fields,
            Fields::Unnamed(_) | Fields::Unit => unimplemented!(),
        },
        Data::Enum(_) | Data::Union(_) => unimplemented!(),
    }
}

fn partition_fields_by_optional_types(fields: Fields) -> (Vec<(Field, Type)>, Vec<(Field, Type)>) {
    let mut optional_fields = Vec::new();
    let mut mandatory_fields = Vec::new();

    for field in fields.iter() {
        if let Some(ty) = retrieve_base_type_from_option(field.clone()) {
            optional_fields.push((field.to_owned(), ty));
        } else {
            mandatory_fields.push((field.to_owned(), field.ty.to_owned()));
        }
    }

    (mandatory_fields, optional_fields)
}

fn retrieve_base_type_from_option(field: Field) -> Option<Type> {
    match field.ty {
        Type::Path(TypePath {
            qself: None,
            path: Path {
                leading_colon: _,
                segments,
            },
        }) => {
            if segments.len() == 1 {
                if let Some(segment) = segments.first() {
                    if segment.ident.to_string() == "Option" {
                        match segment.arguments.clone() {
                            PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                                colon2_token: _,
                                lt_token: _,
                                gt_token: _,
                                args,
                            }) => {
                                if args.len() == 1 {
                                    if let Some(arg) = args.first() {
                                        match arg {
                                            GenericArgument::Type(ty) => {
                                                return Some(ty.to_owned());
                                            }
                                            _ => (),
                                        }
                                    }
                                }
                            }
                            _ => (),
                        }
                    }
                }
            }
        }
        _ => (),
    }

    None
}
