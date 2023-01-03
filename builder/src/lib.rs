use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::quote;
use syn::{
    parse_macro_input, AngleBracketedGenericArguments, Data, DeriveInput, Field, Fields,
    GenericArgument, Path, PathArguments, Type, TypePath,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let base_struct = input.ident;
    let builder = Ident::new(
        &format!("{}Builder", base_struct.to_string()),
        Span::call_site(),
    );

    let data = input.data;
    let field_definitions = _FieldDefinitions::new(data);

    let idents = field_definitions.idents();
    let mandatory_field_idents = field_definitions.partial_idents(false);
    let optional_field_idents = field_definitions.partial_idents(true);

    let mandatory_field_types = field_definitions.partial_types(false, false);
    let optional_base_field_types = field_definitions.partial_types(true, true);

    let missing_ident_errors = mandatory_field_idents
        .iter()
        .map(|ident| format!("{} is missing", ident.to_owned()))
        .collect::<Vec<_>>();

    let expanded = quote! {
        use std::error::Error;

        #[derive(Clone)]
        pub struct #builder {
            #(#mandatory_field_idents: Option<#mandatory_field_types>,)*
            #(#optional_field_idents: Option<#optional_base_field_types>,)*
        }
        impl #base_struct {
            pub fn builder() -> #builder {
                #builder {
                    #(#idents: None,)*
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
                fn #optional_field_idents(&mut self, #optional_field_idents: #optional_base_field_types) -> &mut Self {
                    self.#optional_field_idents = Some(#optional_field_idents);
                    self
                }
            )*
            pub fn build(&self) -> Result<#base_struct, Box<dyn Error>> {
                let #builder {
                    #(#idents,)*
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

struct _FieldDefinition {
    ident: Ident,
    ty: Type,
    base_ty: Option<Type>,
}

struct _FieldDefinitions {
    definitions: Vec<_FieldDefinition>,
}

impl _FieldDefinition {
    fn new(field: &Field) -> Self {
        let base_ty = retrieve_base_type_from_option(field.clone());
        Self {
            ident: field
                .ident
                .clone()
                .expect("the field must be a named field"),
            ty: field.ty.clone(),
            base_ty,
        }
    }

    fn is_option(&self) -> bool {
        self.base_ty.is_some()
    }
}

impl _FieldDefinitions {
    fn new(data: Data) -> Self {
        let fields = retrieve_named_fields(data);

        let definitions = fields
            .iter()
            .map(|field| _FieldDefinition::new(field))
            .collect::<Vec<_FieldDefinition>>();
        Self { definitions }
    }

    fn idents(&self) -> Vec<Ident> {
        self.definitions
            .iter()
            .map(|field_definition| field_definition.ident.clone())
            .collect::<Vec<Ident>>()
    }

    fn partial_idents(&self, is_option: bool) -> Vec<Ident> {
        self.definitions
            .iter()
            .filter(|field_definition| field_definition.is_option() == is_option)
            .map(|field_definition| field_definition.ident.clone())
            .collect::<Vec<Ident>>()
    }

    fn partial_types(&self, is_option: bool, is_base: bool) -> Vec<Type> {
        self.definitions
            .iter()
            .filter(|field_definition| field_definition.is_option() == is_option)
            .map(|field_definition| {
                if is_base {
                    field_definition
                        .base_ty
                        .clone()
                        .expect("base type does not exist")
                } else {
                    field_definition.ty.clone()
                }
            })
            .collect::<Vec<Type>>()
    }
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
