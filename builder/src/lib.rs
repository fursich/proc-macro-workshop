use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::quote;
use syn::{
    parse_macro_input, AngleBracketedGenericArguments, Data, DeriveInput, Error, Field, Fields,
    GenericArgument, Lit, Meta, MetaList, MetaNameValue, NestedMeta, Path, PathArguments,
    PathSegment, Result, Type, TypePath,
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
    if let Err(err) = field_definitions {
        return err.to_compile_error().into();
    }

    let field_definitions = field_definitions.unwrap();

    let mandatory_field_idents = field_definitions.filtered_by(false, false).select_idents();
    let mandatory_field_types = field_definitions.filtered_by(false, false).select_types();
    let missing_ident_errors = mandatory_field_idents
        .iter()
        .map(|ident| format!("{} is missing", ident.to_owned()))
        .collect::<Vec<_>>();

    let option_field_idents = field_definitions.filtered_by(true, false).select_idents();
    let option_base_field_types = field_definitions
        .filtered_by(true, false)
        .select_base_types();

    let vec_field_idents = field_definitions.filtered_by(false, true).select_idents();
    let vec_base_field_types = field_definitions
        .filtered_by(false, true)
        .select_base_types();

    let (vec_setter_idents, vec_setter_types) = field_definitions
        .filtered_by(false, true)
        .select_vec_setter_components();
    let (vec_element_container_idents, vec_element_setter_idents, vec_element_setter_types) =
        field_definitions
            .filtered_by(false, true)
            .select_vec_element_setter_components();

    let expanded = quote! {
        use std::error::Error;

        #[derive(Clone)]
        pub struct #builder {
            #(#mandatory_field_idents: Option<#mandatory_field_types>,)*
            #(#option_field_idents: Option<#option_base_field_types>,)*
            #(#vec_field_idents: Vec<#vec_base_field_types>,)*
        }
        impl #base_struct {
            pub fn builder() -> #builder {
                #builder {
                    #(#mandatory_field_idents: None,)*
                    #(#option_field_idents: None,)*
                    #(#vec_field_idents: Vec::new(),)*
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
                fn #option_field_idents(&mut self, #option_field_idents: #option_base_field_types) -> &mut Self {
                    self.#option_field_idents = Some(#option_field_idents);
                    self
                }
            )*
            #(
                fn #vec_setter_idents(&mut self, #vec_setter_idents: #vec_setter_types) -> &mut Self {
                    self.#vec_setter_idents = #vec_setter_idents;
                    self
                }
            )*
            #(
                fn #vec_element_setter_idents(&mut self, #vec_element_setter_idents: #vec_element_setter_types) -> &mut Self {
                    self.#vec_element_container_idents.push(#vec_element_setter_idents);
                    self
                }
            )*

            pub fn build(&self) -> Result<#base_struct, Box<dyn Error>> {
                let #builder {
                    #(#mandatory_field_idents,)*
                    #(#option_field_idents,)*
                    #(#vec_field_idents,)*
                } = self.clone();
                let built = #base_struct {
                    #(#mandatory_field_idents: #mandatory_field_idents.ok_or(Box::<dyn Error>::from(#missing_ident_errors))?,)*
                    #(#option_field_idents,)*
                    #(#vec_field_idents,)*
                };
                Ok(built)
            }
        }
    };

    TokenStream::from(expanded)
}

#[derive(Clone, Debug)]
struct _FieldDefinition {
    ident: Ident,
    ty: Type,
    is_option: bool,
    is_vec: bool,

    base_ty: Option<Type>,
    setter_ident: Option<Ident>,
    element_setter_ident: Option<Ident>,
}

#[derive(Debug)]
struct _FieldDefinitions {
    definitions: Vec<_FieldDefinition>,
}

impl _FieldDefinition {
    fn new(field: &Field) -> Result<Self> {
        let ident = field
            .ident
            .clone()
            .expect("the field must be a named field");

        let mut base_ty = try_retrieve_base_type(field, "Option");
        let is_option = base_ty.is_some();
        let mut is_vec = false;

        if !is_option {
            base_ty = try_retrieve_base_type(field, "Vec");
            is_vec = base_ty.is_some();
        }

        let mut setter_ident = Some(ident.clone());
        let element_setter_ident = try_retrieve_element_setter_ident(field)?;
        if element_setter_ident.is_some() {
            if !is_vec {
                panic!(
                    "element setter {} is set for a non-vec type",
                    element_setter_ident.unwrap().to_string()
                );
            }

            if setter_ident == element_setter_ident {
                setter_ident = None;
            }
        }

        Ok(Self {
            ident,
            ty: field.ty.clone(),
            is_option,
            is_vec,
            base_ty,
            setter_ident,
            element_setter_ident,
        })
    }
}

impl _FieldDefinitions {
    fn new(data: Data) -> Result<Self> {
        let fields = retrieve_named_fields(data);

        let definitions = fields
            .iter()
            .map(|field| _FieldDefinition::new(field))
            .collect::<Result<Vec<_FieldDefinition>>>()?;

        Ok(Self { definitions })
    }

    fn filtered_by(&self, is_option: bool, is_vec: bool) -> Self {
        let filtered_definitions = self
            .definitions
            .iter()
            .filter(|field_definition| {
                field_definition.is_option == is_option && field_definition.is_vec == is_vec
            })
            .map(|field_definition| field_definition.to_owned())
            .collect::<Vec<_FieldDefinition>>();
        Self {
            definitions: filtered_definitions,
        }
    }

    fn select_idents(self) -> Vec<Ident> {
        self.definitions
            .iter()
            .map(|field_definition| field_definition.ident.clone())
            .collect::<Vec<Ident>>()
    }

    fn select_types(self) -> Vec<Type> {
        self.definitions
            .iter()
            .map(|field_definition| field_definition.ty.clone())
            .collect::<Vec<Type>>()
    }

    fn select_base_types(self) -> Vec<Type> {
        self.definitions
            .iter()
            .map(|field_definition| {
                field_definition
                    .base_ty
                    .clone()
                    .expect("base type does not exist")
            })
            .collect::<Vec<Type>>()
    }

    fn select_vec_setter_components(&self) -> (Vec<Ident>, Vec<Type>) {
        self.definitions
            .iter()
            .filter(|field_definition| field_definition.setter_ident.is_some())
            .map(|field_definition| {
                (
                    field_definition
                        .setter_ident
                        .clone()
                        .expect("setter_ident must be present"),
                    field_definition.ty.clone(),
                )
            })
            .unzip()
    }

    fn select_vec_element_setter_components(&self) -> (Vec<Ident>, Vec<Ident>, Vec<Type>) {
        let components = self
            .definitions
            .iter()
            .filter(|field_definition| field_definition.element_setter_ident.is_some())
            .map(|field_definition| {
                (
                    field_definition.ident.clone(),
                    field_definition
                        .element_setter_ident
                        .clone()
                        .expect("element_setter_ident must be present"),
                    field_definition
                        .base_ty
                        .clone()
                        .expect("base type does not exist"),
                )
            });

        let mut container_idents = Vec::new();
        let mut setter_idents = Vec::new();
        let mut setter_types = Vec::new();
        for component in components {
            container_idents.push(component.0);
            setter_idents.push(component.1);
            setter_types.push(component.2);
        }

        (container_idents, setter_idents, setter_types)
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

/// Return Some(T) if the given field has a type of X<T>, where X is outer_type
/// otherwise None
fn try_retrieve_base_type(field: &Field, outer_type: &str) -> Option<Type> {
    match field.clone().ty {
        Type::Path(TypePath { qself: None, path }) => {
            if let Some(segment) = try_retrieve_named_ident(&path, outer_type) {
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
        _ => (),
    }

    None
}

fn try_retrieve_named_ident(path: &Path, name: &str) -> Option<PathSegment> {
    if path.leading_colon.is_some() {
        return None; // only available for a path without leading_colon
    }
    let segments = &path.segments;
    if segments.len() == 1 {
        if let Some(segment) = segments.first() {
            if segment.ident.to_string() == name {
                return Some(segment.to_owned());
            }
        }
    }

    None
}

fn try_retrieve_element_setter_ident(field: &Field) -> Result<Option<Ident>> {
    for attr in field.attrs.clone() {
        if let Ok(meta) = attr.parse_meta() {
            match meta.clone() {
                Meta::List(MetaList {
                    path: base_path,
                    paren_token: _,
                    nested,
                }) => {
                    if try_retrieve_named_ident(&base_path, "builder").is_some() {
                        if nested.len() == 1 {
                            if let Some(nested) = nested.first() {
                                match nested {
                                    NestedMeta::Meta(Meta::NameValue(MetaNameValue {
                                        path,
                                        eq_token: _,
                                        lit,
                                    })) => {
                                        if try_retrieve_named_ident(&path, "each").is_some() {
                                            if let Lit::Str(lit) = lit {
                                                return Ok(Some(Ident::new(
                                                    lit.value().as_str(),
                                                    Span::call_site(),
                                                )));
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
            return Err(Error::new_spanned(
                meta.to_owned(),
                "expected `builder(each = \"...\")`",
            ));
        }
    }

    Ok(None)
}
