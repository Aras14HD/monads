extern  crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;
use syn::parse::{Parse, ParseStream, Result};
use syn::{parse_macro_input, Token, Ident, punctuated::Punctuated, parse_str};

struct Input {
    types: Punctuated<Ident, Token![,]>
}

impl Parse for Input {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Self {
            types: input.parse_terminated(Ident::parse, Token![,])?
        })
    }
}

#[proc_macro]
pub fn impl_run_tuple(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as Input);
    let types: Vec<Ident> = input.types.iter().map(|x| x.clone()).collect();
    let typesw: Vec<Ident> = types.iter().map(|x| parse_str(&(x.to_string() + "w")).unwrap()).collect();
    let lower_types: Vec<Ident> = types.iter().map(|x| parse_str(&x.to_string().to_lowercase()).unwrap()).collect();
    let lower_typesw: Vec<Ident> = lower_types.iter().map(|x| parse_str(&(x.to_string() + "w")).unwrap()).collect();
    let mut code = quote! {
        func(aw, #(#lower_typesw,)*)
    };

    for (t, tw) in lower_types.iter().zip(lower_typesw.iter()).rev() {
        code = quote! {
            #t.run::<Inner, _>(|#tw| #code)
        }
    }
    
    let out = quote! {
        impl<Inner, A: Run<Aw>, Aw, #(#types: Run<#typesw, Wrapper<Inner> = A::Wrapper<Inner>>, #typesw,)*> RunTuple<(Aw, #(#typesw,)*), Inner> for (A, #(#types,)*) {
            type Wrapper<T> = A::Wrapper<T>;
            fn run<Func: FnOnce<(Aw, #(#typesw,)*), Output = Self::Wrapper<Inner>>>(self, func: Func) -> Self::Wrapper<Inner> {
                let (a, #(#lower_types,)*) = self;
                a.run(|aw| #code)
            }
        } 
    }.into();
    // println!("{}", out);
    out
}
#[proc_macro]
pub fn impl_run_tuple_trivial(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as Input);
    let types: Vec<Ident> = input.types.iter().map(|x| x.clone()).collect();
    let typesw: Vec<Ident> = types.iter().map(|x| parse_str(&(x.to_string() + "w")).unwrap()).collect();
    let lower_types: Vec<Ident> = types.iter().map(|x| parse_str(&x.to_string().to_lowercase()).unwrap()).collect();
    let lower_typesw: Vec<Ident> = lower_types.iter().map(|x| parse_str(&(x.to_string() + "w")).unwrap()).collect();
    let mut code = quote! {
        func(aw, #(#lower_typesw,)*)
    };

    for (i, (t, tw)) in lower_types.iter().zip(lower_typesw.iter()).rev().enumerate() {
        code = if i > 0 {quote! {
            #t.run::<Inner, _>(|#tw| #code)
        }} else {quote! {
            #t.run_triv::<Inner, _>(|#tw| #code)
        }}
    }

    code = if lower_types.len() > 0 {quote! {
        a.run(|aw| #code)
    }} else {quote! {
        a.run_triv(|aw| #code)
    }};
    
    let out = quote! {
        impl<Inner, A: RunTrivial<Aw>, Aw, #(#types: RunTrivial<#typesw, Wrapper<Inner> = A::Wrapper<Inner>>, #typesw,)*> RunTupleTrivial<(Aw, #(#typesw,)*), Inner> for (A, #(#types,)*) {
            fn run_triv<Func: FnOnce<(Aw, #(#typesw,)*), Output = Inner>>(self, func: Func) -> Self::Wrapper<Inner> {
                let (a, #(#lower_types,)*) = self;
                #code
            }
        } 
    }.into();
    // println!("{}", out);
    out
}
