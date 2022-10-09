extern crate proc_macro;

use core::panic;
use proc_macro::TokenStream;
use proc_macro2::{Ident, Literal, Span};
use quote::quote;
use std::fs;

#[proc_macro]
pub fn gen_tests(input: TokenStream) -> TokenStream {
    let mut iter = input.into_iter();

    let arg = iter.next().expect("Expected 1 argument");
    let arg = arg.to_string();
    let arg = arg.trim_matches('"');
    let mut out = quote!();
    for entry in fs::read_dir(arg).unwrap() {
        let entry = entry.unwrap().path();
        let filename = entry.to_string_lossy();

        if let Some(_) = filename.strip_suffix(".lua") {
            let name = Ident::new_raw(
                &entry
                    .file_name()
                    .unwrap()
                    .to_string_lossy()
                    .strip_suffix(".lua")
                    .unwrap(),
                Span::call_site(),
            );
            let path = Literal::string(&filename);

            if let Some(_) = iter.next() {
                panic!("Expected 1 argument");
            }

            let test = quote! {
                #[test]
                fn #name(){
                    crate::compile_file(::std::path::Path::new(#path),String::new(),0).unwrap();
                }
            };
            out = quote! {
                #out
                #test
            }
        }
    }

    out.into()
}
