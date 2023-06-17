extern crate proc_macro;

use core::panic;
use proc_macro::TokenStream;
use proc_macro2::{Ident, Literal, Span};
use quote::quote;
use std::fs;

#[proc_macro]
pub fn gen_tests(input: TokenStream) -> TokenStream {
    let mut iter = input.into_iter();

    let folder = iter.next().expect("Expected 2 argument");
    let folder = folder.to_string();
    let folder = folder.trim_matches('"');
    let version = folder.split("-").next().unwrap().replace(".", "");
    assert!(iter.next().expect("Expected 2 arguments").to_string() == ",");

    let func = iter.next().expect("Expected 2 arguments");
    let func = func.to_string();
    let func = Ident::new(&func, Span::call_site());

    let mut out = quote!();
    for entry in fs::read_dir(folder).unwrap() {
        let entry = entry.unwrap().path();
        let filename = entry.to_string_lossy();

        if let Some(_) = filename.strip_suffix(".lua") {
            let name = version.clone()
                + "_"
                + entry
                    .file_name()
                    .unwrap()
                    .to_string_lossy()
                    .strip_suffix(".lua")
                    .unwrap();
            let name = Ident::new_raw(&format!("{name}_{func}"), Span::call_site());
            let path = Literal::string(&filename);

            if let Some(_) = iter.next() {
                panic!("Expected 1 argument");
            }

            let test = quote! {
                #[test]
                fn #name(){
                    #func(::std::path::PathBuf::from(#path)).unwrap();
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
