use std::path::Path;

use tests_proc_macro::gen_test;

gen_test!("hello");
fn compile_file(a: &Path, b: String, c: usize) {}
fn main() {}
