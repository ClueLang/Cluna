use probe::lexer::scan_code;
use std::path::Path;

fn main() {
    scan("main.lua").unwrap();
}

fn scan<P: AsRef<Path> + Copy>(path: P) -> Result<(), String> {
    let code = std::fs::read_to_string(path).unwrap();
    let scanned = scan_code(code)?;
    let path = path.as_ref().ancestors().next().unwrap().to_string_lossy() + ".scanned";
    std::fs::write(&*path, format!("{scanned:#?}")).unwrap();

    Ok(())
}

#[cfg(test)]
mod test {
    use std::path::PathBuf;

    use probe::{lexer::scan_code, parser::parse_tokens};
    use tests_proc_macro::gen_tests;

    fn scan(path: PathBuf) -> Result<(), String> {
        let code = std::fs::read_to_string(path).unwrap();
        let scanned = scan_code(code)?;
        insta::assert_debug_snapshot!(scanned);

        Ok(())
    }

    gen_tests!("lua5.1-tests", scan);
    // gen_tests!("lua5.2-tests");
    // gen_tests!("lua5.3-tests");
    // gen_tests!("lua5.4-tests");
}
