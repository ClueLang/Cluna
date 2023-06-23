use probe::{lexer::scan_code, parser::parse_tokens};
use std::path::Path;

fn main() -> Result<(), String> {
    let path = Path::new("main.lua");
    let code = std::fs::read_to_string(path).unwrap();
    let scanned = scan_code(code)?;
    let path = path.ancestors().next().unwrap().to_string_lossy() + ".scanned";
    std::fs::write(&*path, format!("{scanned:#?}")).unwrap();
    let parsed = parse_tokens(&scanned)?;
    dbg!(parsed);

    Ok(())
}

#[cfg(test)]
mod test {
    use std::path::PathBuf;

    use probe::{lexer::scan_code, parser::parse_tokens};
    use tests_proc_macro::gen_tests;

    macro_rules! settings {
        ($test:literal, $path:expr) => {{
            let path = &$path;
            let dir = {
                let mut path = path.ancestors().nth(1).unwrap().to_path_buf();
                path.push(concat!("snapshots/", $test));
                PathBuf::from("../").join(path)
            };
            let name = path.file_name().unwrap().to_string_lossy().to_string();

            let mut settings = insta::Settings::new();
            settings.set_snapshot_path(dir);
            settings.set_input_file(path.to_string_lossy().to_string());
            settings.set_snapshot_suffix(name);
            settings.set_prepend_module_to_snapshot(false);

            settings
        }};
    }

    fn scan(path: PathBuf) -> Result<(), String> {
        let code = std::fs::read_to_string(&path).unwrap();
        let scanned = scan_code(code)?;
        let settings = settings!("lexer", path);

        settings.bind(|| {
            insta::assert_debug_snapshot!(scanned);
        });

        Ok(())
    }

    fn parse(path: PathBuf) -> Result<(), String> {
        let code = std::fs::read_to_string(&path).unwrap();
        let scanned = scan_code(code)?;
        let parsed = parse_tokens(&scanned)?;
        let settings = settings!("parser", path);

        settings.bind(|| {
            insta::assert_debug_snapshot!(parsed);
        });

        Ok(())
    }

    gen_tests!("test-data/lua5.1-tests", scan);
    gen_tests!("test-data/lua5.1-tests", parse);
    gen_tests!("test-data/mytests", scan);
    gen_tests!("test-data/mytests", parse);
}
