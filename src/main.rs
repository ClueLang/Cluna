use clap::Parser;
use probe::{compiler::compile_ast, lexer::scan_code, parser::parse_tokens};
use std::path::PathBuf;

#[derive(Parser)]
struct Cli {
    path: PathBuf,
    #[clap(short, long)]
    output: Option<PathBuf>,
}

fn compile_file(path: &PathBuf, output: Option<PathBuf>) -> Result<(), String> {
    let code = std::fs::read_to_string(path).map_err(|e| e.to_string())?;
    let scanned = scan_code(code)?;
    let parsed = parse_tokens(&scanned)?;
    let compiled = compile_ast(parsed);

    let output = output.unwrap_or_else(|| path.with_extension("clue"));

    std::fs::write(output, compiled).map_err(|e| e.to_string())?;
    Ok(())
}

fn main() -> Result<(), String> {
    let args = Cli::parse();
    let path = args.path;

    if !path.exists() {
        return Err(format!("File {:?} does not exist", path));
    }

    if path.is_file() {
        return compile_file(&path, args.output);
    } else if path.is_dir() {
        if args.output.is_some() {
            eprintln!("Warning: output flag is ignored when compiling a directory");
        }

        let mut stack = vec![path];

        while let Some(path) = stack.pop() {
            for entry in std::fs::read_dir(path).map_err(|e| e.to_string())? {
                let entry = entry.map_err(|e| e.to_string())?;
                let path = entry.path();

                if path.is_dir() {
                    stack.push(path);
                } else if path.is_file() && path.extension().map_or(false, |ext| ext == "lua") {
                    compile_file(&path, None)?;
                }
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod test {
    use std::path::PathBuf;

    use probe::{compiler::compile_ast, lexer::scan_code, parser::parse_tokens};
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

    fn compile(path: PathBuf) -> Result<(), String> {
        let code = std::fs::read_to_string(&path).unwrap();
        let scanned = scan_code(code)?;
        let parsed = parse_tokens(&scanned)?;
        let compiled = compile_ast(parsed);
        let settings = settings!("compiler", path);

        settings.bind(|| {
            insta::assert_display_snapshot!(compiled);
        });

        Ok(())
    }

    mod lua5_1 {
        use super::*;

        gen_tests!("test-data/lua5.1-tests", scan);
        gen_tests!("test-data/lua5.1-tests", parse);
        gen_tests!("test-data/lua5.1-tests", compile);
    }

    mod lua5_2 {
        use super::*;

        gen_tests!("test-data/lua5.2-tests", scan);
        gen_tests!("test-data/lua5.2-tests", parse);
        gen_tests!("test-data/lua5.2-tests", compile);
    }

    mod extra {
        use super::*;
        gen_tests!("test-data/extra", scan);
        gen_tests!("test-data/extra", parse);
        gen_tests!("test-data/extra", compile);
    }

    mod negative {
        use super::*;
        fn compile(path: PathBuf) -> Result<(), String> {
            let code = std::fs::read_to_string(path).unwrap();
            let scanned = scan_code(code)?;
            let parsed = parse_tokens(&scanned)?;
            compile_ast(parsed);

            Ok(())
        }

        fn should_fail(path: PathBuf) -> Result<(), ()> {
            compile(path).map_or(Ok(()), |_| Err(()))
        }

        gen_tests!("test-data/negative", should_fail);
    }
}
