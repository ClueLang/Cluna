use clap::Parser;
use cluna::{compiler::compile_ast, lexer::scan_code, parser::parse_tokens};
use std::path::PathBuf;

#[derive(Parser)]
struct Cli {
    /// Input file path.
    path: PathBuf,
    /// Output file path. This flag is ignored when compiling a directory.
    #[clap(short, long)]
    output: Option<PathBuf>,
    /// Output directory path.
    #[clap(long)]
    out_dir: Option<PathBuf>,
}

fn compile_file(path: &PathBuf, output: Option<PathBuf>) -> Result<(), String> {
    let code = std::fs::read_to_string(path).map_err(|e| e.to_string())?;
    let scanned = scan_code(code)?;
    let parsed = parse_tokens(&scanned)?;
    let compiled = compile_ast(parsed);

    let output = output.unwrap_or_else(|| path.with_extension("clue"));
    std::fs::create_dir_all(output.parent().unwrap()).map_err(|e| e.to_string())?;
    std::fs::write(output, compiled).map_err(|e| e.to_string())?;
    Ok(())
}

fn main() -> Result<(), String> {
    let args = Cli::parse();
    let path = args.path;

    if !path.exists() {
        return Err(format!("The path {:?} does not exist", path));
    }

    if path.is_file() {
        let output = args.output.or_else(|| {
            args.out_dir.clone().map(|mut out_dir| {
                out_dir.push(path.file_name().unwrap());
                out_dir.set_extension("clue");
                out_dir
            })
        });
        return compile_file(&path, output);
    } else if path.is_dir() {
        if args.output.is_some() {
            eprintln!("Warning: output flag is ignored when compiling a directory");
        }
        let dir = path.clone();
        let mut stack = vec![path];

        while let Some(path) = stack.pop() {
            for entry in std::fs::read_dir(path).map_err(|e| e.to_string())? {
                let entry = entry.map_err(|e| e.to_string())?;
                let path = entry.path();

                if path.is_dir() {
                    stack.push(path);
                } else if path.is_file() && path.extension().map_or(false, |ext| ext == "lua") {
                    let output = args.out_dir.as_ref().map(|out_dir| {
                        let mut out = out_dir.clone();
                        out.push(path.strip_prefix(&dir).unwrap());
                        out.set_extension("clue");
                        out
                    });
                    compile_file(&path, output)?;
                }
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod test {
    use std::path::PathBuf;

    use cluna::{compiler::compile_ast, lexer::scan_code, parser::parse_tokens};
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

    mod lua5_3 {
        use super::*;

        gen_tests!("test-data/lua5.3-tests", scan);
        gen_tests!("test-data/lua5.3-tests", parse);
        gen_tests!("test-data/lua5.3-tests", compile);
    }

    mod lua5_4 {
        use super::*;

        gen_tests!("test-data/lua5.4-tests", scan);
        gen_tests!("test-data/lua5.4-tests", parse);
        gen_tests!("test-data/lua5.4-tests", compile);
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
