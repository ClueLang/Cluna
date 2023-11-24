use clap::Parser;
use cluna::{
    compiler::compile_ast,
    error::{get_files, Diagnostic, DiagnosticLevel},
    lexer::scan_code,
    parser::parse_tokens,
};
use colored::Colorize;
use rayon::prelude::*;
use std::{
    path::PathBuf,
    time::{Duration, Instant},
};

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
    /// Supress debug output.
    #[clap(short, long)]
    quiet: bool,
}

fn compile_file(path: &PathBuf, output: Option<PathBuf>, quiet: bool) -> Result<(), Diagnostic> {
    if !quiet {
        eprintln!("   {} {}", "Compiling".green().bold(), path.display());
    }
    let code = std::fs::read_to_string(path).map_err(|e| Diagnostic::other(e.to_string()))?;
    {
        let files = get_files();
        let mut files = files.lock().unwrap();
        files.insert(path.display().to_string(), code.clone());
    }
    let scanned = scan_code(code, Some(path.display().to_string()))?;
    let parsed = parse_tokens(&scanned, Some(path.display().to_string()))?;
    let compiled = compile_ast(parsed);
    let output = output.unwrap_or_else(|| path.with_extension("clue"));
    std::fs::create_dir_all(output.parent().unwrap())
        .map_err(|e| Diagnostic::other(e.to_string()))?;
    std::fs::write(output, compiled).map_err(|e| Diagnostic::other(e.to_string()))
}

fn format_duration(duration: Duration) -> String {
    let secs = duration.as_secs();
    let millis = duration.subsec_millis();
    let micros = duration.subsec_micros() % 1000;
    let nanos = duration.subsec_nanos() % 1000;

    if secs > 3600 {
        format!("{}h {}m {}s", secs / 3600, secs % 3600 / 60, secs % 60)
    } else if secs > 60 {
        format!("{}m {}s", secs / 60, secs % 60)
    } else if secs > 0 {
        format!("{}.{:03}s", secs, millis)
    } else if millis > 0 {
        format!("{}.{:03}ms", millis, micros)
    } else if micros > 0 {
        format!("{}.{:03}μs", micros, nanos)
    } else {
        format!("{}ns", nanos)
    }
}

fn compile() -> Result<(), Diagnostic> {
    let args = Cli::parse();
    let path = args.path;
    if !path.exists() {
        return Err(Diagnostic::other(format!(
            "The path {:?} does not exist",
            path
        )));
    }

    let start = Instant::now();
    if path.is_file() {
        let output = args.output.or_else(|| {
            args.out_dir.clone().map(|mut out_dir| {
                out_dir.push(path.file_name().unwrap());
                out_dir.set_extension("clue");
                out_dir
            })
        });
        compile_file(&path, output, args.quiet)?;
    } else if path.is_dir() {
        if args.output.is_some() {
            eprintln!(
                "{}",
                Diagnostic::other("output flag is ignored when compiling a directory".to_owned())
                    .level(DiagnosticLevel::Warning)
            );
        }

        let mut stack = vec![path.clone()];

        let files = std::iter::from_fn(move || {
            while let Some(path) = stack.pop() {
                if path.is_dir() {
                    stack.extend(path.read_dir().unwrap().map(|entry| entry.unwrap().path()));
                } else if path.is_file() && path.extension().map_or(false, |ext| ext == "lua") {
                    return Some(path);
                }
            }

            None
        });

        files
            .par_bridge()
            .try_for_each(|path| compile_file(&path, None, args.quiet))?;
    }

    if !args.quiet {
        eprintln!(
            "    {} {} in {}",
            "Finished".green().bold(),
            path.display(),
            format_duration(start.elapsed())
        );
    }

    Ok(())
}

fn main() {
    if let Err(e) = compile() {
        eprintln!("{}", e);
        std::process::exit(1);
    }
}

#[cfg(test)]
mod test {
    use std::path::PathBuf;

    use cluna::{compiler::compile_ast, error::Diagnostic, lexer::scan_code, parser::parse_tokens};
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

    fn scan(path: PathBuf) -> Result<(), Diagnostic> {
        let code = std::fs::read_to_string(&path).unwrap();
        let scanned = scan_code(code, Some(path.display().to_string()))?;
        let settings = settings!("lexer", path);

        settings.bind(|| {
            insta::assert_debug_snapshot!(scanned);
        });

        Ok(())
    }

    fn parse(path: PathBuf) -> Result<(), Diagnostic> {
        let code = std::fs::read_to_string(&path).unwrap();
        let scanned = scan_code(code, Some(path.display().to_string()))?;
        let parsed = parse_tokens(&scanned, Some(path.display().to_string()))?;
        let settings = settings!("parser", path);

        settings.bind(|| {
            insta::assert_debug_snapshot!(parsed);
        });

        Ok(())
    }

    fn compile(path: PathBuf) -> Result<(), Diagnostic> {
        let code = std::fs::read_to_string(&path).unwrap();
        let scanned = scan_code(code, Some(path.display().to_string()))?;
        let parsed = parse_tokens(&scanned, Some(path.display().to_string()))?;
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
        fn compile(path: PathBuf) -> Result<(), Diagnostic> {
            let code = std::fs::read_to_string(&path).unwrap();
            let scanned = scan_code(code, Some(path.display().to_string()))?;
            let parsed = parse_tokens(&scanned, Some(path.display().to_string()))?;
            compile_ast(parsed);

            Ok(())
        }

        fn should_fail(path: PathBuf) -> Result<(), ()> {
            compile(path).map_or(Ok(()), |_| Err(()))
        }

        gen_tests!("test-data/negative", should_fail);
    }
}
