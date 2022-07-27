#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]

macro_rules! check {
    ($tocheck: expr) => {
        match $tocheck {
            Ok(t) => t,
            Err(e) => return Err(e.to_string()),
        }
    };
}

macro_rules! arg {
    ($name: expr) => {
        unsafe { $name }
    };
}

// mod compiler;
mod compiler;
mod parser;
mod scanner;

use clap::Parser;
use compiler::compile_tokens;
use parser::*;
use scanner::*;
use std::{fs, fs::File, io::prelude::*, path::Path, time::Instant};

pub static mut ENV_TOKENS: bool = false;
pub static mut ENV_STRUCT: bool = false;
pub static mut ENV_OUTPUT: bool = false;
pub static mut ENV_JITBIT: Option<String> = None;
pub static mut ENV_CONTINUE: bool = false;
pub static mut ENV_DONTSAVE: bool = false;
pub static mut ENV_PATHISCODE: bool = false;
pub static mut ENV_RAWSETGLOBALS: bool = false;
pub static mut ENV_DEBUGCOMMENTS: bool = false;

#[derive(Parser)]
#[clap(about, version, long_about = None)]
struct Cli {
    /// The path to the directory where the *.lua files are located.
    /// Every directory inside the given directory will be checked too.
    /// If the path points to a single *.lua file, only that file will be compiled.
    #[clap(required_unless_present = "license")]
    path: Option<String>,

    /// The name the output file will have
    #[clap(default_value = "main", value_name = "OUTPUT FILE NAME")]
    outputname: String,

    /// Print license information
    #[clap(short = 'L', long, display_order = 1000)]
    license: bool,

    /// Print list of detected tokens in compiled files
    #[clap(long)]
    tokens: bool,

    /// Print syntax structure of the tokens of the compiled files
    #[clap(long)]
    r#struct: bool,

    /// Print output Lua code in the console
    #[clap(long)]
    output: bool,

    /// Use LuaJIT's bit library for bitwise operations
    #[clap(short, long, value_name = "VAR NAME")]
    jitbit: Option<String>,

    /// Use tags and goto for continue
    #[clap(short, long)]
    r#continue: bool,

    /// Don't save compiled code
    #[clap(short = 'D', long)]
    dontsave: bool,

    /// Treat PATH not as a path but as lua code
    #[clap(short, long)]
    pathiscode: bool,

    /// Use rawset to create globals
    #[clap(short, long)]
    rawsetglobals: bool,

    /// Don't include debug comments in the output
    #[clap(short, long)]
    debugcomments: bool,
}

fn compile_code(code: String, name: String, scope: usize) -> Result<String, String> {
    let time = Instant::now();
    let (tokens, comments): (Vec<Token>, Vec<Comment>) = scan_code(code, name.clone())?;
    if arg!(ENV_TOKENS) {
        println!("Scanned tokens of file \"{}\":\n{:#?}", name, tokens);
    }
    let ctokens = parse_tokens(tokens, name.clone())?;
    if arg!(ENV_STRUCT) {
        println!("Parsed structure of file \"{}\":\n{:#?}", name, ctokens);
    }

    let code = compile_tokens(scope, ctokens);
    if arg!(ENV_OUTPUT) {
        println!("Compiled Lua code of file \"{}\":\n{}", name, code);
    }
    println!(
        "Compiled file \"{}\" in {} seconds!",
        name,
        time.elapsed().as_secs_f32()
    );
    Ok(code)
}

fn compile_file(path: &Path, name: String, scope: usize) -> Result<String, String> {
    let mut code: String = String::new();
    check!(check!(File::open(path)).read_to_string(&mut code));
    Ok(compile_code(code, name, scope)?)
}

fn compile_folder(path: &Path, rpath: String) -> Result<(), String> {
    for entry in check!(fs::read_dir(path)) {
        let entry = check!(entry);
        let name: String = entry
            .path()
            .file_name()
            .unwrap()
            .to_string_lossy()
            .into_owned();
        let filePathName: String = format!("{}/{}", path.display(), name);
        let filepath: &Path = &Path::new(&filePathName);
        let rname = rpath.clone() + &name;
        if filepath.is_dir() {
            compile_folder(filepath, rname + ".")?;
        } else if filePathName.ends_with(".lua") {
            let code = compile_file(filepath, name, 2)?;
            let compiled_path = format!(
                "{}{}",
                filepath.display().to_string().strip_suffix(".lua").unwrap(),
                ".clue"
            );

            check!(fs::write(compiled_path, code))
        }
    }
    Ok(())
}

fn main() -> Result<(), String> {
    let cli = Cli::parse();
    if cli.license {
        println!("{}", include_str!("../LICENSE"));
        return Ok(());
    }
    unsafe {
        ENV_TOKENS = cli.tokens;
        ENV_STRUCT = cli.r#struct;
        ENV_OUTPUT = cli.output;
        ENV_JITBIT = cli.jitbit;
        ENV_CONTINUE = cli.r#continue;
        ENV_DONTSAVE = cli.dontsave;
        ENV_PATHISCODE = cli.pathiscode;
        ENV_RAWSETGLOBALS = cli.rawsetglobals;
        ENV_DEBUGCOMMENTS = cli.debugcomments;
    }
    if let Some(bit) = arg!(&ENV_JITBIT) {
        todo!("Handle luajit bit")
    }
    let codepath = cli.path.unwrap();
    if arg!(ENV_PATHISCODE) {
        todo!("Evaluate code received as argument");
    }
    let path: &Path = Path::new(&codepath);

    if path.is_dir() {
        let outputname = &format!("{}.clue", cli.outputname);
        let compiledname = if path.display().to_string().ends_with('/')
            || path.display().to_string().ends_with('\\')
        {
            format!("{}{}", path.display(), outputname)
        } else {
            format!("{}/{}", path.display(), outputname)
        };

        compile_folder(path, String::new())?;
    } else if path.is_file() {
        let compiledname =
            String::from(path.display().to_string().strip_suffix(".lua").unwrap()) + ".clue";
        let code = compile_file(
            path,
            path.file_name().unwrap().to_string_lossy().into_owned(),
            0,
        )?;

        check!(fs::write(compiledname, code))
    } else {
        return Err(String::from("The given path doesn't exist"));
    }
    Ok(())
}

#[cfg(test)]
mod test {
    use crate::compile_folder;
    use std::path::Path;

    #[test]
    fn compile() {
        compile_folder(Path::new("tests/"), String::new()).unwrap();
    }
}
