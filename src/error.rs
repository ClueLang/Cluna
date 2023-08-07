use std::{
    collections::HashMap,
    error::Error,
    fmt,
    sync::{Arc, Mutex, OnceLock},
};

use colored::Colorize;

use crate::lexer::Position;

#[derive(Debug)]
pub struct Diagnostic {
    message: String,
    hint: Option<String>,
    kind: DiagnosticKind,
    level: DiagnosticLevel,
}

#[derive(Debug)]
enum DiagnosticKind {
    Compiler {
        path: Option<String>,
        position: Position,
    },
    Other,
}

type Files = Arc<Mutex<HashMap<String, String>>>;
static FILES: OnceLock<Files> = OnceLock::new();
pub fn get_files() -> Files {
    FILES
        .get_or_init(|| Arc::new(Mutex::new(HashMap::new())))
        .clone()
}

#[derive(Debug, Default)]
pub enum DiagnosticLevel {
    #[default]
    Error,
    Warning,
    Note,
}

impl Diagnostic {
    pub fn new(message: String, path: Option<String>, position: Position) -> Self {
        Self {
            message,
            hint: None,
            level: Default::default(),
            kind: DiagnosticKind::Compiler { path, position },
        }
    }

    pub fn expected(expected: String, path: Option<String>, position: Position) -> Self {
        Self {
            message: format!("expected {}", expected),
            hint: None,
            level: Default::default(),
            kind: DiagnosticKind::Compiler { path, position },
        }
    }

    pub fn expected_found(
        expected: String,
        found: String,
        path: Option<String>,
        position: Position,
    ) -> Self {
        Self {
            message: format!("expected {}, found {}", expected, found),
            hint: None,
            level: Default::default(),
            kind: DiagnosticKind::Compiler { path, position },
        }
    }

    pub fn expected_before(
        expected: String,
        token: String,
        path: Option<String>,
        position: Position,
    ) -> Self {
        Self {
            message: format!("expected {} before {}", expected, token),
            hint: None,
            level: Default::default(),
            kind: DiagnosticKind::Compiler { path, position },
        }
    }

    pub fn expected_after(
        expected: String,
        token: String,
        path: Option<String>,
        position: Position,
    ) -> Self {
        Self {
            message: format!("expected {} after {}", expected, token),
            hint: None,
            level: Default::default(),
            kind: DiagnosticKind::Compiler { path, position },
        }
    }

    pub fn unexpected(found: String, path: Option<String>, position: Position) -> Self {
        Self {
            message: format!("unexpected {}", found),
            hint: None,
            level: Default::default(),
            kind: DiagnosticKind::Compiler { path, position },
        }
    }

    pub fn other(message: String) -> Self {
        Self {
            message,
            hint: None,
            level: Default::default(),
            kind: DiagnosticKind::Other,
        }
    }

    pub const fn level(mut self, level: DiagnosticLevel) -> Self {
        self.level = level;
        self
    }

    pub fn with_hint(mut self, hint: String) -> Self {
        self.hint = Some(hint);
        self
    }
}

impl Error for Diagnostic {}

impl fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.level {
            DiagnosticLevel::Error => write!(f, "{}", "error: ".red().bold())?,
            DiagnosticLevel::Warning => write!(f, "{}", "warning: ".yellow().bold())?,
            DiagnosticLevel::Note => write!(f, "{}", "note: ".blue().bold())?,
        }

        write!(f, "{}", self.message)?;
        match &self.kind {
            DiagnosticKind::Compiler { path, position } => {
                let Position { line, column, .. } = &position;
                write!(f, "\n  {} ", "-->".blue().bold())?;
                if let Some(path) = path {
                    write!(f, "{}:", path)?;
                }
                writeln!(f, "{}:{}", line, column)?;

                if let Some(path) = path {
                    let files = get_files();
                    let files = files.lock().unwrap();
                    let source = files.get(path);

                    if let Some(source) = source {
                        let mut start = position.span.start;
                        while start > 0 && source.chars().nth(start - 1) != Some('\n') {
                            start -= 1;
                        }
                        let mut end = position.span.end;
                        while end < source.len() && source.chars().nth(end) != Some('\n') {
                            end += 1;
                        }

                        let source = source[start..end].to_owned();
                        let end = position.span.end - start;
                        let start = position.span.start - start;

                        for (i, line) in source.lines().enumerate() {
                            if i > 0 {
                                writeln!(f)?;
                            }
                            writeln!(f, "{:4} | {}", i + position.line, line)?;
                            write!(
                                f,
                                "     | {}{}",
                                " ".repeat(start),
                                "^".repeat(end - start).bright_red()
                            )?;
                        }
                    }
                }
            }
            DiagnosticKind::Other => {}
        }

        if let Some(hint) = &self.hint {
            write!(f, "\nhint: {}", hint)?;
        }
        writeln!(f)?;

        Ok(())
    }
}
