use std::{error::Error, fmt};

use colored::Colorize;

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
        line: usize,
        column: usize,
    },
    Other,
}

#[derive(Debug, Default)]
pub enum DiagnosticLevel {
    #[default]
    Error,
    Warning,
    Note,
}

impl Diagnostic {
    pub fn new(message: String, path: Option<String>, line: usize, column: usize) -> Self {
        Self {
            message,
            hint: None,
            level: Default::default(),
            kind: DiagnosticKind::Compiler { path, line, column },
        }
    }

    pub fn expected(expected: String, path: Option<String>, line: usize, column: usize) -> Self {
        Self {
            message: format!("expected {}", expected),
            hint: None,
            level: Default::default(),
            kind: DiagnosticKind::Compiler { path, line, column },
        }
    }

    pub fn expected_found(
        expected: String,
        found: String,
        path: Option<String>,
        line: usize,
        column: usize,
    ) -> Self {
        Self {
            message: format!("expected {}, found {}", expected, found),
            hint: None,
            level: Default::default(),
            kind: DiagnosticKind::Compiler { path, line, column },
        }
    }

    pub fn expected_before(
        expected: String,
        token: String,
        path: Option<String>,
        line: usize,
        column: usize,
    ) -> Self {
        Self {
            message: format!("expected {} before {}", expected, token),
            hint: None,
            level: Default::default(),
            kind: DiagnosticKind::Compiler { path, line, column },
        }
    }

    pub fn expected_after(
        expected: String,
        token: String,
        path: Option<String>,
        line: usize,
        column: usize,
    ) -> Self {
        Self {
            message: format!("expected {} after {}", expected, token),
            hint: None,
            level: Default::default(),
            kind: DiagnosticKind::Compiler { path, line, column },
        }
    }

    pub fn unexpected(found: String, path: Option<String>, line: usize, column: usize) -> Self {
        Self {
            message: format!("unexpected {}", found),
            hint: None,
            level: Default::default(),
            kind: DiagnosticKind::Compiler { path, line, column },
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
            DiagnosticKind::Compiler { path, line, column } => {
                write!(f, "\n  {} ", "-->".blue().bold())?;
                if let Some(path) = path {
                    write!(f, "{}:", path)?;
                }
                write!(f, "{}:{}", line, column)?;
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
