use std::fmt;

#[derive(Debug, Clone)]
pub enum CompilationErrorKind {
    GeneralError,
    UnresolvedSymbol,
    ModuleImport(String),
}

impl fmt::Display for CompilationErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CompilationErrorKind::GeneralError => {
                write!(f, "Unexpected compilation error")
            }
            CompilationErrorKind::UnresolvedSymbol => {
                write!(f, "Unresolved symbol")
            }
            CompilationErrorKind::ModuleImport(path) => {
                write!(f, "Error importing module \"{}\"", path)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct CompilationError {
    kind: CompilationErrorKind,
    msg: String,
}

impl CompilationError {
    pub fn new(kind: CompilationErrorKind, msg: String) -> Self {
        CompilationError { kind, msg }
    }
}

impl fmt::Display for CompilationError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.kind, self.msg)
    }
}
