use std::fmt;

#[derive(Debug, Clone)]
pub enum VMErrorKind {
    RuntimeError,
    IllegalOperation,
    ShellCommand,
}

impl fmt::Display for VMErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            VMErrorKind::RuntimeError => {
                write!(f, "Unexpected runtime error")
            }
            VMErrorKind::IllegalOperation => {
                write!(f, "Illegal operation")
            }
            VMErrorKind::ShellCommand => {
                write!(f, "Shell command execution error")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct VMError {
    kind: VMErrorKind,
    msg: String,
}

impl VMError {
    pub fn new(kind: VMErrorKind, msg: String) -> Self {
        VMError { kind, msg }
    }
}

impl fmt::Display for VMError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.kind, self.msg)
    }
}
