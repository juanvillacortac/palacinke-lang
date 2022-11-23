use std::fmt;

use pk_lexer::tokens::Token;

#[derive(Debug, Clone)]
pub enum ParseErrorKind {
    UnexpectedToken(Token),
}

impl fmt::Display for ParseErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseErrorKind::UnexpectedToken((_, pos)) => {
                write!(f, "Unexpected Token at pos {}:{}", pos.start, pos.end)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParseError {
    kind: ParseErrorKind,
    msg: String,
}

impl ParseError {
    pub fn new(kind: ParseErrorKind, msg: String) -> Self {
        ParseError { kind, msg }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.kind, self.msg)
    }
}
