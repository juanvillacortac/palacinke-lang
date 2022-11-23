pub use logos::Logos;
use logos::Span;

pub type Token = (RawToken, Span);
pub type TokenList = Vec<Token>;

pub fn tokenize(source: &str) -> TokenList {
    RawToken::lexer(source).spanned().collect()
}

#[test]
fn test() {
    let tokens = tokenize("'let' x = 10");
    assert_eq!(RawToken::Ident("x".to_string()), tokens[0].0);
}

#[derive(Logos, PartialEq, Clone, Debug)]
pub enum RawToken {
    // Keywords
    #[token("fn")]
    Function,
    #[token("let")]
    Let,
    #[token("import")]
    Import,
    #[token("true")]
    True,
    #[token("do")]
    Do,
    #[token("while")]
    While,
    #[token("stop")]
    Stop,
    #[token("next")]
    Next,
    #[token("false")]
    False,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("return")]
    Return,

    // Operators
    #[token("=")]
    Assign,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("!")]
    Bang,
    #[token("*")]
    Asterisk,
    #[token("/")]
    Slash,
    #[token("^")]
    Pow,
    #[token("%")]
    Mod,

    #[token("<")]
    LessThan,
    #[token(">")]
    GreaterThan,
    #[token("==")]
    Equal,
    #[token("!=")]
    NotEqual,
    #[token("<=")]
    LessThanEqual,
    #[token(">=")]
    GreaterThanEqual,

    #[token("&&")]
    And,

    #[token("||")]
    Or,

    // Delimiters
    #[token(",")]
    Comma,
    #[token(";")]
    Semicolon,
    #[token(":")]
    Colon,
    #[token("(")]
    ParenL,
    #[token(")")]
    ParenR,
    #[token("{")]
    BraceL,
    #[token("}")]
    BraceR,
    #[token("[")]
    BracketL,
    #[token("]")]
    BracketR,

    #[token("nil")]
    Nil,

    // Identifiers + literals
    #[regex(r"[_a-zA-Z][_a-zA-Z0-9]*", |lex| String::from(lex.slice()))]
    Ident(String),
    #[regex("([0-9]+([.][0-9]*)?|[.][0-9]+)", |lex| lex.slice().parse())]
    Number(f64),
    #[regex(r#""(?:[^"]|\\")*""#, |lex| String::from(lex.slice()))]
    #[regex(r#"'(?:[^']|\\")*'"#, |lex| String::from(lex.slice()))]
    String(String),

    #[regex(r"//.*\n?", logos::skip)]
    #[regex(r#"/\*.*\*/"#, logos::skip)]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    #[error]
    Error,

    EOF,
}
