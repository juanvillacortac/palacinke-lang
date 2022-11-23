pub type BlockStatement = Vec<Statement>;
pub type Module = BlockStatement;

#[derive(PartialEq, Clone, Debug)]
pub enum Statement {
    Expression(Expression),
    Import(Option<Ident>, Expression),
    Let(Ident, Expression),
    Assign(Ident, Expression),
    FunctionLet(Ident, Expression),
    Return(Expression),
    Do(Option<Expression>, BlockStatement),
    Stop,
    Next,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Literal {
    Number(f64),
    String(String),
    Bool(bool),
    Array(Vec<Expression>),
    Hash(Vec<(Expression, Expression)>),
    Nil,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Expression {
    Ident(Ident),
    Literal(Literal),
    Prefix(Prefix, Box<Expression>),
    Infix(Infix, Box<Expression>, Box<Expression>),
    Index(Box<Expression>, Box<Expression>),
    If {
        cond: Box<Expression>,
        consequence: BlockStatement,
        alternative: Option<BlockStatement>,
    },
    Function {
        params: Vec<Ident>,
        body: BlockStatement,
    },
    Call {
        func: Box<Expression>,
        args: Vec<Expression>,
    },
}

#[derive(PartialEq, Clone, Debug)]
pub struct Ident(pub String);

#[derive(PartialEq, Clone, Debug)]
pub enum Prefix {
    Plus,
    Minus,
    Not,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Infix {
    Plus,
    Minus,
    Divide,
    Multiply,
    Pow,
    Mod,
    Equal,
    NotEqual,
    GreaterThanEqual,
    GreaterThan,
    LessThanEqual,
    LessThan,
    And,
    Or,
}

#[derive(PartialEq, PartialOrd, Debug, Clone)]
pub enum Precedence {
    Lowest,
    BoolUnions,  // || &&
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // myFunction(x)
    Index,       // array[index]
}
