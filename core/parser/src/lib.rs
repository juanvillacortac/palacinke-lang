use pk_lexer::tokens::{RawToken, Token, TokenList};

pub mod ast;
pub mod errors;

use ast::*;
use errors::*;

pub type ParseErrors = Vec<ParseError>;

pub struct Parser {
    tokens: TokenList,
    cursor: usize,
    errors: ParseErrors,
}

impl Parser {
    pub fn new(tokens: TokenList) -> Self {
        let parser = Parser {
            tokens,
            cursor: 0,
            errors: vec![],
        };
        parser
    }

    pub fn from_source(source: &str) -> Self {
        Self::new(pk_lexer::tokenize(source))
    }

    pub fn parse(&mut self) -> Result<Module, ParseErrors> {
        let mut program: Module = vec![];

        while !self.current_token_is(&RawToken::EOF) {
            match self.parse_statement() {
                Some(stmt) => program.push(stmt),
                None => {}
            }
            self.step();
        }

        if self.errors.len() > 0 {
            return Err(self.errors.clone());
        }

        Ok(program)
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        if let (Some((current, _)), next) = (self.current_token(), self.next_token()) {
            match (current, next) {
                (RawToken::Let, _) => self.parse_let_stmt(),
                (RawToken::Function, _) => self.parse_func_stmt(),
                (RawToken::Import, _) => self.parse_import_stmt(),
                (RawToken::Return, _) => self.parse_return_stmt(),
                (RawToken::Stop, _) => self.parse_stop_stmt(),
                (RawToken::Next, _) => self.parse_next_stmt(),
                (RawToken::Do, _) => self.parse_do_stmt(),
                (RawToken::Ident(_), Some((RawToken::Assign, _))) => self.parse_assign_stmt(),
                _ => self.parse_expr_statement(),
            }
        } else {
            None
        }
    }

    fn parse_expr_statement(&mut self) -> Option<Statement> {
        match self.parse_expression(Precedence::Lowest) {
            Some(expr) => {
                if self.next_token_is(&RawToken::Semicolon) {
                    self.step();
                }
                Some(Statement::Expression(expr))
            }
            None => None,
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let Some((current, _)) = self.current_token() else { return None };

        // parse prefixes
        let mut left = match current {
            RawToken::Ident(_) => self.parse_ident_expr(),
            RawToken::Number(_) | RawToken::HexInteger(_) => self.parse_number_expr(),
            RawToken::String(_) => self.parse_string_expr(),
            RawToken::Nil => self.parse_nil_expr(),
            RawToken::True | RawToken::False => self.parse_boolean_expr(),
            RawToken::Plus | RawToken::Bang | RawToken::Minus => self.parse_prefix_expr(),
            RawToken::ParenL => self.parse_grouped_expr(),
            RawToken::BracketL => self.parse_array_expr(),
            RawToken::BraceL => self.parse_hash_expr(),
            RawToken::Function => self.parse_func_expr(),
            RawToken::If => self.parse_if_expr(),
            _ => {
                self.error_no_prefix_parser();
                return None;
            }
        };

        // parse infixes
        while left.is_some()
            && precedence < self.next_token_precedence().unwrap_or(Precedence::Lowest)
            && !self.next_token_is(&RawToken::Semicolon)
        {
            match self.next_token() {
                Some((next_token, _)) => match next_token {
                    RawToken::Plus
                    | RawToken::Minus
                    | RawToken::Slash
                    | RawToken::Asterisk
                    | RawToken::Equal
                    | RawToken::NotEqual
                    | RawToken::LessThan
                    | RawToken::LessThanEqual
                    | RawToken::GreaterThan
                    | RawToken::Mod
                    | RawToken::Pow
                    | RawToken::And
                    | RawToken::Or
                    | RawToken::GreaterThanEqual => {
                        self.step();
                        left = self.parse_infix_expr(left.unwrap());
                    }
                    RawToken::BracketL => {
                        self.step();
                        left = self.parse_index_expr(left.unwrap());
                    }
                    RawToken::ParenL => {
                        self.step();
                        left = self.parse_call_expr(left.unwrap());
                    }
                    _ => return left,
                },
                _ => return left,
            }
        }

        left
    }

    fn parse_if_expr(&mut self) -> Option<Expression> {
        self.step();

        let cond = match self.parse_expression(Precedence::Lowest) {
            Some(expr) => expr,
            None => return None,
        };

        if !self.expect_next_token(RawToken::BraceL) {
            return None;
        }

        let consequence = self.parse_block_stmt();
        let mut alternative = None;

        if self.next_token_is(&RawToken::Else) {
            self.step();

            if !self.expect_next_token(RawToken::BraceL) {
                return None;
            }

            alternative = Some(self.parse_block_stmt());
        }

        Some(Expression::If {
            cond: Box::new(cond),
            consequence,
            alternative,
        })
    }

    fn parse_func_expr(&mut self) -> Option<Expression> {
        if !self.expect_next_token(RawToken::ParenL) {
            return None;
        }

        let params = match self.parse_func_params() {
            Some(params) => params,
            None => return None,
        };

        if !self.expect_next_token(RawToken::BraceL) {
            return None;
        }

        Some(Expression::Function {
            params,
            body: self.parse_block_stmt(),
        })
    }

    fn parse_block_stmt(&mut self) -> BlockStatement {
        self.step();

        let mut block = vec![];

        while !self.current_token_is(&RawToken::BraceR) && !self.current_token_is(&RawToken::EOF) {
            match self.parse_statement() {
                Some(stmt) => block.push(stmt),
                None => {}
            }
            self.step();
        }

        block
    }

    fn parse_func_params(&mut self) -> Option<Vec<Ident>> {
        let mut params = vec![];

        if self.next_token_is(&RawToken::ParenR) {
            self.step();
            return Some(params);
        }

        self.step();

        match self.parse_ident() {
            Some(ident) => params.push(ident),
            None => return None,
        };

        while self.next_token_is(&RawToken::Comma) {
            self.step();
            self.step();

            match self.parse_ident() {
                Some(ident) => params.push(ident),
                None => return None,
            };
        }

        if !self.expect_next_token(RawToken::ParenR) {
            return None;
        }

        Some(params)
    }

    fn parse_call_expr(&mut self, func: Expression) -> Option<Expression> {
        let args = match self.parse_expr_list(RawToken::ParenR) {
            Some(args) => args,
            None => return None,
        };

        Some(Expression::Call {
            func: Box::new(func),
            args,
        })
    }

    fn parse_index_expr(&mut self, left: Expression) -> Option<Expression> {
        self.step();

        let index = match self.parse_expression(Precedence::Lowest) {
            Some(expr) => expr,
            None => return None,
        };

        if !self.expect_next_token(RawToken::BracketR) {
            return None;
        }

        Some(Expression::Index(Box::new(left), Box::new(index)))
    }

    fn parse_grouped_expr(&mut self) -> Option<Expression> {
        self.step();

        let expr = self.parse_expression(Precedence::Lowest);

        if !self.expect_next_token(RawToken::ParenR) {
            None
        } else {
            expr
        }
    }

    fn parse_import_stmt(&mut self) -> Option<Statement> {
        let Some((next, _)) = self.next_token() else { return None };
        match next {
            RawToken::Ident(_) | RawToken::String(_) => self.step(),
            _ => return None,
        }

        let ident = self.parse_ident();

        let Some((token, _)) = (if ident.is_none() {
            self.current_token()
        } else {
            self.next_token()
        }) else {
            return None;
        };

        match token {
            RawToken::String(_) => {
                if ident.is_some() {
                    self.step()
                }
            }
            _ => {
                self.error_next_token(&RawToken::String(String::from("")));
                return None;
            }
        }

        let expr = match self.parse_string_expr() {
            Some(expr) => expr,
            None => return None,
        };

        Some(Statement::Import(ident, expr))
    }

    fn parse_let_stmt(&mut self) -> Option<Statement> {
        let Some((next, _)) = self.next_token() else { return None };
        match next {
            RawToken::Ident(_) => self.step(),
            _ => return None,
        }
        let ident = match self.parse_ident() {
            Some(ident) => ident,
            _ => return None,
        };

        if !self.expect_next_token(RawToken::Assign) {
            return None;
        }

        self.step();

        let expr = match self.parse_expression(Precedence::Lowest) {
            Some(expr) => expr,
            None => return None,
        };

        if self.next_token_is(&RawToken::Semicolon) {
            self.step();
        }

        Some(Statement::Let(ident, expr))
    }

    fn parse_assign_stmt(&mut self) -> Option<Statement> {
        let ident = match self.parse_ident() {
            Some(ident) => ident,
            _ => return None,
        };

        if !self.expect_next_token(RawToken::Assign) {
            return None;
        }

        self.step();

        let expr = match self.parse_expression(Precedence::Lowest) {
            Some(expr) => expr,
            None => return None,
        };

        if self.next_token_is(&RawToken::Semicolon) {
            self.step();
        }

        Some(Statement::Assign(ident, expr))
    }

    fn parse_func_stmt(&mut self) -> Option<Statement> {
        let Some((next, _)) = self.next_token() else { return None };
        match next {
            RawToken::Ident(_) => self.step(),
            _ => return None,
        }
        let ident = match self.parse_ident() {
            Some(ident) => ident,
            _ => return None,
        };

        let expr = match self.parse_func_expr() {
            Some(expr) => expr,
            None => return None,
        };

        Some(Statement::FunctionLet(ident, expr))
    }

    fn parse_stop_stmt(&mut self) -> Option<Statement> {
        self.step();

        Some(Statement::Stop)
    }

    fn parse_next_stmt(&mut self) -> Option<Statement> {
        self.step();

        Some(Statement::Next)
    }

    fn parse_return_stmt(&mut self) -> Option<Statement> {
        self.step();

        let expr = match self.parse_expression(Precedence::Lowest) {
            Some(expr) => expr,
            None => return None,
        };

        if self.next_token_is(&RawToken::Semicolon) {
            self.step();
        }

        Some(Statement::Return(expr))
    }

    fn parse_do_stmt(&mut self) -> Option<Statement> {
        let mut cond = None;
        if let Some((RawToken::While, _)) = self.next_token() {
            self.step();
            self.step();
            cond = self.parse_expression(Precedence::Lowest);
        }

        if !self.expect_next_token(RawToken::BraceL) {
            return None;
        }

        let block = self.parse_block_stmt();

        Some(Statement::Do(cond, block))
    }

    fn parse_ident(&mut self) -> Option<Ident> {
        match self.current_token() {
            Some((token, _)) => match token {
                RawToken::Ident(name) => Some(Ident(name.clone())),
                _ => None,
            },
            None => None,
        }
    }

    fn parse_prefix_expr(&mut self) -> Option<Expression> {
        let Some((current, _)) = self.current_token() else { return None };
        let prefix = match current {
            RawToken::Bang => Prefix::Not,
            RawToken::Minus => Prefix::Minus,
            RawToken::Plus => Prefix::Plus,
            _ => return None,
        };

        self.step();

        match self.parse_expression(Precedence::Prefix) {
            Some(expr) => Some(Expression::Prefix(prefix, Box::new(expr))),
            None => None,
        }
    }

    fn parse_infix_expr(&mut self, left: Expression) -> Option<Expression> {
        let Some((current, _)) = self.current_token() else { return None };
        let infix = match current {
            RawToken::Plus => Infix::Plus,
            RawToken::Minus => Infix::Minus,
            RawToken::Slash => Infix::Divide,
            RawToken::Asterisk => Infix::Multiply,
            RawToken::Pow => Infix::Pow,
            RawToken::Mod => Infix::Mod,
            RawToken::Equal => Infix::Equal,
            RawToken::NotEqual => Infix::NotEqual,
            RawToken::LessThan => Infix::LessThan,
            RawToken::LessThanEqual => Infix::LessThanEqual,
            RawToken::GreaterThan => Infix::GreaterThan,
            RawToken::GreaterThanEqual => Infix::GreaterThanEqual,
            RawToken::And => Infix::And,
            RawToken::Or => Infix::Or,
            _ => return None,
        };

        let precedence = token_to_precedence(&current);

        self.step();

        match self.parse_expression(precedence) {
            Some(expr) => Some(Expression::Infix(infix, Box::new(left), Box::new(expr))),
            None => None,
        }
    }

    fn parse_ident_expr(&mut self) -> Option<Expression> {
        match self.parse_ident() {
            Some(ident) => Some(Expression::Ident(ident)),
            None => None,
        }
    }

    fn parse_number_expr(&mut self) -> Option<Expression> {
        match self.current_token() {
            Some((token, _)) => match token {
                RawToken::Number(number) | RawToken::HexInteger(number) => {
                    Some(Expression::Literal(Literal::Number(number)))
                }
                _ => None,
            },
            None => None,
        }
    }

    fn parse_boolean_expr(&mut self) -> Option<Expression> {
        match self.current_token() {
            Some((token, _)) => match token {
                RawToken::True => Some(Expression::Literal(Literal::Bool(true))),
                RawToken::False => Some(Expression::Literal(Literal::Bool(false))),
                _ => None,
            },
            _ => None,
        }
    }

    fn parse_string_expr(&mut self) -> Option<Expression> {
        match self.current_token() {
            Some((token, _)) => match token {
                RawToken::String(string) => Some(Expression::Literal(Literal::String(
                    String::from(&string[1..string.len() - 1]),
                ))),
                _ => None,
            },
            None => None,
        }
    }

    fn parse_nil_expr(&mut self) -> Option<Expression> {
        match self.current_token() {
            Some((token, _)) => match token {
                RawToken::Nil => Some(Expression::Literal(Literal::Nil)),
                _ => None,
            },
            None => None,
        }
    }

    fn parse_array_expr(&mut self) -> Option<Expression> {
        match self.parse_expr_list(RawToken::BracketR) {
            Some(list) => Some(Expression::Literal(Literal::Array(list))),
            None => None,
        }
    }

    fn parse_hash_expr(&mut self) -> Option<Expression> {
        let mut pairs = Vec::new();

        while !self.next_token_is(&RawToken::BraceR) {
            self.step();

            let key = match self.parse_expression(Precedence::Lowest) {
                Some(expr) => match expr {
                    Expression::Ident(Ident(name)) => Expression::Literal(Literal::String(name)),
                    _ => expr,
                },
                None => return None,
            };

            if !self.expect_next_token(RawToken::Colon) {
                return None;
            }

            self.step();

            let value = match self.parse_expression(Precedence::Lowest) {
                Some(expr) => expr,
                None => return None,
            };

            pairs.push((key, value));

            if !self.next_token_is(&RawToken::BraceR) && !self.expect_next_token(RawToken::Comma) {
                return None;
            }
        }

        if !self.expect_next_token(RawToken::BraceR) {
            return None;
        }

        Some(Expression::Literal(Literal::Hash(pairs)))
    }

    fn parse_expr_list(&mut self, end: RawToken) -> Option<Vec<Expression>> {
        let mut list = vec![];

        if self.next_token_is(&end) {
            self.step();
            return Some(list);
        }

        self.step();

        match self.parse_expression(Precedence::Lowest) {
            Some(expr) => list.push(expr),
            None => return None,
        }

        while self.next_token_is(&RawToken::Comma) {
            self.step();
            self.step();

            match self.parse_expression(Precedence::Lowest) {
                Some(expr) => list.push(expr),
                None => return None,
            }
        }

        if !self.expect_next_token(end) {
            return None;
        }

        Some(list)
    }

    fn current_token_is(&self, token: &RawToken) -> bool {
        match self.current_token() {
            Some((current, _)) => &current == token,
            None => token == &RawToken::EOF,
        }
    }
    fn next_token_is(&self, token: &RawToken) -> bool {
        match self.next_token() {
            Some((current, _)) => &current == token,
            None => token == &RawToken::EOF,
        }
    }

    fn current_token(&self) -> Option<Token> {
        self.tokens.get(self.cursor).cloned()
    }
    fn next_token(&self) -> Option<Token> {
        self.tokens.get(self.cursor + 1).cloned()
    }

    fn step(&mut self) {
        self.cursor += 1;
    }

    fn expect_next_token(&mut self, tok: RawToken) -> bool {
        if self.next_token_is(&tok) {
            self.step();
            return true;
        } else {
            self.error_next_token(&tok);
            return false;
        }
    }

    fn error_next_token(&mut self, tok: &RawToken) {
        let (Some(_), Some(next)) = (self.current_token(), self.next_token()) else { return };
        self.errors.push(ParseError::new(
            ParseErrorKind::UnexpectedToken(next.clone()),
            format!(
                "expected next token to be \"{:?}\", got \"{:?}\" instead",
                tok, next.0,
            ),
        ));
    }

    fn error_no_prefix_parser(&mut self) {
        let Some(current) = self.current_token() else { return };
        self.errors.push(ParseError::new(
            ParseErrorKind::UnexpectedToken(current.clone()),
            format!("no prefix parse function for \"{:?}\" found", current.0,),
        ));
    }

    fn next_token_precedence(&mut self) -> Option<Precedence> {
        match self.next_token() {
            Some((token, _)) => Some(token_to_precedence(&token)),
            _ => None,
        }
    }
}

fn token_to_precedence(token: &RawToken) -> Precedence {
    match token {
        RawToken::Equal | RawToken::NotEqual => Precedence::Equals,
        RawToken::LessThan | RawToken::LessThanEqual => Precedence::LessGreater,
        RawToken::GreaterThan | RawToken::GreaterThanEqual => Precedence::LessGreater,
        RawToken::Plus | RawToken::Minus => Precedence::Sum,
        RawToken::Slash | RawToken::Asterisk | RawToken::Mod | RawToken::Pow => Precedence::Product,
        RawToken::BracketL => Precedence::Index,
        RawToken::ParenL => Precedence::Call,
        RawToken::And | RawToken::Or => Precedence::BoolUnions,
        _ => Precedence::Lowest,
    }
}
