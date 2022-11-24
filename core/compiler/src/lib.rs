pub mod code;
pub mod errors;
pub mod objects;
pub mod symbols_table;

use std::{cell::RefCell, rc::Rc, vec};

use code::*;
use errors::*;
use objects::{Float, Object};
use pk_parser::ast::*;
use symbols_table::SymbolTable;

pub type CompiledInstructions = Vec<u8>;
pub type ConstantsPool = Vec<Rc<Object>>;

pub fn new_constants_pool() -> ConstantsPool {
    vec![]
}

#[derive(Debug, Clone)]
pub struct EmitedInstruction(Instruction, usize);

#[derive(Debug, Clone)]
pub struct CompiledBytecode {
    pub instructions: CompiledInstructions,
    pub constants: ConstantsPool,
}

pub struct Compiler {
    instructions: CompiledInstructions,
    constants: Rc<RefCell<ConstantsPool>>,
    last_instruction: Option<EmitedInstruction>,
    previous_instruction: Option<EmitedInstruction>,
    symbols_table: Rc<RefCell<SymbolTable>>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            instructions: vec![],
            constants: Rc::new(RefCell::new(vec![])),
            last_instruction: None,
            previous_instruction: None,
            symbols_table: Rc::new(RefCell::new(SymbolTable::new())),
        }
    }
    pub fn new_with_state(
        symbols_table: Rc<RefCell<SymbolTable>>,
        constants: Rc<RefCell<ConstantsPool>>,
    ) -> Self {
        let mut compiler = Self::new();
        compiler.symbols_table = Rc::clone(&symbols_table);
        compiler.constants = Rc::clone(&constants);
        compiler
    }

    fn get_bytecode(&self) -> CompiledBytecode {
        return CompiledBytecode {
            instructions: self.instructions.clone(),
            constants: self.constants.borrow().clone(),
        };
    }

    fn emit(&mut self, instruction: Instruction, operands: &Operands) -> usize {
        let packer = InstructionPacker(ByteEndianness::Big);
        let encoded = packer.encode_instruction(instruction.clone(), operands);

        let pos = self.add_instructions(&encoded);

        let previous_instruction = self.last_instruction.clone();
        let last_instruction = Some(EmitedInstruction(instruction, pos));

        self.previous_instruction = previous_instruction;
        self.last_instruction = last_instruction;

        pos
    }

    fn add_instructions(&mut self, instructions: &CompiledInstructions) -> usize {
        let new_pos = self.instructions.len();
        self.instructions.append(&mut instructions.clone());
        new_pos
    }

    fn add_constant(&mut self, obj: Object) -> usize {
        self.constants.borrow_mut().push(Rc::new(obj));
        self.constants.borrow().len() - 1
    }

    fn replace_instructions(&mut self, pos: usize, new_instructions: CompiledInstructions) {
        for (idx, byte) in new_instructions.iter().enumerate() {
            self.instructions[pos + idx] = *byte;
        }
    }

    fn replace_operands(&mut self, inst_pos: usize, operands: &Operands) {
        let packer = InstructionPacker(ByteEndianness::Big);
        match self.instructions.get(inst_pos) {
            Some(byte) => {
                let instruction = Instruction::from_u8(*byte);
                let encoded = packer.encode_instruction(instruction, operands);

                self.replace_instructions(inst_pos, encoded);
            }
            _ => {}
        }
    }

    pub fn compile(&mut self, module: Module) -> Result<CompiledBytecode, CompilationError> {
        match self.compile_block(&module) {
            Some(err) => return Err(err),
            _ => Ok(self.get_bytecode()),
        }
    }

    pub fn compile_block(&mut self, block: &BlockStatement) -> Option<CompilationError> {
        for statement in block {
            let error = self.compile_statement(&statement);
            if error.is_some() {
                return error;
            }
        }
        None
    }

    pub fn compile_statement(&mut self, statement: &Statement) -> Option<CompilationError> {
        match statement {
            Statement::Let(Ident(ident), expr) => {
                let err = self.compile_expression(expr);
                if err.is_some() {
                    return err;
                }
                let symbol = self.symbols_table.borrow_mut().define(ident);
                self.emit(Instruction::SetGlobal, &vec![symbol.index]);
                None
            }
            Statement::Expression(expr) => {
                let err = self.compile_expression(expr);
                if err.is_none() {
                    self.emit(Instruction::Pop, &vec![]);
                }
                err
            }
            stmt => Some(CompilationError::new(
                CompilationErrorKind::UnresolvedSymbol,
                format!("Statement \"{:#?}\" not implemented yet", stmt),
            )),
        }
    }

    pub fn compile_expression(&mut self, expr: &Expression) -> Option<CompilationError> {
        match expr {
            Expression::Prefix(_, _) => self.compile_prefix(expr),
            Expression::Infix(_, _, _) => self.compile_infix_expr(expr),
            Expression::Literal(literal) => self.compile_literal(literal),
            Expression::Ident(Ident(ident)) => {
                let symbol = self.symbols_table.borrow().resolve(ident);
                if symbol.is_none() {
                    return Some(CompilationError::new(
                        CompilationErrorKind::UnresolvedSymbol,
                        format!("variable {} is not defined", ident),
                    ));
                }
                self.emit(Instruction::GetGlobal, &vec![symbol.unwrap().index]);
                None
            }
            Expression::If { .. } => self.compile_if_expr(expr),
            stmt => Some(CompilationError::new(
                CompilationErrorKind::UnresolvedSymbol,
                format!("Expression \"{:?}\" not implemented yet", stmt),
            )),
        }
    }

    pub fn remove_last_pop(&mut self) {
        match self.last_instruction {
            Some(EmitedInstruction(Instruction::Pop, pos)) => {
                self.instructions = self.instructions[..pos].to_vec();
                self.last_instruction = self.previous_instruction.clone();
            }
            _ => {}
        }
    }

    pub fn compile_if_expr(&mut self, expr: &Expression) -> Option<CompilationError> {
        let Expression::If { cond, consequence, alternative } = expr else { return None };

        match self.compile_expression(cond) {
            Some(err) => return Some(err),
            _ => {}
        }

        let jump_not_pos = self.emit(Instruction::JumpNot, &vec![9999 as usize]);

        match self.compile_block(&consequence) {
            Some(err) => return Some(err),
            _ => {}
        }

        self.remove_last_pop();

        let jump_pos = self.emit(Instruction::Jump, &vec![9999 as usize]);

        let after_cons_pos = self.instructions.len();
        self.replace_operands(jump_not_pos, &vec![after_cons_pos]);

        match alternative {
            Some(alternative) => {
                match self.compile_block(&alternative) {
                    Some(err) => return Some(err),
                    _ => {}
                }
                self.remove_last_pop();
            }
            None => {
                self.emit(Instruction::Nil, &vec![]);
            }
        };

        let after_alt_pos = self.instructions.len();
        self.replace_operands(jump_pos, &vec![after_alt_pos]);

        None
    }

    pub fn compile_infix_expr(&mut self, expr: &Expression) -> Option<CompilationError> {
        match expr {
            Expression::Infix(infix, left, right) => {
                let error = self.compile_expression(left);
                if error.is_some() {
                    return error;
                }
                let error = self.compile_expression(right);
                if error.is_some() {
                    return error;
                }
                self.compile_infix(infix)
            }
            _ => None,
        }
    }

    pub fn compile_prefix(&mut self, prefix: &Expression) -> Option<CompilationError> {
        match prefix {
            Expression::Prefix(prefix, right) => {
                self.compile_expression(right);
                match prefix {
                    Prefix::Minus => {
                        self.emit(Instruction::Negative, &vec![]);
                    }
                    Prefix::Not => {
                        self.emit(Instruction::Not, &vec![]);
                    }
                    _ => {}
                }
            }
            _ => {}
        }
        None
    }

    pub fn compile_infix(&mut self, infix: &Infix) -> Option<CompilationError> {
        match infix {
            Infix::Plus => {
                self.emit(Instruction::Add, &vec![]);
            }
            Infix::Minus => {
                self.emit(Instruction::Sub, &vec![]);
            }
            Infix::Multiply => {
                self.emit(Instruction::Mul, &vec![]);
            }
            Infix::Divide => {
                self.emit(Instruction::Div, &vec![]);
            }
            Infix::Mod => {
                self.emit(Instruction::Mod, &vec![]);
            }
            Infix::Pow => {
                self.emit(Instruction::Pow, &vec![]);
            }
            Infix::LessThan => {
                self.emit(Instruction::Lt, &vec![]);
            }
            Infix::GreaterThan => {
                self.emit(Instruction::Gt, &vec![]);
            }
            Infix::LessThanEqual => {
                self.emit(Instruction::Lte, &vec![]);
            }
            Infix::GreaterThanEqual => {
                self.emit(Instruction::Gte, &vec![]);
            }
            Infix::Equal => {
                self.emit(Instruction::Eq, &vec![]);
            }
            Infix::NotEqual => {
                self.emit(Instruction::Neq, &vec![]);
            }
            Infix::And => {
                self.emit(Instruction::And, &vec![]);
            }
            Infix::Or => {
                self.emit(Instruction::Or, &vec![]);
            }
        }
        None
    }

    pub fn compile_literal(&mut self, literal: &Literal) -> Option<CompilationError> {
        match literal {
            Literal::Number(val) => {
                let number = Object::Number(Float(*val));
                let operands = vec![self.add_constant(number)];
                self.emit(Instruction::Const, &operands);
                None
            }
            Literal::Bool(val) => {
                self.emit(
                    if *val {
                        Instruction::True
                    } else {
                        Instruction::False
                    },
                    &vec![],
                );
                None
            }
            Literal::Nil => {
                self.emit(Instruction::Nil, &vec![]);
                None
            }
            _ => None,
        }
    }
}
