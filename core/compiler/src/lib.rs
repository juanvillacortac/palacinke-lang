pub mod code;
pub mod errors;
pub mod objects;
pub mod symbols_table;

use std::rc::Rc;

use code::*;
use errors::*;
use objects::{Float, Object};
use pk_parser::ast::*;
use symbols_table::{ConstantsPool, SymbolScope, SymbolTable};

#[derive(Debug, Clone)]
pub struct EmitedInstruction(Instruction, usize);
pub struct CompiledBytecode {
    pub instructions: CompiledInstructions,
    pub constants: ConstantsPool,
}

#[derive(Debug, Clone)]
pub struct CompilationScope {
    instructions: Vec<Instruction>,
    last_instruction: Option<EmitedInstruction>,
    previous_instruction: Option<EmitedInstruction>,
}

#[derive(Debug)]
pub struct Compiler<'a> {
    constants: &'a mut ConstantsPool,
    symbols_table: &'a mut SymbolTable,
    scopes: Vec<CompilationScope>,
    scope_idx: usize,
}

impl<'a> Compiler<'a> {
    pub fn new(symbols_table: &'a mut SymbolTable, constants: &'a mut ConstantsPool) -> Self {
        Self {
            constants,
            symbols_table,
            scopes: vec![CompilationScope {
                last_instruction: None,
                previous_instruction: None,
                instructions: vec![],
            }],
            scope_idx: 0,
        }
    }

    fn get_bytecode(&self) -> CompiledBytecode {
        let bytecode = CompiledBytecode {
            instructions: Instruction::compile_instructions(
                self.scopes[self.scope_idx].instructions.clone(),
            ),
            constants: self.constants.clone(),
        };
        bytecode
    }

    fn emit(&mut self, instruction: Instruction) -> usize {
        let pos = self.add_instructions(instruction.clone());

        let previous_instruction = self.scopes[self.scope_idx].last_instruction.clone();
        let last_instruction = Some(EmitedInstruction(instruction, pos));

        self.scopes[self.scope_idx].previous_instruction = previous_instruction;
        self.scopes[self.scope_idx].last_instruction = last_instruction;

        pos
    }

    fn add_instructions(&mut self, instruction: Instruction) -> usize {
        let new_pos = self.scopes[self.scope_idx].instructions.len();
        self.scopes[self.scope_idx].instructions.push(instruction);
        new_pos
    }

    fn add_constant(&mut self, obj: Rc<Object>) -> usize {
        self.constants.set_object(obj)
    }

    fn replace_instruction(&mut self, pos: usize, new_instruction: Instruction) {
        self.scopes[self.scope_idx].instructions[pos] = new_instruction;
    }

    fn enter_scope(&mut self) {
        let scope = CompilationScope {
            instructions: vec![],
            last_instruction: None,
            previous_instruction: None,
        };
        self.scopes.push(scope);
        self.scope_idx += 1;
        *self.symbols_table = SymbolTable::new_enclosed(self.symbols_table.clone());
    }

    fn leave_scope(&mut self) -> Vec<Instruction> {
        let instructions = self.scopes[self.scope_idx].instructions.clone();
        self.scopes = self.scopes[..self.scopes.len() - 1].to_vec();
        self.scope_idx -= 1;
        match &self.symbols_table.outer {
            Some(ref outer) => {
                *self.symbols_table = *outer.clone();
            }
            _ => {}
        }
        instructions
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
                let symbol = self.symbols_table.define(ident);
                let idx: u16 = symbol.index.try_into().unwrap();
                self.emit(match symbol.scope {
                    SymbolScope::Global => Instruction::SetGlobal(idx),
                    SymbolScope::Local => Instruction::SetLocal(idx),
                });
                None
            }
            Statement::FunctionLet(Ident(ident), expr) => {
                let err = self.compile_expression(expr);
                if err.is_some() {
                    return err;
                }
                let symbol = self.symbols_table.define(ident);
                let idx: u16 = symbol.index.try_into().unwrap();
                self.emit(match symbol.scope {
                    SymbolScope::Global => Instruction::SetGlobal(idx),
                    SymbolScope::Local => Instruction::SetLocal(idx),
                });
                None
            }
            Statement::Assign(Ident(ident), expr) => {
                let symbol = self.symbols_table.resolve(ident);
                if symbol.is_none() {
                    return Some(CompilationError::new(
                        CompilationErrorKind::UnresolvedSymbol,
                        format!("variable {} is not defined", ident),
                    ));
                }
                let symbol = self.symbols_table.define(ident);
                let idx: u16 = symbol.index.try_into().unwrap();
                let err = self.compile_expression(expr);
                if err.is_some() {
                    return err;
                }
                self.emit(match symbol.scope {
                    SymbolScope::Global => Instruction::SetGlobal(idx),
                    SymbolScope::Local => Instruction::SetLocal(idx),
                });
                None
            }
            Statement::Return(expr) => {
                let err = self.compile_expression(expr);
                if err.is_none() {
                    self.emit(Instruction::ReturnValue);
                }
                err
            }
            Statement::Expression(expr) => {
                let err = self.compile_expression(expr);
                if err.is_none() {
                    self.emit(Instruction::Pop);
                }
                err
            }
            stmt => Some(CompilationError::new(
                CompilationErrorKind::UnresolvedSymbol,
                format!("Statement \"{:#?}\" not implemented yet", stmt),
            )),
        }
    }

    fn last_instruction_is(&self, instruction: &Instruction) -> bool {
        if self.scopes[self.scope_idx].instructions.len() == 0 {
            return false;
        }
        if let Some(EmitedInstruction(op, _)) = self.scopes[self.scope_idx].last_instruction.clone()
        {
            return op == *instruction;
        }
        false
    }

    pub fn compile_expression(&mut self, expr: &Expression) -> Option<CompilationError> {
        match expr {
            Expression::Function { params, body } => {
                self.enter_scope();

                for param in params {
                    self.symbols_table.define(&param.0);
                }

                if let Some(err) = self.compile_block(body) {
                    return Some(err);
                }
                if self.last_instruction_is(&Instruction::Pop) {
                    self.replace_last_pop_with_return();
                }
                if !self.last_instruction_is(&Instruction::ReturnValue) {
                    self.emit(Instruction::Return);
                }
                let locals = self.symbols_table.num_defs;
                let instructions = self.leave_scope();
                let compiled_fn = Object::CompiledFunction {
                    instructions: Instruction::compile_instructions(instructions),
                    locals,
                };
                let idx = self.add_constant(Rc::new(compiled_fn));
                self.emit(Instruction::Const(idx as u16));
                None
            }
            Expression::Call { func, args } => {
                if let Some(err) = self.compile_expression(func) {
                    return Some(err);
                }
                for arg in args {
                    let err = self.compile_expression(arg);
                    if err.is_some() {
                        return err;
                    }
                }
                self.emit(Instruction::Call(args.len() as u16));
                None
            }
            Expression::Prefix(_, _) => self.compile_prefix(expr),
            Expression::Infix(_, _, _) => self.compile_infix_expr(expr),
            Expression::Literal(literal) => self.compile_literal(literal),
            Expression::Ident(Ident(ident)) => {
                let symbol = self.symbols_table.resolve(ident);
                if symbol.is_none() {
                    return Some(CompilationError::new(
                        CompilationErrorKind::UnresolvedSymbol,
                        format!("variable {} is not defined", ident),
                    ));
                }
                let idx = symbol.clone().unwrap().index as u16;
                self.emit(match symbol.unwrap().scope {
                    SymbolScope::Global => Instruction::GetGlobal(idx),
                    SymbolScope::Local => Instruction::GetLocal(idx),
                });
                None
            }
            Expression::Index(left, index) => {
                let err = self.compile_expression(left);
                if err.is_some() {
                    return err;
                }
                let err = self.compile_expression(index);
                if err.is_some() {
                    return err;
                }
                self.emit(Instruction::Index);
                None
            }
            Expression::If { .. } => self.compile_if_expr(expr),
            #[allow(unreachable_patterns)]
            stmt => Some(CompilationError::new(
                CompilationErrorKind::UnresolvedSymbol,
                format!("Expression \"{:?}\" not implemented yet", stmt),
            )),
        }
    }

    pub fn remove_last_pop(&mut self) {
        match self.scopes[self.scope_idx].last_instruction {
            Some(EmitedInstruction(Instruction::Pop, pos)) => {
                self.scopes[self.scope_idx].instructions =
                    self.scopes[self.scope_idx].instructions[..pos].to_vec();

                self.scopes[self.scope_idx].last_instruction =
                    self.scopes[self.scope_idx].previous_instruction.clone();
            }
            _ => {}
        }
    }

    pub fn replace_last_pop_with_return(&mut self) {
        match self.scopes[self.scope_idx].last_instruction {
            Some(EmitedInstruction(Instruction::Pop, pos)) => {
                self.replace_instruction(pos, Instruction::ReturnValue);

                if let Some(EmitedInstruction(_, pos)) =
                    self.scopes[self.scope_idx].last_instruction
                {
                    self.scopes[self.scope_idx].last_instruction =
                        Some(EmitedInstruction(Instruction::ReturnValue, pos));
                }
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

        let jump_not_pos = self.emit(Instruction::JumpNot(9999));

        match self.compile_block(&consequence) {
            Some(err) => return Some(err),
            _ => {}
        }

        self.remove_last_pop();

        let jump_pos = self.emit(Instruction::Jump(9999));

        let after_cons_pos = self.scopes[self.scope_idx].instructions.len();
        self.replace_instruction(jump_not_pos, Instruction::JumpNot(after_cons_pos as u16));

        match alternative {
            Some(alternative) => {
                match self.compile_block(&alternative) {
                    Some(err) => return Some(err),
                    _ => {}
                }
                self.remove_last_pop();
            }
            None => {
                self.emit(Instruction::Nil);
            }
        };

        let after_alt_pos = self.scopes[self.scope_idx].instructions.len();
        self.replace_instruction(jump_pos, Instruction::Jump(after_alt_pos as u16));

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
                        self.emit(Instruction::Negative);
                    }
                    Prefix::Not => {
                        self.emit(Instruction::Not);
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
                self.emit(Instruction::Add);
            }
            Infix::Minus => {
                self.emit(Instruction::Sub);
            }
            Infix::Multiply => {
                self.emit(Instruction::Mul);
            }
            Infix::Divide => {
                self.emit(Instruction::Div);
            }
            Infix::Mod => {
                self.emit(Instruction::Mod);
            }
            Infix::Pow => {
                self.emit(Instruction::Pow);
            }
            Infix::LessThan => {
                self.emit(Instruction::Lt);
            }
            Infix::GreaterThan => {
                self.emit(Instruction::Gt);
            }
            Infix::LessThanEqual => {
                self.emit(Instruction::Lte);
            }
            Infix::GreaterThanEqual => {
                self.emit(Instruction::Gte);
            }
            Infix::Equal => {
                self.emit(Instruction::Eq);
            }
            Infix::NotEqual => {
                self.emit(Instruction::Neq);
            }
            Infix::And => {
                self.emit(Instruction::And);
            }
            Infix::Or => {
                self.emit(Instruction::Or);
            }
        }
        None
    }

    pub fn compile_literal(&mut self, literal: &Literal) -> Option<CompilationError> {
        match literal {
            Literal::Number(val) => {
                let number = Object::Number(Float(*val));
                let idx = self.add_constant(Rc::new(number)) as u16;
                self.emit(Instruction::Const(idx));
                None
            }
            Literal::String(val) => {
                let string = Object::String(val.to_string());
                let idx = self.add_constant(Rc::new(string)) as u16;
                self.emit(Instruction::Const(idx));
                None
            }
            Literal::Array(exprs) => {
                for expr in exprs {
                    if let Some(err) = self.compile_expression(expr) {
                        return Some(err);
                    }
                }
                self.emit(Instruction::Array(exprs.len() as u16));
                None
            }
            Literal::Hash(objs) => {
                for (k, v) in objs.clone() {
                    if let Some(err) = self.compile_expression(&k) {
                        return Some(err);
                    }
                    if let Some(err) = self.compile_expression(&v) {
                        return Some(err);
                    }
                }
                self.emit(Instruction::Hash((objs.len() * 2) as u16));
                None
            }
            Literal::Bool(val) => {
                self.emit(Instruction::Bool(*val));
                None
            }
            Literal::Nil => {
                self.emit(Instruction::Nil);
                None
            }
            #[allow(unreachable_patterns)]
            _ => None,
        }
    }
}

pub struct BytecodeDecompiler;

impl BytecodeDecompiler {
    pub fn disassemble_function(instructions: &CompiledInstructions) -> String {
        let mut decoded_string = String::new();
        let mut idx = 0;

        let decoded = Instruction::decompile_instructions(instructions).unwrap();
        for op in decoded {
            decoded_string.push_str(&format!("{:0>8x} ", idx));
            decoded_string.push_str(&op.as_string());
            decoded_string.push('\n');
            idx = idx + 1;
        }

        return decoded_string;
    }

    pub fn disassemble_instructions(bytecode: &CompiledBytecode) -> String {
        let instructions = &bytecode.instructions;
        let decoded_string = BytecodeDecompiler::disassemble_function(instructions);
        return decoded_string;
    }

    pub fn disassemble_constants(bytecode: &CompiledBytecode) -> String {
        // constant pool:
        let constant_pool = &bytecode.constants;
        let mut decoded_string = String::new();
        let mut idx = 0;
        for item in &constant_pool.objects {
            match item.as_ref() {
                Object::CompiledFunction {
                    instructions: sub, ..
                } => {
                    decoded_string.push_str(&format!("{:0>8x} {}\n", idx, item));
                    let repr = BytecodeDecompiler::disassemble_function(sub);
                    decoded_string.push_str("Subroutine Start:\n");
                    decoded_string.push_str(&repr);
                    decoded_string.push_str("Subroutine End\n");
                    idx = idx + 1;
                }
                _ => {
                    let repr = item.to_string();
                    decoded_string.push_str(&format!("{:0>8x} {}\n", idx, repr));
                    idx = idx + 1;
                }
            }
        }
        decoded_string.push('\n');

        return decoded_string;
    }

    pub fn disassemble(bytecode: &CompiledBytecode) -> String {
        let mut decoded_string = String::new();

        decoded_string.push_str("\nConstants: \n");
        decoded_string.push_str(&BytecodeDecompiler::disassemble_constants(&bytecode));

        decoded_string.push_str("Instructions: \n");
        decoded_string.push_str(&BytecodeDecompiler::disassemble_instructions(&bytecode));

        return decoded_string;
    }
}
