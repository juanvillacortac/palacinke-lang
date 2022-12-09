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

pub type CompiledInstructions = Vec<u8>;

#[derive(Debug, Clone)]
pub struct EmitedInstruction(Instruction, usize);

#[derive(Debug, Clone)]
pub struct CompiledBytecode {
    pub instructions: CompiledInstructions,
    pub constants: ConstantsPool,
}

#[derive(Debug, Clone)]
pub struct CompilationScope {
    instructions: CompiledInstructions,
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
            instructions: self.scopes[self.scope_idx].instructions.clone(),
            constants: self.constants.clone(),
        };
        println!("{}", BytecodeDecompiler::disassemble(&bytecode));
        bytecode
    }

    fn emit(&mut self, instruction: Instruction, operands: &Operands) -> usize {
        let packer = InstructionPacker(ByteEndianness::Big);
        let encoded = packer.encode_instruction(instruction.clone(), operands);

        let pos = self.add_instructions(&encoded);

        let previous_instruction = self.scopes[self.scope_idx].last_instruction.clone();
        let last_instruction = Some(EmitedInstruction(instruction, pos));

        self.scopes[self.scope_idx].previous_instruction = previous_instruction;
        self.scopes[self.scope_idx].last_instruction = last_instruction;

        pos
    }

    fn add_instructions(&mut self, instructions: &CompiledInstructions) -> usize {
        let new_pos = self.scopes[self.scope_idx].instructions.len();
        self.scopes[self.scope_idx]
            .instructions
            .append(&mut instructions.clone());
        new_pos
    }

    fn add_constant(&mut self, obj: Rc<Object>) -> usize {
        self.constants.set_object(obj)
    }

    fn replace_instructions(&mut self, pos: usize, new_instructions: CompiledInstructions) {
        for (idx, byte) in new_instructions.iter().enumerate() {
            self.scopes[self.scope_idx].instructions[pos + idx] = *byte;
        }
    }

    fn replace_operands(&mut self, inst_pos: usize, operands: &Operands) {
        let packer = InstructionPacker(ByteEndianness::Big);
        match self.scopes[self.scope_idx].instructions.get(inst_pos) {
            Some(byte) => {
                let instruction = Instruction::from_u8(*byte);
                let encoded = packer.encode_instruction(instruction, operands);

                self.replace_instructions(inst_pos, encoded);
            }
            _ => {}
        }
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

    fn leave_scope(&mut self) -> CompiledInstructions {
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
                self.emit(
                    match symbol.scope {
                        SymbolScope::Global => Instruction::SetGlobal,
                        SymbolScope::Local => Instruction::SetLocal,
                    },
                    &vec![symbol.index],
                );
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
                let err = self.compile_expression(expr);
                if err.is_some() {
                    return err;
                }
                let symbol = self.symbols_table.define(ident);
                self.emit(
                    match symbol.scope {
                        SymbolScope::Global => Instruction::SetGlobal,
                        SymbolScope::Local => Instruction::SetLocal,
                    },
                    &vec![symbol.index],
                );
                None
            }
            Statement::Return(expr) => {
                let err = self.compile_expression(expr);
                if err.is_none() {
                    self.emit(Instruction::ReturnValue, &vec![]);
                }
                err
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
            Expression::Function { params: _, body } => {
                self.enter_scope();
                if let Some(err) = self.compile_block(body) {
                    return Some(err);
                }
                if self.last_instruction_is(&Instruction::Pop) {
                    self.replace_last_pop_with_return();
                }
                if !self.last_instruction_is(&Instruction::ReturnValue) {
                    self.emit(Instruction::Return, &vec![]);
                }
                let locals = self.symbols_table.num_defs;
                let instructions = self.leave_scope();
                let compiled_fn = Object::CompiledFunction {
                    instructions,
                    locals,
                };
                let operands = vec![self.add_constant(Rc::new(compiled_fn))];
                self.emit(Instruction::Const, &operands);
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
                let operands = vec![args.len()];
                self.emit(Instruction::Call, &operands);
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
                self.emit(
                    match symbol.clone().unwrap().scope {
                        SymbolScope::Global => Instruction::GetGlobal,
                        SymbolScope::Local => Instruction::GetLocal,
                    },
                    &vec![symbol.unwrap().index],
                );
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
                self.emit(Instruction::Index, &vec![]);
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
        let packer = InstructionPacker(ByteEndianness::Big);
        match self.scopes[self.scope_idx].last_instruction {
            Some(EmitedInstruction(Instruction::Pop, pos)) => {
                let encoded = packer.encode_instruction(Instruction::ReturnValue, &vec![]);

                self.replace_instructions(pos, encoded);

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

        let jump_not_pos = self.emit(Instruction::JumpNot, &vec![9999 as usize]);

        match self.compile_block(&consequence) {
            Some(err) => return Some(err),
            _ => {}
        }

        self.remove_last_pop();

        let jump_pos = self.emit(Instruction::Jump, &vec![9999 as usize]);

        let after_cons_pos = self.scopes[self.scope_idx].instructions.len();
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

        let after_alt_pos = self.scopes[self.scope_idx].instructions.len();
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
                    Prefix::Dollar => {
                        self.emit(Instruction::ShellCommand, &vec![]);
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
                let operands = vec![self.add_constant(Rc::new(number))];
                self.emit(Instruction::Const, &operands);
                None
            }
            Literal::String(val) => {
                let string = Object::String(val.to_string());
                let operands = vec![self.add_constant(Rc::new(string))];
                self.emit(Instruction::Const, &operands);
                None
            }
            Literal::Array(exprs) => {
                for expr in exprs {
                    if let Some(err) = self.compile_expression(expr) {
                        return Some(err);
                    }
                }
                self.emit(Instruction::Array, &vec![exprs.len()]);
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
                self.emit(Instruction::Hash, &vec![objs.len() * 2]);
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
            #[allow(unreachable_patterns)]
            _ => None,
        }
    }
}

pub struct BytecodeDecompiler;

impl BytecodeDecompiler {
    pub fn disassemble_function(instructions: &CompiledInstructions) -> String {
        let length = instructions.len();

        let mut decoded_string = String::new();
        let mut idx = 0;

        while idx < length {
            let op = instructions[idx];
            let op_kind: Instruction = Instruction::from_u8(op);
            let (operands, next_offset) = InstructionPacker(ByteEndianness::Big)
                .decode_instruction(&op_kind, &instructions[idx + 1..]);
            decoded_string.push_str(&format!("{:0>8x} ", idx));
            decoded_string.push_str(&op_kind.disasm_instruction(&operands));
            decoded_string.push('\n');

            idx = idx + next_offset + 1;
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
