pub mod errors;
mod executors;

use std::{cell::RefCell, rc::Rc};

use errors::*;
use executors::VMExecutor;
use pk_compiler::{
    code::{ByteEndianness, Instruction, InstructionPacker},
    objects::*,
    CompiledBytecode, CompiledInstructions, ConstantsPool,
};

const STACK_SIZE: usize = 2048;
const GLOBALS_SIZE: usize = 65536;

pub type GlobalsStore = Vec<Option<Object>>;
pub type Stack = Vec<Option<Object>>;

pub fn new_globals_store() -> GlobalsStore {
    vec![None; GLOBALS_SIZE]
}

pub struct VM {
    constants: ConstantsPool,
    instructions: CompiledInstructions,

    globals: Rc<RefCell<GlobalsStore>>,
    stack: Stack,
    sp: usize,
    ip: usize,

    packer: InstructionPacker,
}

impl VM {
    pub fn new(bytecode: CompiledBytecode) -> Self {
        Self {
            instructions: bytecode.instructions,
            constants: bytecode.constants,
            stack: vec![None; STACK_SIZE],
            globals: Rc::new(RefCell::new(new_globals_store())),
            sp: 0,
            ip: 0,
            packer: InstructionPacker(ByteEndianness::Big),
        }
    }

    pub fn new_with_globals(
        bytecode: CompiledBytecode,
        globals: Rc<RefCell<GlobalsStore>>,
    ) -> Self {
        let mut vm = Self::new(bytecode);
        vm.globals = Rc::clone(&globals);
        vm
    }

    pub fn last_popped(&self) -> Option<Object> {
        self.stack.get(self.sp).unwrap_or(&None).clone()
    }

    pub fn stack_top(&self) -> Option<Object> {
        if self.sp == 0 {
            return None;
        }
        self.stack.get(self.sp - 1).unwrap_or(&None).clone()
    }

    pub fn forward_ip(&mut self, op: &Instruction, offset: usize) {
        match op {
            Instruction::SetGlobal
            | Instruction::GetGlobal
            | Instruction::Jump
            | Instruction::JumpNot => {}
            _ => {
                self.ip += offset + 1;
            }
        }
    }

    pub fn run(&mut self) -> Option<VMError> {
        self.ip = 0;
        while self.ip < self.instructions.len() {
            let op: Instruction = Instruction::from_u8(self.instructions[self.ip]);

            let (operands, next_offset) = self
                .packer
                .decode_instruction(&op, &self.instructions[self.ip + 1..]);

            match self.exec_instruction(&op, &operands) {
                Some(err) => return Some(err),
                _ => {}
            };
            self.forward_ip(&op, next_offset);
        }
        None
    }

    fn push(&mut self, obj: &Object) -> Option<VMError> {
        if self.sp >= STACK_SIZE {
            return Some(VMError::new(
                VMErrorKind::RuntimeError,
                "stack overflow".to_string(),
            ));
        }
        self.stack[self.sp] = Some(obj.clone());
        self.sp += 1;
        None
    }

    fn pop(&mut self) -> Option<Object> {
        if self.sp == 0 {
            return None;
        }
        let obj = self.stack.get(self.sp - 1);
        self.sp -= 1;
        obj.unwrap_or(&None).clone()
    }
}
