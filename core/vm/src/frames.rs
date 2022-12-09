use pk_compiler::code::{CompiledInstructions, Instruction};

#[derive(Debug, Clone)]
pub struct Frame {
    pub instructions: Vec<Instruction>,
    pub ip: usize,
    pub bp: i64,
}

impl Frame {
    pub fn new(instructions: CompiledInstructions, bp: i64) -> Self {
        Self {
            instructions: Instruction::decompile_instructions(&instructions).unwrap(),
            ip: 0,
            bp,
        }
    }

    pub fn set_ip<F>(&mut self, cb: F)
    where
        F: FnOnce(usize) -> usize,
    {
        self.ip = cb(self.ip);
    }

    pub fn forward_ip(&mut self, op: &Instruction) {
        match op {
            Instruction::Call(_) => {
                self.ip = 0;
            }
            _ => {
                self.ip += 1;
            }
        }
    }

    pub fn read_current_instruction(&self) -> Instruction {
        self.instructions[self.ip].clone()
    }

    pub fn has_instructions(&self) -> bool {
        self.ip < self.instructions.len()
    }
}
