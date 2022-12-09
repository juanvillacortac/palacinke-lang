use pk_compiler::{
    code::{ByteEndianness, Instruction, InstructionPacker, Operands},
    CompiledInstructions,
};

#[derive(Debug, Clone)]
pub struct Frame {
    pub instructions: CompiledInstructions,
    pub ip: usize,
    pub bp: i64,
}

impl Frame {
    pub fn new(instructions: CompiledInstructions, bp: i64) -> Self {
        Self {
            instructions,
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

    pub fn forward_ip(&mut self, op: &Instruction, offset: usize) {
        match op {
            Instruction::SetGlobal
            | Instruction::GetGlobal
            | Instruction::SetLocal
            | Instruction::GetLocal
            | Instruction::Jump
            | Instruction::JumpNot
            | Instruction::Array
            | Instruction::Hash => {
                self.ip += 1;
            }
            Instruction::Call => {
                self.ip = 0;
            }
            _ => {
                self.ip += offset + 1;
            }
        }
    }

    pub fn read_current_instruction(&self) -> (Instruction, Operands, usize) {
        let decoded_opcode: Instruction = Instruction::from_u8(self.instructions[self.ip]);

        let (operands, next_offset) = InstructionPacker(ByteEndianness::Big)
            .decode_instruction(&decoded_opcode, &self.instructions[self.ip + 1..]);

        return (decoded_opcode, operands, next_offset);
    }

    pub fn has_instructions(&self) -> bool {
        self.ip < self.instructions.len()
    }
}
