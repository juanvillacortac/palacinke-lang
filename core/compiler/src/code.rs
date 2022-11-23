pub const BYTE_ENDIAN: &'static str = "big";
pub enum ByteEndianness {
    Big,
    Little,
}

#[repr(u8)]
#[derive(Eq, PartialEq, Debug, Clone)]
pub enum Instruction {
    Const,
    Pop,

    Add,
    Mul,
    Sub,
    Div,
    Mod,
    Pow,

    True,
    False,

    Lt,
    Gt,
    Eq,
    Neq,
    Lte,
    Gte,
    And,
    Or,

    Negative,
    Not,

    Jump,
    JumpNot,
}

impl Instruction {
    pub fn from_u8(byte: u8) -> Self {
        unsafe { ::std::mem::transmute(byte) }
    }
}

pub type Operand = usize;
pub type Operands = Vec<usize>;

pub struct InstructionPacker(pub ByteEndianness);

impl InstructionPacker {
    fn unpack_16(&self, operand: u16) -> (u8, u8) {
        let x1 = ((operand >> 8) & 0x00FF) as u8;
        let x2 = (operand & 0x00FF) as u8;

        let InstructionPacker(endian) = self;
        match endian {
            ByteEndianness::Big => (x1, x2),
            ByteEndianness::Little => (x2, x1),
        }
    }

    fn pack_u16(&self, x1: u8, x2: u8) -> u16 {
        let InstructionPacker(endian) = self;
        match endian {
            ByteEndianness::Big => (((x1 as u16) & 0x00FF) << 8) | ((x2 as u16) & 0x00FF),
            ByteEndianness::Little => (((x2 as u16) & 0x00FF) << 8) | ((x1 as u16) & 0x00FF),
        }
    }

    pub fn encode_instruction(&self, instruction: Instruction, operands: &Operands) -> Vec<u8> {
        let operand_sizes = instruction.get_encoding_width();
        let mut statement = Vec::new();

        statement.push(instruction as u8);
        for idx in 0..operands.len() {
            let operand = operands[idx];
            let width = operand_sizes[idx];

            if width == 2 {
                let (x1, x2) = self.unpack_16(operand as u16);
                statement.push(x1);
                statement.push(x2);
            } else {
                statement.push(operand as u8);
            }
        }

        return statement;
    }

    pub fn decode_instruction(
        &self,
        instruction: &Instruction,
        packed_ops: &[u8],
    ) -> (Vec<usize>, usize) {
        let operand_widths = instruction.get_encoding_width();
        let mut unpacked_stmt = vec![];
        let mut offset = 0;

        for width in operand_widths {
            if width == 2 {
                let operand_bytes = &packed_ops[offset..offset + 2];
                let packed_value = self.pack_u16(operand_bytes[0], operand_bytes[1]);
                unpacked_stmt.push(packed_value as usize);
                offset += 2;
            } else {
                unpacked_stmt.push(packed_ops[offset] as usize);
                offset += 1;
            }
        }

        return (unpacked_stmt, offset);
    }
}

impl Instruction {
    #[allow(dead_code)]
    pub fn as_string(&self) -> String {
        match self {
            Self::Const => "CONST".to_string(),
            Self::Pop => "POP".to_string(),
            Self::Add => "ADD".to_string(),
            Self::Sub => "SUB".to_string(),
            Self::Mul => "MUL".to_string(),
            Self::Div => "DIV".to_string(),
            Self::Pow => "POW".to_string(),
            Self::Mod => "MOD".to_string(),
            Self::True => "TRUE".to_string(),
            Self::False => "FALSE".to_string(),
            Self::Gt => "GT".to_string(),
            Self::Lt => "LT".to_string(),
            Self::Eq => "EQ".to_string(),
            Self::Neq => "NEQ".to_string(),
            Self::Lte => "LTE".to_string(),
            Self::Gte => "GTE".to_string(),
            Self::And => "AND".to_string(),
            Self::Or => "OR".to_string(),
            Self::Negative => "NEGATIVE".to_string(),
            Self::Not => "NOT".to_string(),
            Self::Jump => "JUMP".to_string(),
            Self::JumpNot => "JUMP_NOT".to_string(),
        }
    }

    #[allow(dead_code)]
    pub fn get_encoding_width(&self) -> Vec<u8> {
        match self {
            Self::Const | Self::Jump | Self::JumpNot => vec![2],
            Self::Pop
            | Self::Add
            | Self::Sub
            | Self::Mul
            | Self::Div
            | Self::Mod
            | Self::Pow
            | Self::True
            | Self::False
            | Self::Lt
            | Self::Gt
            | Self::Lte
            | Self::Gte
            | Self::Eq
            | Self::Neq
            | Self::And
            | Self::Or
            | Self::Not
            | Self::Negative => vec![],
        }
    }

    #[allow(dead_code)]
    pub fn disasm_instruction(&self, operands: &Operands) -> String {
        let op_strings: Vec<String> = operands.into_iter().map(|op| format!("{:x}", op)).collect();
        let op_formatted = op_strings.join(", ");
        let opcode = self.as_string();

        return format!("{} {}", opcode, op_formatted);
    }
}
