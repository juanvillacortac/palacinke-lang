use bytecoding::{Bytecode, DecodeError};

pub type CompiledInstructions = Vec<u8>;

#[derive(Eq, PartialEq, Debug, Clone, Bytecode)]
#[bytecode(type = u8)]
pub enum Instruction {
    Illegal,

    Call(u16),
    Closure(u16, u16),
    CurrentClosure,

    Return,
    ReturnValue,

    Const(u16),
    Pop,

    SetGlobal(u16),
    GetGlobal(u16),
    SetLocal(u16),
    GetLocal(u16),
    GetFree(u16),

    Array(u16),
    Hash(u16),
    Index,

    Jump(u16),
    JumpNot(u16),

    Add,
    Mul,
    Sub,
    Div,
    Mod,
    Pow,

    Bool(#[bytecode(flatten_all = [true, false])] bool),

    Nil,

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
}

impl Instruction {
    #[allow(dead_code)]
    pub fn as_string(&self) -> String {
        match self {
            Self::Const(op) => format!("OpConst {op}"),
            Self::Pop => "OpPop".to_string(),
            Self::Call(op) => format!("OpCall {op}"),
            Self::Closure(op1, op2) => format!("OpClosure {op1} {op2}"),
            Self::CurrentClosure => "OpCurrentClosure".to_string(),
            Self::Return => "OpReturn".to_string(),
            Self::ReturnValue => "OpReturnValue".to_string(),
            Self::Add => "OpAdd".to_string(),
            Self::Sub => "OpSub".to_string(),
            Self::Mul => "OpMul".to_string(),
            Self::Div => "OpDiv".to_string(),
            Self::Pow => "OpPow".to_string(),
            Self::Mod => "OpMod".to_string(),
            Self::Bool(op) => format!("OpBool {op}"),
            Self::Gt => "OpGt".to_string(),
            Self::Lt => "OpLt".to_string(),
            Self::Eq => "OpEq".to_string(),
            Self::Neq => "OpNeq".to_string(),
            Self::Lte => "OpLte".to_string(),
            Self::Gte => "OpGte".to_string(),
            Self::And => "OpAnd".to_string(),
            Self::Or => "OpOr".to_string(),
            Self::Negative => "OpNegative".to_string(),
            Self::Not => "OpNot".to_string(),
            Self::Jump(op) => format!("OpJump {op}"),
            Self::JumpNot(op) => format!("OpJumpNot {op}"),
            Self::Nil => "OpNil".to_string(),
            Self::SetGlobal(op) => format!("OpSetGlobal {op}"),
            Self::GetGlobal(op) => format!("OpGetGlobal {op}"),
            Self::SetLocal(op) => format!("OpSetLocal {op}"),
            Self::GetLocal(op) => format!("OpGetLocal {op}"),
            Self::GetFree(op) => format!("OpGetFree {op}"),
            Self::Array(op) => format!("OpArr {op}"),
            Self::Hash(op) => format!("OpHash {op}"),
            Self::Index => "OpIndex".to_string(),
            Self::Illegal => "ILLEGAL_OP".to_string(),
        }
    }

    pub fn compile(&self) -> CompiledInstructions {
        let mut buf = Vec::new();
        self.encode(&mut buf);
        buf
    }

    pub fn compile_instructions(instructions: Vec<Instruction>) -> Vec<u8> {
        let mut encoded = Vec::new();
        for ins in instructions {
            ins.encode(&mut encoded);
        }
        encoded
    }

    pub fn decompile_instructions(
        instructions: &CompiledInstructions,
    ) -> Result<Vec<Instruction>, DecodeError> {
        let mut buf: &[u8] = &instructions;
        let mut ins = Vec::new();
        while !buf.is_empty() {
            ins.push(Instruction::decode(&mut buf)?);
        }
        Ok(ins)
    }
}
