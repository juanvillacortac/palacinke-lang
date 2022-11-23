pub mod errors;

use errors::*;
use pk_compiler::{
    code::{ByteEndianness, Instruction, InstructionPacker},
    objects::*,
    CompiledBytecode, CompiledInstructions, ConstantsPool,
};

const STACK_SIZE: usize = 2048;

pub struct VM {
    constants: ConstantsPool,
    instructions: CompiledInstructions,

    stack: Vec<Option<Object>>,
    sp: usize,

    packer: InstructionPacker,
}

impl VM {
    pub fn new(bytecode: CompiledBytecode) -> Self {
        Self {
            instructions: bytecode.instructions,
            constants: bytecode.constants,
            stack: vec![None; STACK_SIZE],
            sp: 0,
            packer: InstructionPacker(ByteEndianness::Big),
        }
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

    pub fn run(&mut self) -> Option<VMError> {
        let mut ip: usize = 0;
        while ip < self.instructions.len() {
            let op: Instruction = unsafe { ::std::mem::transmute(self.instructions[ip]) };

            let (operands, next_offset) = self
                .packer
                .decode_instruction(&op, &self.instructions[ip + 1..]);

            match op {
                Instruction::Const => {
                    let err = self.push(self.constants[operands[0]].clone());
                    if err.is_some() {
                        return err;
                    }
                }
                Instruction::Add
                | Instruction::Sub
                | Instruction::Mul
                | Instruction::Div
                | Instruction::Pow
                | Instruction::Mod
                | Instruction::Eq
                | Instruction::Neq
                | Instruction::Lt
                | Instruction::Gt
                | Instruction::Lte
                | Instruction::Gte
                | Instruction::And
                | Instruction::Or => {
                    let err = self.exec_binary_op(&op);
                    if err.is_some() {
                        return err;
                    }
                }
                Instruction::True => {
                    self.push(Object::Boolean(true));
                }
                Instruction::False => {
                    self.push(Object::Boolean(false));
                }
                Instruction::Not => {
                    let obj = self.pop();
                    if obj.is_some() {
                        match obj.unwrap() {
                            Object::Boolean(val) => {
                                self.push(Object::Boolean(!val));
                            }
                            _ => {
                                self.push(Object::Boolean(false));
                            }
                        }
                    }
                }
                Instruction::Negative => {
                    let obj = self.pop();
                    if obj.is_some() {
                        match obj.unwrap() {
                            Object::Number(Float(val)) => {
                                self.push(Object::Number(Float(-val)));
                            }
                            obj => {
                                return Some(VMError::new(
                                    VMErrorKind::IllegalOperation,
                                    format!(
                                        "\"{}\" is a unsuported type for negation",
                                        obj.type_str()
                                    ),
                                ))
                            }
                        }
                    }
                }
                Instruction::Pop => {
                    self.pop();
                }
                _ => {}
            }

            ip += next_offset + 1;
        }
        None
    }

    fn push(&mut self, obj: Object) -> Option<VMError> {
        if self.sp >= STACK_SIZE {
            return Some(VMError::new(
                VMErrorKind::RuntimeError,
                "stack overflow".to_string(),
            ));
        }
        self.stack[self.sp] = Some(obj);
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

    fn exec_binary_op(&mut self, op: &Instruction) -> Option<VMError> {
        let right = self.pop();
        let left = self.pop();
        if let (Some(left), Some(right)) = (left, right) {
            use Object::*;
            match op {
                Instruction::Eq => {
                    self.push(Object::Boolean(left == right));
                }
                Instruction::Neq => {
                    self.push(Object::Boolean(left != right));
                }
                _ => match (&left, &right) {
                    (Number(_), Number(_)) => self.exec_binary_numeric_op(&op, &left, &right),
                    (Boolean(left), Boolean(right)) => match op {
                        Instruction::And => {
                            self.push(Object::Boolean(*left && *right));
                        }
                        Instruction::Or => {
                            self.push(Object::Boolean(*left || *right));
                        }
                        _ => {}
                    },
                    _ => {
                        return Some(VMError::new(
                            VMErrorKind::IllegalOperation,
                            format!("you can't use the operator \"{}\" with the types \"{}\" and \"{}\"", op.as_string(), left.type_str(), right.type_str()),
                        ));
                    }
                },
            }
        };
        None
    }

    fn exec_binary_numeric_op(&mut self, op: &Instruction, left: &Object, right: &Object) {
        use Object::Number;
        let (left, right) = match (left, right) {
            (Number(Float(left)), Number(Float(right))) => (left, right),
            (..) => return,
        };
        match op {
            Instruction::Add => {
                self.push(Object::Number(Float(left + right)));
            }
            Instruction::Sub => {
                self.push(Object::Number(Float(left - right)));
            }
            Instruction::Div => {
                self.push(Object::Number(Float(left / right)));
            }
            Instruction::Mul => {
                self.push(Object::Number(Float(left * right)));
            }
            Instruction::Pow => {
                self.push(Object::Number(Float(left.powf(*right))));
            }
            Instruction::Mod => {
                self.push(Object::Number(Float(left % right)));
            }
            Instruction::Lt => {
                self.push(Object::Boolean(left < right));
            }
            Instruction::Gt => {
                self.push(Object::Boolean(left > right));
            }
            Instruction::Lte => {
                self.push(Object::Boolean(left <= right));
            }
            Instruction::Gte => {
                self.push(Object::Boolean(left >= right));
            }
            _ => {}
        }
    }
}
