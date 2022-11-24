use std::ops::Deref;

use pk_compiler::{
    code::{Instruction, Operands},
    objects::{Float, Object},
};

use crate::{
    errors::{VMError, VMErrorKind},
    VM,
};

pub trait VMExecutor {
    fn exec_instruction(&mut self, op: &Instruction, operands: &Operands) -> Option<VMError>;
    fn exec_const(&mut self, operands: &Operands) -> Option<VMError>;
    fn exec_set_global(&mut self, operands: &Operands) -> Option<VMError>;
    fn exec_get_global(&mut self, operands: &Operands) -> Option<VMError>;
    fn exec_binary_op(&mut self, op: &Instruction) -> Option<VMError>;
    fn exec_binary_numeric_op(&mut self, op: &Instruction, left: &Object, right: &Object);
    fn exec_booleans_objects(&mut self, op: &Instruction) -> Option<VMError>;
    fn exec_pop(&mut self) -> Option<VMError>;
    fn exec_jumps(&mut self, op: &Instruction, operands: &Operands) -> Option<VMError>;
    fn exec_not(&mut self) -> Option<VMError>;
    fn exec_negative(&mut self) -> Option<VMError>;
}

impl VMExecutor for VM {
    fn exec_instruction(&mut self, op: &Instruction, operands: &Operands) -> Option<VMError> {
        match op {
            Instruction::Const => self.exec_const(&operands),
            Instruction::SetGlobal => self.exec_set_global(&operands),
            Instruction::GetGlobal => self.exec_get_global(&operands),
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
            | Instruction::Or => self.exec_binary_op(&op),
            Instruction::True | Instruction::False | Instruction::Nil => {
                self.exec_booleans_objects(&op)
            }
            Instruction::Not => self.exec_not(),
            Instruction::Negative => self.exec_negative(),
            Instruction::Pop => self.exec_pop(),
            Instruction::Jump | Instruction::JumpNot => self.exec_jumps(&op, &operands),
        }
    }
    fn exec_const(&mut self, operands: &Operands) -> Option<VMError> {
        self.push(&self.constants[operands[0]].clone())
    }
    fn exec_set_global(&mut self, operands: &Operands) -> Option<VMError> {
        self.ip += 3;
        self.globals.deref().borrow_mut()[operands[0]] = self.pop();
        None
    }
    fn exec_get_global(&mut self, operands: &Operands) -> Option<VMError> {
        self.ip += 3;
        match (*self.globals).to_owned().borrow().get(operands[0]) {
            Some(Some(obj)) => {
                if let Some(err) = self.push(&obj) {
                    return Some(err.clone());
                }
            }
            _ => {
                self.push(&Object::Nil);
            }
        }
        None
    }
    fn exec_binary_op(&mut self, op: &Instruction) -> Option<VMError> {
        let right = self.pop();
        let left = self.pop();
        if let (Some(left), Some(right)) = (left, right) {
            use Object::*;
            match op {
                Instruction::Eq => {
                    self.push(&Object::Boolean(left == right));
                }
                Instruction::Neq => {
                    self.push(&Object::Boolean(left != right));
                }
                _ => match (&left, &right) {
                    (Number(_), Number(_)) => self.exec_binary_numeric_op(&op, &left, &right),
                    (Boolean(left), Boolean(right)) => match op {
                        Instruction::And => {
                            self.push(&Object::Boolean(*left && *right));
                        }
                        Instruction::Or => {
                            self.push(&Object::Boolean(*left || *right));
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
                self.push(&Object::Number(Float(left + right)));
            }
            Instruction::Sub => {
                self.push(&Object::Number(Float(left - right)));
            }
            Instruction::Div => {
                self.push(&Object::Number(Float(left / right)));
            }
            Instruction::Mul => {
                self.push(&Object::Number(Float(left * right)));
            }
            Instruction::Pow => {
                self.push(&Object::Number(Float(left.powf(*right))));
            }
            Instruction::Mod => {
                self.push(&Object::Number(Float(left % right)));
            }
            Instruction::Lt => {
                self.push(&Object::Boolean(left < right));
            }
            Instruction::Gt => {
                self.push(&Object::Boolean(left > right));
            }
            Instruction::Lte => {
                self.push(&Object::Boolean(left <= right));
            }
            Instruction::Gte => {
                self.push(&Object::Boolean(left >= right));
            }
            _ => {}
        }
    }
    fn exec_booleans_objects(&mut self, op: &Instruction) -> Option<VMError> {
        match op {
            Instruction::True => {
                self.push(&Object::Boolean(true));
            }
            Instruction::False => {
                self.push(&Object::Boolean(false));
            }
            Instruction::Nil => {
                self.push(&Object::Nil);
            }
            _ => {}
        };
        None
    }
    fn exec_pop(&mut self) -> Option<VMError> {
        self.pop();
        None
    }
    fn exec_jumps(&mut self, op: &Instruction, operands: &Operands) -> Option<VMError> {
        match op {
            Instruction::Jump => {
                self.ip = operands[0] - 1;
            }
            Instruction::JumpNot => {
                self.ip += 2;
                let obj = self.pop();
                match obj {
                    Some(Object::Boolean(false) | Object::Nil) => {
                        self.ip = operands[0];
                    }
                    _ => {}
                }
            }
            _ => {}
        };
        None
    }
    fn exec_not(&mut self) -> Option<VMError> {
        let obj = self.pop();
        if obj.is_some() {
            match obj.unwrap() {
                Object::Boolean(val) => {
                    self.push(&Object::Boolean(!val));
                }
                Object::Nil => {
                    self.push(&Object::Boolean(true));
                }
                _ => {
                    self.push(&Object::Boolean(false));
                }
            }
        };
        None
    }
    fn exec_negative(&mut self) -> Option<VMError> {
        let obj = self.pop();
        if obj.is_some() {
            match obj.unwrap() {
                Object::Number(Float(val)) => {
                    self.push(&Object::Number(Float(-val)));
                }
                obj => {
                    return Some(VMError::new(
                        VMErrorKind::IllegalOperation,
                        format!("\"{}\" is a unsuported type for negation", obj.type_str()),
                    ))
                }
            }
        };
        None
    }
}
