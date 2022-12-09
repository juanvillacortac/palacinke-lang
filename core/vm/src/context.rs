use std::{cell::RefCell, collections::HashMap, ops::Range, process::Command, rc::Rc};

use pk_compiler::{
    code::{Instruction, Operands},
    objects::{Float, Object},
    symbols_table::ConstantsPool,
};

use crate::{
    errors::{VMError, VMErrorKind},
    frames::Frame,
    stack::{CallStack, DataStack},
    GlobalsStore,
};

pub struct VMContext<'a> {
    pub globals: &'a mut GlobalsStore,
    pub data_stack: &'a mut DataStack,
    pub call_stack: &'a mut CallStack,
    pub constants: &'a mut ConstantsPool,
}

impl<'a> VMContext<'a> {
    pub fn exec_instruction(&mut self, op: &Instruction, operands: &Operands) -> Option<VMError> {
        match op {
            Instruction::Const => self.exec_const(&operands),
            Instruction::SetGlobal => self.exec_set_global(&operands),
            Instruction::GetGlobal => self.exec_get_global(&operands),
            Instruction::SetLocal => self.exec_set_local(&operands),
            Instruction::GetLocal => self.exec_get_local(&operands),
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
            Instruction::ShellCommand => self.exec_shell_command(),
            Instruction::Pop => self.exec_pop(),
            Instruction::Jump | Instruction::JumpNot => self.exec_jumps(&op, &operands),
            Instruction::Array => self.exec_array(&operands),
            Instruction::Hash => self.exec_hash(&operands),
            Instruction::Index => self.exec_index(),
            Instruction::Call => self.exec_call(&operands),
            Instruction::Return | Instruction::ReturnValue => self.exec_returns(&op),
            #[allow(unreachable_patterns)]
            _ => {
                return Some(VMError::new(
                    VMErrorKind::IllegalOperation,
                    format!("instruction \"{}\" not implemented", op.as_string(),),
                ));
            }
        }
    }
    fn exec_const(&mut self, operands: &Operands) -> Option<VMError> {
        let obj = self.constants.get_object(operands[0]);
        self.data_stack.push_object(obj.unwrap())
    }
    fn exec_set_global(&mut self, operands: &Operands) -> Option<VMError> {
        self.call_stack.top().ip += 2;
        self.globals[operands[0]] = Some(self.data_stack.pop_object().unwrap().as_ref().clone());
        None
    }
    fn exec_get_global(&mut self, operands: &Operands) -> Option<VMError> {
        self.call_stack.top().ip += 2;
        match &self.globals[operands[0]] {
            Some(obj) => {
                if let Some(err) = self.data_stack.push_object(Rc::new(obj.clone())) {
                    return Some(err.clone());
                }
            }
            _ => {
                self.data_stack.push_object(Rc::new(Object::Nil));
            }
        }
        None
    }
    fn exec_set_local(&mut self, operands: &Operands) -> Option<VMError> {
        self.call_stack.top().ip += 2;
        let frame = self.call_stack.top_ref().clone();
        self.data_stack.stack[(frame.bp - 1) as usize + operands[0]] =
            self.data_stack.pop_object().unwrap();
        None
    }
    fn exec_get_local(&mut self, operands: &Operands) -> Option<VMError> {
        self.call_stack.top().ip += 2;
        let frame = self.call_stack.top_ref().clone();
        match self
            .data_stack
            .stack
            .get((frame.bp - 1) as usize + operands[0])
            .cloned()
        {
            Some(obj) => {
                if let Some(err) = self.data_stack.push_object(obj) {
                    return Some(err.clone());
                }
            }
            _ => {
                self.data_stack.push_object(Rc::new(Object::Nil));
            }
        }
        None
    }
    fn exec_binary_op(&mut self, op: &Instruction) -> Option<VMError> {
        let right = self.data_stack.pop_object();
        let left = self.data_stack.pop_object();
        if let (Some(left), Some(right)) = (left, right) {
            use Object::*;
            return match op {
                Instruction::Eq => self
                    .data_stack
                    .push_object(Rc::new(Object::Boolean(left == right))),
                Instruction::Neq => self
                    .data_stack
                    .push_object(Rc::new(Object::Boolean(*left != *right))),
                _ => match (left.as_ref().clone(), right.as_ref().clone(), op) {
                    (Number(_), Number(_), _) => {
                        self.exec_binary_numeric_op(&op, &left, &right);
                        None
                    }
                    (String(left), String(right), _) => self
                        .data_stack
                        .push_object(Rc::new(Object::String(format!("{}{}", left, right)))),
                    (left, right, Instruction::Add) => self
                        .data_stack
                        .push_object(Rc::new(Object::String(format!("{}{}", left, right)))),
                    (Boolean(left), Boolean(right), _) => match op {
                        Instruction::And => self
                            .data_stack
                            .push_object(Rc::new(Object::Boolean(left && right))),
                        Instruction::Or => self
                            .data_stack
                            .push_object(Rc::new(Object::Boolean(left || right))),
                        _ => None,
                    },
                    _ => {
                        return Some(VMError::new(
                            VMErrorKind::IllegalOperation,
                            format!("you can't use the operator \"{}\" with the types \"{}\" and \"{}\"", op.as_string(), left.type_str(), right.type_str()),
                        ));
                    }
                },
            };
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
                self.data_stack
                    .push_object(Rc::new(Object::Number(Float(left + right))));
            }
            Instruction::Sub => {
                self.data_stack
                    .push_object(Rc::new(Object::Number(Float(left - right))));
            }
            Instruction::Div => {
                self.data_stack
                    .push_object(Rc::new(Object::Number(Float(left / right))));
            }
            Instruction::Mul => {
                self.data_stack
                    .push_object(Rc::new(Object::Number(Float(left * right))));
            }
            Instruction::Pow => {
                self.data_stack
                    .push_object(Rc::new(Object::Number(Float(left.powf(*right)))));
            }
            Instruction::Mod => {
                self.data_stack
                    .push_object(Rc::new(Object::Number(Float(left % right))));
            }
            Instruction::Lt => {
                self.data_stack
                    .push_object(Rc::new(Object::Boolean(left < right)));
            }
            Instruction::Gt => {
                self.data_stack
                    .push_object(Rc::new(Object::Boolean(left > right)));
            }
            Instruction::Lte => {
                self.data_stack
                    .push_object(Rc::new(Object::Boolean(left <= right)));
            }
            Instruction::Gte => {
                self.data_stack
                    .push_object(Rc::new(Object::Boolean(left >= right)));
            }
            _ => {}
        }
    }
    fn exec_booleans_objects(&mut self, op: &Instruction) -> Option<VMError> {
        match op {
            Instruction::True => {
                self.data_stack.push_object(Rc::new(Object::Boolean(true)));
            }
            Instruction::False => {
                self.data_stack.push_object(Rc::new(Object::Boolean(false)));
            }
            Instruction::Nil => {
                self.data_stack.push_object(Rc::new(Object::Nil));
            }
            _ => {}
        };
        None
    }
    #[allow(dead_code)]
    fn exec_pop(&mut self) -> Option<VMError> {
        self.data_stack.pop_object();
        None
    }
    fn exec_jumps(&mut self, op: &Instruction, operands: &Operands) -> Option<VMError> {
        match op {
            Instruction::Jump => {
                self.call_stack.top().ip = operands[0] - 1;
            }
            Instruction::JumpNot => {
                self.call_stack.top().ip += 2;
                let obj = self.data_stack.pop_object();
                match *obj.unwrap() {
                    Object::Boolean(false) | Object::Nil => {
                        self.call_stack.top().ip = operands[0] - 1;
                    }
                    _ => {}
                }
            }
            _ => {}
        };
        None
    }
    fn exec_not(&mut self) -> Option<VMError> {
        let obj = self.data_stack.pop_object();
        if obj.is_some() {
            match *obj.unwrap() {
                Object::Boolean(val) => {
                    self.data_stack.push_object(Rc::new(Object::Boolean(!val)));
                }
                Object::Nil => {
                    self.data_stack.push_object(Rc::new(Object::Boolean(true)));
                }
                _ => {
                    self.data_stack.push_object(Rc::new(Object::Boolean(false)));
                }
            }
        };
        None
    }
    fn exec_negative(&mut self) -> Option<VMError> {
        let obj = self.data_stack.pop_object();
        if obj.is_some() {
            match &*obj.unwrap() {
                Object::Number(Float(val)) => {
                    self.data_stack
                        .push_object(Rc::new(Object::Number(Float(-val))));
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
    fn exec_shell_command(&mut self) -> Option<VMError> {
        let obj = self.data_stack.pop_object();
        if obj.is_some() {
            match &*obj.unwrap() {
                Object::String(cmd) => {
                    let mut hash: HashMap<Object, Object> = HashMap::new();
                    let splited: Vec<&str> = cmd.split_whitespace().collect();
                    let mut cmd = &mut Command::new(splited.get(0).unwrap_or(&""));
                    if splited.len() > 1 {
                        cmd = cmd.args(&splited[1..]);
                    }
                    let output = cmd.output();
                    match output {
                        Ok(output) => {
                            hash.insert(
                                Object::String("stdout".to_string()),
                                Object::String(String::from_utf8_lossy(&output.stdout).to_string()),
                            );
                            hash.insert(
                                Object::String("stderr".to_string()),
                                Object::String(String::from_utf8_lossy(&output.stderr).to_string()),
                            );
                            hash.insert(
                                Object::String("status".to_string()),
                                Object::Number(Float(output.status.code().unwrap_or(0) as f64)),
                            );
                            self.data_stack.push_object(Rc::new(Object::Hash(hash)));
                        }
                        Err(err) => {
                            return Some(VMError::new(VMErrorKind::ShellCommand, err.to_string()))
                        }
                    }
                }
                obj => {
                    return Some(VMError::new(
                        VMErrorKind::IllegalOperation,
                        format!(
                            "cannot exec \"{}\" object type as shell command",
                            obj.type_str()
                        ),
                    ))
                }
            }
        };
        None
    }
    fn exec_array(&mut self, operands: &Operands) -> Option<VMError> {
        self.call_stack.top().ip += 2;
        dbg!(self.data_stack.stack_pointer, &operands[0]);
        let array = self.build_array(Range {
            start: self.data_stack.stack_pointer as usize - operands[0],
            end: self.data_stack.stack_pointer as usize,
        });
        self.data_stack.push_object(array)
    }
    fn exec_hash(&mut self, operands: &Operands) -> Option<VMError> {
        self.call_stack.top().ip += 2;
        let hash = self.build_hash(Range {
            start: self.data_stack.stack_pointer as usize - operands[0],
            end: self.data_stack.stack_pointer as usize,
        });
        // self.call_stack.stack_pointer -= operands[0] as i64;
        self.data_stack.push_object(hash)
    }
    fn exec_index(&mut self) -> Option<VMError> {
        let index = self.data_stack.pop_object().unwrap_or(Rc::new(Object::Nil));
        let left = self.data_stack.pop_object().unwrap_or(Rc::new(Object::Nil));
        match (&*left, &*index) {
            (Object::Array(_), Object::Number(_)) => self.exec_array_index(&left, &index),
            (Object::Hash(_), _) => self.exec_hash_index(&left, &index),
            _ => Some(VMError::new(
                VMErrorKind::IllegalOperation,
                format!(
                    "\"{}\" index type is not supported by \"{}\" object type",
                    index.type_str(),
                    left.type_str()
                ),
            )),
        }
    }
    fn exec_array_index(&mut self, array: &Object, index: &Object) -> Option<VMError> {
        let Object::Array(array) = array else { return self.data_stack.push_object(Rc::new(Object::Nil)) };
        let Object::Number(Float(index)) = index else { return self.data_stack.push_object(Rc::new(Object::Nil)) };
        let index = *index as i64;
        let max = (array.len() - 1) as i64;
        if index < 0 || index > max {
            return self.data_stack.push_object(Rc::new(Object::Nil));
        }
        self.data_stack
            .push_object(Rc::new(array[index as usize].clone()))
    }
    fn exec_hash_index(&mut self, hash: &Object, index: &Object) -> Option<VMError> {
        let Object::Hash(hash) = hash else { return self.data_stack.push_object(Rc::new(Object::Nil)) };
        self.data_stack
            .push_object(Rc::new(hash.get(index).unwrap_or(&Object::Nil).clone()))
    }
    fn exec_call(&mut self, operands: &Operands) -> Option<VMError> {
        let obj = &*self.data_stack.top_offset(operands[0]);
        // dbg!(&self.data_stack.stack, obj, operands);
        match obj {
            Object::CompiledFunction {
                instructions,
                locals,
            } => {
                let bp = self.data_stack.stack_pointer;
                let frame = Frame::new(instructions.clone(), bp);
                match self.call_stack.push_frame(RefCell::new(frame)) {
                    Err(err) => Some(err),
                    _ => {
                        self.data_stack.stack_pointer = bp + *locals as i64;
                        None
                    }
                }
            }
            _ => Some(VMError::new(
                VMErrorKind::RuntimeError,
                "calling non-function object".to_string(),
            )),
        }
    }
    fn exec_returns(&mut self, op: &Instruction) -> Option<VMError> {
        match op {
            Instruction::Return => {
                let current_frame_res = self.call_stack.pop_frame();
                if current_frame_res.is_err() {
                    return Some(current_frame_res.unwrap_err());
                }
                let frame = current_frame_res.unwrap();

                self.data_stack.stack_pointer = frame.borrow().bp - 1;
                self.data_stack.push_object(Rc::new(Object::Nil))
            }
            Instruction::ReturnValue => {
                let return_value = self.data_stack.pop_object();
                if return_value.is_none() {
                    return None;
                };
                let return_value = return_value.unwrap();

                let current_frame_res = self.call_stack.pop_frame();
                if current_frame_res.is_err() {
                    return Some(current_frame_res.unwrap_err());
                }
                let frame = current_frame_res.unwrap();
                let bp = frame.borrow().bp;
                self.data_stack.stack.truncate(bp as usize);

                self.data_stack.stack_pointer = frame.borrow().bp - 1;
                self.data_stack.push_object(return_value)
            }
            _ => None,
        }
    }

    pub fn build_array(&self, range: Range<usize>) -> Rc<Object> {
        let mut elements = vec![Object::Nil; range.end - range.start];
        for idx in range.clone() {
            elements[idx - range.start] = self.data_stack.stack[idx].as_ref().clone();
        }
        Rc::new(Object::Array(elements))
    }

    pub fn build_hash(&self, range: Range<usize>) -> Rc<Object> {
        let mut pairs: HashMap<Object, Object> = HashMap::new();
        let mut idx = range.start;
        while idx < range.end {
            let key = self.data_stack.stack[idx].as_ref().clone();
            let obj = self.data_stack.stack[idx + 1].as_ref().clone();
            pairs.insert(key, obj);
            idx += 2
        }
        Rc::new(Object::Hash(pairs))
    }
}
