use std::{
    cell::{Ref, RefCell, RefMut},
    rc::Rc,
};

use pk_compiler::objects::Object;

use crate::{
    errors::{VMError, VMErrorKind},
    frames::Frame,
};

const FRAME_STACK_SIZE: usize = 1024;
const DATA_STACK_SIZE: usize = 2048;

pub type FrameRef = RefCell<Frame>;

#[derive(Debug)]
pub struct CallStack {
    pub stack: Vec<FrameRef>,
    pub max_size: usize,
}

impl CallStack {
    pub fn new() -> CallStack {
        return CallStack {
            stack: vec![],
            max_size: FRAME_STACK_SIZE,
        };
    }

    pub fn push_frame(&mut self, frame: FrameRef) -> Result<usize, VMError> {
        if self.stack.len() >= self.max_size {
            return Err(VMError::new(
                VMErrorKind::RuntimeError,
                "Call stack overflow".to_string(),
            ));
        }

        self.stack.push(frame);
        return Ok(self.stack.len());
    }

    pub fn pop_frame(&mut self) -> Result<FrameRef, VMError> {
        if self.stack.len() == 0 {
            return Err(VMError::new(
                VMErrorKind::RuntimeError,
                "Call stack underflow".to_string(),
            ));
        }

        let popped = self.stack.pop();

        return Ok(popped.unwrap());
    }

    pub fn top(&mut self) -> RefMut<Frame> {
        self.stack.last().unwrap().borrow_mut()
    }

    pub fn top_ref(&self) -> Ref<Frame> {
        self.stack.last().unwrap().borrow()
    }
}

pub struct DataStack {
    pub stack: Vec<Rc<Object>>,
    pub max_size: usize,
    pub last_popped: Option<Rc<Object>>,
    pub stack_pointer: i64,
}

impl DataStack {
    pub fn new() -> DataStack {
        let mut stack = vec![];
        stack.reserve(DATA_STACK_SIZE);

        return DataStack {
            stack,
            max_size: DATA_STACK_SIZE,
            last_popped: None,
            stack_pointer: 0,
        };
    }

    pub fn push_object(&mut self, obj: Rc<Object>) -> Option<VMError> {
        if self.stack.len() >= self.max_size {
            return Some(VMError::new(
                VMErrorKind::RuntimeError,
                "Stack overflow".to_string(),
            ));
        }

        self.stack.push(obj);
        self.stack_pointer += 1;
        None
    }

    pub fn pop_object(&mut self) -> Option<Rc<Object>> {
        if self.stack.len() == 0 {
            return None;
        }

        let popped = self.stack.pop();
        self.stack_pointer -= 1;
        self.last_popped = popped;
        self.last_popped.clone()
    }

    pub fn top(&mut self) -> Rc<Object> {
        self.stack.last().unwrap().clone()
    }
    pub fn top_offset(&mut self, offset: usize) -> Rc<Object> {
        self.stack
            .get((self.stack.len() - offset) - 1)
            .unwrap()
            .clone()
    }
}
