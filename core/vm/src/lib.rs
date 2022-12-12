mod context;
pub mod errors;
mod frames;
mod stack;

use std::{cell::RefCell, rc::Rc};

use context::VMContext;
use errors::*;
use frames::Frame;
use pk_compiler::{objects::*, symbols_table::ConstantsPool, CompiledBytecode};
use stack::{CallStack, DataStack};

const GLOBALS_SIZE: usize = 65536;

pub type GlobalsStore = Vec<Option<Object>>;
pub type Stack = Vec<Option<Object>>;

pub fn new_globals_store() -> GlobalsStore {
    vec![None; GLOBALS_SIZE]
}

pub struct VM<'a> {
    constants: ConstantsPool,
    globals: &'a mut GlobalsStore,
    data_stack: DataStack,
    call_stack: CallStack,
}

impl<'a> VM<'a> {
    pub fn new_state(bytecode: &CompiledBytecode, globals: &'a mut GlobalsStore) -> Self {
        let mut call_stack = CallStack::new();
        let data_stack = DataStack::new();

        let main_frame = Frame::new(
            Closure {
                function: CompiledFunction {
                    instructions: bytecode.instructions.clone(),
                    locals: 0,
                },
                free: vec![],
            },
            0,
        );
        let _ = call_stack.push_frame(RefCell::new(main_frame));

        return VM {
            constants: bytecode.constants.clone(),
            call_stack,
            data_stack,
            globals,
        };
    }

    pub fn new_empty_from_state(globals: &'a mut GlobalsStore, constants: ConstantsPool) -> Self {
        let call_stack = CallStack::new();
        let data_stack = DataStack::new();

        return VM {
            constants,
            call_stack,
            data_stack,
            globals,
        };
    }

    pub fn eval(&mut self) -> Result<Rc<Object>, VMError> {
        let mut ctx = VMContext {
            call_stack: &mut self.call_stack,
            constants: &mut self.constants,
            data_stack: &mut self.data_stack,
            globals: &mut self.globals,
        };
        Self::eval_from_context(&mut ctx)
    }

    pub fn push_new_frame(&mut self, frame: RefCell<Frame>) -> Option<VMError> {
        let push_result = self.call_stack.push_frame(frame);
        if push_result.is_err() {
            return Some(push_result.unwrap_err());
        }
        return None;
    }

    pub fn eval_from_context(ctx: &mut VMContext) -> Result<Rc<Object>, VMError> {
        while ctx.call_stack.top_ref().has_instructions() {
            let ins = ctx.call_stack.top_ref().read_current_instruction();

            match ctx.exec_instruction(&ins) {
                Some(err) => return Err(err),
                _ => {}
            };

            ctx.call_stack.top().forward_ip(&ins);
        }
        match &ctx.data_stack.last_popped {
            Some(popped_result) => Ok(popped_result.clone()),
            None => Ok(Rc::new(Object::Nil)),
        }
    }
}
