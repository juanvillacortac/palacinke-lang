mod context;
pub mod errors;
mod frames;
mod stack;

use std::{cell::RefCell, collections::HashMap, rc::Rc};

use context::VMContext;
use errors::*;
use frames::Frame;
use pk_compiler::{modules_table::ModulesTable, objects::*, symbols_table::ConstantsPool};
use stack::{CallStack, DataStack};

const GLOBALS_SIZE: usize = 65536;

pub type GlobalsStore = Vec<Option<Object>>;
pub type ModulesStore = Vec<Option<Object>>;
pub type Stack = Vec<Option<Object>>;

pub fn new_globals_store() -> GlobalsStore {
    vec![None; GLOBALS_SIZE]
}

pub struct VM<'a> {
    constants: ConstantsPool,
    modules_table: ModulesTable,
    globals: &'a mut GlobalsStore,
    modules: &'a mut ModulesStore,
    data_stack: DataStack,
    call_stack: CallStack,
    exported_objects: HashMap<Object, Object>,
}

impl<'a> VM<'a> {
    pub fn new_state(
        module: &Module,
        modules_table: ModulesTable,
        globals: &'a mut GlobalsStore,
        modules: &'a mut ModulesStore,
    ) -> Self {
        let mut call_stack = CallStack::new();
        let data_stack = DataStack::new();

        let main_frame = Frame::new(
            Closure {
                function: CompiledFunction {
                    instructions: module.bytecode.instructions.clone(),
                    locals: 0,
                },
                free: vec![],
            },
            0,
        );
        let _ = call_stack.push_frame(RefCell::new(main_frame));

        return VM {
            modules_table,
            constants: module.bytecode.constants.clone(),
            call_stack,
            data_stack,
            globals,
            modules,
            exported_objects: HashMap::new(),
        };
    }

    pub fn eval(&mut self) -> Result<Rc<Object>, VMError> {
        let mut ctx = VMContext {
            modules_table: self.modules_table.clone(),
            call_stack: &mut self.call_stack,
            constants: &mut self.constants,
            data_stack: &mut self.data_stack,
            globals: &mut self.globals,
            modules: &mut self.modules,
            exported_objects: &mut self.exported_objects,
        };
        Self::eval_from_context(&mut ctx)
    }

    pub fn exported(&self) -> Object {
        Object::Hash(self.exported_objects.clone())
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
