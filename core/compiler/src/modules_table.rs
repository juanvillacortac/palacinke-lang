use std::{collections::HashMap, rc::Rc};

use crate::{
    errors::{CompilationError, CompilationErrorKind},
    loaders::{fs_loader, fs_resolver, LoaderErr},
    objects::Module,
    symbols_table::{ConstantsPool, SymbolTable},
    Compiler,
};

#[derive(Debug, Clone)]
pub struct ModuleEntry {
    pub module: Rc<Module>,
    pub index: usize,
}

#[derive(Debug, Clone)]
pub struct ModulesTable {
    pub store: HashMap<String, ModuleEntry>,
    pub stack: Vec<Rc<Module>>,
}

impl ModulesTable {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
            stack: vec![],
        }
    }

    pub fn define(
        &mut self,
        current: &str,
        module_path: &str,
    ) -> Result<ModuleEntry, CompilationError> {
        let path = match fs_resolver(current, module_path) {
            Ok(path) => path,
            Err(LoaderErr(err)) => {
                return Err(CompilationError::new(
                    CompilationErrorKind::ModuleImport(module_path.to_string()),
                    format!("{}", err),
                ));
            }
        };

        if current == &path {
            return Err(CompilationError::new(
                CompilationErrorKind::ModuleImport(module_path.to_string()),
                format!("you cannot self-import a module"),
            ));
        }

        if self.store.contains_key(&path) {
            return Ok(self.store.get(&path).unwrap().clone());
        }

        let parsed_module = match fs_loader(&path) {
            Ok(result) => result,
            Err(LoaderErr(err)) => {
                return Err(CompilationError::new(
                    CompilationErrorKind::ModuleImport(path.clone()),
                    format!("{}", err),
                ));
            }
        };

        let parsed_module = match parsed_module {
            Ok(module) => module,
            Err(errors) => {
                let msg = errors
                    .into_iter()
                    .map(|e| format!("-> {}", e))
                    .collect::<Vec<String>>()
                    .join("\n");
                return Err(CompilationError::new(
                    CompilationErrorKind::ModuleImport(path.clone()),
                    format!("error parsing module:\n{}", msg),
                ));
            }
        };
        let mut symbols_table = SymbolTable::new();
        let mut constants = ConstantsPool::new();
        let mut compiler = Compiler::new(&mut symbols_table, &mut constants, self, &path);
        let module = match compiler.compile(parsed_module) {
            Ok(module) => Rc::new(module),
            Err(err) => {
                return Err(CompilationError::new(
                    CompilationErrorKind::ModuleImport(path.clone()),
                    format!("{}", err),
                ));
            }
        };
        let entry = ModuleEntry {
            index: self.stack.len(),
            module: Rc::clone(&module),
        };

        self.store.insert(path.to_string(), entry.clone());
        self.stack.push(Rc::clone(&module));

        Ok(entry)
    }

    pub fn resolve(&self, index: usize) -> Option<Rc<Module>> {
        self.stack.get(index).cloned()
    }
}
