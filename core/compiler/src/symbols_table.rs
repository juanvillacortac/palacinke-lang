use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::objects::Object;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SymbolScope {
    Public,
    Global,
    Local,
    Free,
    Function,
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub scope: SymbolScope,
    pub index: usize,
}

#[derive(Debug, Clone)]
pub struct SymbolTable {
    pub outer: Option<Box<RefCell<SymbolTable>>>,
    pub num_defs: usize,
    store: HashMap<String, Symbol>,
    pub free: Vec<Symbol>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            outer: None,
            store: HashMap::new(),
            num_defs: 0,
            free: vec![],
        }
    }

    pub fn new_enclosed(outer: Self) -> Self {
        Self {
            outer: Some(Box::new(RefCell::new(outer))),
            store: HashMap::new(),
            num_defs: 0,
            free: vec![],
        }
    }

    pub fn define_function(&mut self, name: &str) -> Symbol {
        let symbol = Symbol {
            index: 0,
            scope: SymbolScope::Function,
            name: name.to_string(),
        };
        self.store.insert(name.to_string(), symbol.clone());
        symbol
    }

    pub fn define(&mut self, name: &str) -> Symbol {
        let scope = match self.outer {
            Some(_) => SymbolScope::Local,
            None => SymbolScope::Global,
        };
        let symbol = Symbol {
            index: self.num_defs,
            scope,
            name: name.to_string(),
        };
        if !self.store.contains_key(&name.to_string()) {
            self.num_defs += 1;
        }
        self.store.insert(name.to_string(), symbol.clone());
        symbol
    }

    pub fn define_public(&mut self, name: &str) -> Symbol {
        let symbol = Symbol {
            index: self.num_defs,
            scope: SymbolScope::Public,
            name: name.to_string(),
        };
        self.num_defs += 1;
        self.store.insert(name.to_string(), symbol.clone());
        symbol
    }

    pub fn define_free(&mut self, original: &Symbol) -> Symbol {
        self.free.push(original.clone());
        let symbol = Symbol {
            index: self.free.len() - 1,
            scope: SymbolScope::Free,
            name: original.name.clone(),
        };
        self.store.insert(symbol.name.clone(), symbol.clone());
        symbol
    }

    pub fn resolve(&mut self, name: &str) -> Option<Symbol> {
        let inner = self.store.get(name).cloned();
        match (&self.outer, &inner) {
            (Some(outer), None) => {
                let symbol = outer.borrow_mut().resolve(name);
                match symbol {
                    Some(obj) => match obj.scope.clone() {
                        SymbolScope::Global => Some(obj.clone()),
                        _ => Some(self.define_free(&obj)),
                    },
                    None => inner,
                }
            }
            _ => inner,
        }
    }

    pub fn get_public_symbols(&self) -> Vec<Symbol> {
        self.store
            .iter()
            .filter(|(_, v)| v.scope == SymbolScope::Public)
            .map(|(_, v)| v.clone())
            .collect()
    }

    pub fn get_public_objects(&self, globals: Vec<Option<Object>>) -> HashMap<String, Object> {
        let symbols = self.get_public_symbols();
        let mut hash = HashMap::new();
        for (idx, obj) in globals.iter().enumerate() {
            let obj = obj.clone().unwrap_or(Object::Nil);
            let matched = symbols.iter().find(|v| v.index == idx);
            if matched.is_some() {
                hash.insert(matched.unwrap().name.clone(), obj);
            }
        }
        hash
    }
}

#[derive(Debug, Clone)]
pub struct ConstantsPool {
    pub objects: Vec<Rc<Object>>,
    pub size: usize,
}

impl ConstantsPool {
    pub fn new() -> ConstantsPool {
        return ConstantsPool {
            objects: vec![],
            size: 0,
        };
    }

    pub fn get_object(&self, idx: usize) -> Option<Rc<Object>> {
        if idx < self.size {
            return Some(self.objects[idx].clone());
        }
        return None;
    }

    pub fn set_object(&mut self, object: Rc<Object>) -> usize {
        self.objects.push(object);
        self.size += 1;
        return self.size - 1;
    }

    pub fn get_size(&self) -> usize {
        self.size
    }
}
