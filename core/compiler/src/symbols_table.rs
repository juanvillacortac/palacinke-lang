use std::{collections::HashMap, rc::Rc};

use crate::objects::Object;

#[derive(Debug, Clone)]
pub enum SymbolScope {
    Global,
    Local,
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub scope: SymbolScope,
    pub index: usize,
}

#[derive(Debug, Clone)]
pub struct SymbolTable {
    pub outer: Option<Box<SymbolTable>>,
    pub num_defs: usize,
    store: HashMap<String, Symbol>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            outer: None,
            store: HashMap::new(),
            num_defs: 0,
        }
    }

    pub fn new_enclosed(outer: Self) -> Self {
        Self {
            outer: Some(Box::new(outer)),
            store: HashMap::new(),
            num_defs: 0,
        }
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

    pub fn resolve(&self, name: &str) -> Option<Symbol> {
        let inner = self.store.get(name).cloned();
        match (&self.outer, &inner) {
            (Some(outer), None) => outer.resolve(name),
            _ => inner,
        }
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
