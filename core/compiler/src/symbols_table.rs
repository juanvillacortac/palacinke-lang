use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum SymbolScope {
    Global,
    Local(String),
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub scope: SymbolScope,
    pub index: usize,
}

#[derive(Debug, Clone)]
pub struct SymbolTable {
    store: HashMap<String, Symbol>,
    num_defs: usize,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
            num_defs: 0,
        }
    }

    pub fn define(&mut self, name: &str) -> Symbol {
        let symbol = Symbol {
            index: self.num_defs,
            scope: SymbolScope::Global,
            name: name.to_string(),
        };
        self.store.insert(name.to_string(), symbol.clone());
        self.num_defs += 1;
        symbol
    }

    pub fn resolve(&self, name: &str) -> Option<Symbol> {
        self.store.get(name).cloned()
    }
}
