use std::{fs, path::PathBuf};

use pk_parser::{Parser, ParserResult};

pub type ModuleLoader = dyn FnOnce(&str, &str) -> ParserResult;
pub type ModuleResolver = dyn FnOnce(&str, &str) -> Result<String, LoaderErr>;

pub struct LoaderErr(pub String);

impl From<std::io::Error> for LoaderErr {
    fn from(err: std::io::Error) -> Self {
        Self(err.to_string())
    }
}

pub fn fs_resolver(current: &str, module_path: &str) -> Result<String, LoaderErr> {
    let md = fs::metadata(&current)?;
    let cwd = if md.is_file() {
        let path = PathBuf::from(&current.to_string());
        PathBuf::from(path.parent().unwrap())
    } else {
        PathBuf::from(&current.to_string())
    }
    .canonicalize()?;

    let mut import_path = PathBuf::from(&cwd);
    import_path.push(module_path);
    Ok(import_path.to_str().unwrap().to_string())
}

pub fn fs_loader(path: &str) -> Result<ParserResult, LoaderErr> {
    let buff = fs::read(path.clone())?;

    let source = String::from_utf8_lossy(&buff);
    let mut parser = Parser::from_source(&source);
    Ok(parser.parse())
}
