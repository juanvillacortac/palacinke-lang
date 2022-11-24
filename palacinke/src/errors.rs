use pk_compiler::errors::CompilationError;
use pk_parser::ParseErrors;
use pk_vm::errors::VMError;

pub enum EvaluationError {
    CompilationError(CompilationError),
    ParseErrors(ParseErrors),
    VMError(VMError),
}
