extern crate inkwell;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValue, BasicValueEnum, FloatValue, FunctionValue, PointerValue};
use inkwell::{FloatPredicate, OptimizationLevel};

use jack_compiler::ast::*;
use jack_compiler::defines::TokenType;

pub struct ClassCompiler<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub fpm: &'a PassManager<FunctionValue<'ctx>>,
    pub module: &'a Module<'ctx>,
    // variables: HashMap<String, PointerValue<'ctx>>,
    // fn_value_opt: Option<FunctionValue<'ctx>>,
}

impl<'a, 'ctx> ClassCompiler<'a, 'ctx> {
    pub fn new(
        context: &'ctx Context,
        builder: &'a Builder<'ctx>,
        pass_manager: &'a PassManager<FunctionValue<'ctx>>,
        module: &'a Module<'ctx>,
    ) -> ClassCompiler<'a, 'ctx> {
        ClassCompiler {
            context: context,
            builder: builder,
            fpm: pass_manager,
            module: module,
        }
    }

    pub fn expr_codegen(&mut self, expr: Box<Expr>) -> FunctionValue<'ctx> {}
}

fn main() {
    let context = Context::create();
    let module = context.create_module("jackc");
    let builder = context.create_builder();
    let fpm = PassManager::create(&module);
    // fpm.add_reassociate_pass();
    fpm.initialize();

    let mut cc = ClassCompiler::new(&context, &builder, &fpm, &module);
}
