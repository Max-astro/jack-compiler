extern crate inkwell;

use std::collections::HashMap;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::types::{AnyTypeEnum, BasicType, BasicTypeEnum, FunctionType, StructType};
use inkwell::values::{
    AnyValue, BasicValue, BasicValueEnum, FloatValue, FunctionValue, PointerValue,
};
use inkwell::AddressSpace;
use inkwell::{IntPredicate, OptimizationLevel};

use jack_compiler::ast::*;
use jack_compiler::defines::TokenType;

struct ClassTypeDecl<'a, 'ctx> {
    compiler: IRCompiler<'a, 'ctx>,
    class_ty: StructType<'ctx>,
    constructor: Option<FunctionType<'ctx>>,
    functions: HashMap<String, FunctionType<'ctx>>,
    methods: HashMap<String, FunctionType<'ctx>>,
    static_members: HashMap<String, BasicTypeEnum<'ctx>>,
    field_members: HashMap<String, BasicTypeEnum<'ctx>>,
    member_offset: HashMap<String, usize>,
}

// impl<'a, 'ctx> ClassTypeDecl<'a, 'ctx> {
//     // pub fn new_class()
//     pub fn process_var_decl(&mut self, memb_vars: &[ClassVarDec]) {
//         for var in memb_vars {
//             if var.field {
//                 let ty = self.compiler.convert_type(&var.ty);
//                 let size = ty.into().size_of();
//             }
//         }
//     }
// }

pub struct IRCompiler<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub fpm: &'a PassManager<FunctionValue<'ctx>>,
    pub module: &'a Module<'ctx>,
    pub instance_symbol_tbl: HashMap<String, (ClassType, BasicValueEnum<'ctx>)>,
    type_cache: HashMap<String, BasicTypeEnum<'ctx>>,
    curr_fn: Option<FunctionValue<'ctx>>,
    // functions: HashMap<String, FunctionValue<'ctx>>,
    // fn_value_opt: Option<FunctionValue<'ctx>>,
}

impl<'a, 'ctx> IRCompiler<'a, 'ctx> {
    pub fn new(
        context: &'ctx Context,
        builder: &'a Builder<'ctx>,
        pass_manager: &'a PassManager<FunctionValue<'ctx>>,
        module: &'a Module<'ctx>,
    ) -> IRCompiler<'a, 'ctx> {
        IRCompiler {
            context: context,
            builder: builder,
            fpm: pass_manager,
            module: module,
            instance_symbol_tbl: HashMap::new(),
            type_cache: HashMap::new(),
            curr_fn: None,
        }
    }

    pub fn expr_codegen(&mut self, expr: Box<Expr>) -> BasicValueEnum<'ctx> {
        match *expr {
            Expr::IntConst(num) => self
                .context
                .i32_type()
                .const_int(num as u64, true)
                .as_basic_value_enum(),
            Expr::KwConst(token) => match token {
                TokenType::True => self
                    .context
                    .bool_type()
                    .const_int(1, false)
                    .as_basic_value_enum(),
                TokenType::False => self
                    .context
                    .bool_type()
                    .const_int(0, false)
                    .as_basic_value_enum(),
                _ => panic!("unexpected token: {:?}", token),
            },
            Expr::Binary { op, left, right } => {
                let lhs = self.expr_codegen(left).into_int_value();
                let rhs = self.expr_codegen(right).into_int_value();
                match op {
                    TokenType::Plus => self
                        .builder
                        .build_int_add(lhs, rhs, "tmpadd")
                        .as_basic_value_enum(),
                    TokenType::Minus => self
                        .builder
                        .build_int_sub(lhs, rhs, "tmpsub")
                        .as_basic_value_enum(),
                    TokenType::Multi => self
                        .builder
                        .build_int_mul(lhs, rhs, "tmpmul")
                        .as_basic_value_enum(),
                    TokenType::Divide => self
                        .builder
                        .build_int_signed_div(lhs, rhs, "tmpdiv")
                        .as_basic_value_enum(),
                    TokenType::And => self
                        .builder
                        .build_and(lhs, rhs, "tmpand")
                        .as_basic_value_enum(),
                    TokenType::Or => self
                        .builder
                        .build_or(lhs, rhs, "tmpor")
                        .as_basic_value_enum(),
                    TokenType::Gt => self
                        .builder
                        .build_int_compare(IntPredicate::SGT, lhs, rhs, "tmpgt")
                        .as_basic_value_enum(),
                    TokenType::Lt => self
                        .builder
                        .build_int_compare(IntPredicate::SLT, lhs, rhs, "tmplt")
                        .as_basic_value_enum(),
                    TokenType::Equal => self
                        .builder
                        .build_int_compare(IntPredicate::EQ, lhs, rhs, "tmpeq")
                        .as_basic_value_enum(),
                    _ => panic!("unexpected token: {:?}", op),
                }
            }
            Expr::Unary { op, expr } => {
                let val = self.expr_codegen(expr).into_int_value();
                match op {
                    TokenType::Not => self.builder.build_not(val, "tmpnot").as_basic_value_enum(),
                    TokenType::Minus => self
                        .builder
                        .build_int_neg(val, "tmpneg")
                        .as_basic_value_enum(),
                    _ => panic!("expr_codgen: {:?} is not a unary op", op),
                }
            }
            Expr::StringConst(_) => todo!(),
            Expr::VarName(var_name) => {
                if let Some(&(_, value)) = self.instance_symbol_tbl.get(&var_name) {
                    value
                } else {
                    panic!("expr_codgen: can not find {} in symbel teble", var_name);
                }
            }
            Expr::ArrElem(_, _) => todo!(),
            Expr::SubroutineCall {
                class_type,
                obj_name,
                routine_name,
                args,
            } => {
                if let Some(class) = class_type {
                    let class_name = match class {
                        ClassType::Class(class_name) => class_name,
                        _ => panic!("Not a class"),
                    };

                    if obj_name.is_some() {
                        panic!("This subroutine should be a static function, can not apply in a object.");
                    }

                    let fn_name = format!("class_{}_fn_{}", class_name, routine_name);
                    let args_val = args
                        .into_iter()
                        .map(|expr| self.expr_codegen(expr))
                        .collect::<Vec<_>>();

                    self.subroutine_call(fn_name, args_val.as_slice())
                } else {
                    let mut args_val = args
                        .into_iter()
                        .map(|expr| self.expr_codegen(expr))
                        .collect::<Vec<_>>();

                    let obj_name = obj_name.unwrap();
                    if let Some((class, value)) = self.instance_symbol_tbl.get(&obj_name) {
                        let class_name = match class {
                            ClassType::Class(class_name) => class_name,
                            _ => panic!("Not a class"),
                        };

                        let fn_name = format!("class_{}_method_{}", class_name, routine_name);
                        args_val.insert(0, value.clone());
                        self.subroutine_call(fn_name, args_val.as_slice())
                    } else {
                        panic!("Object {} not found", obj_name);
                    }
                }
            }
        }
    }

    fn subroutine_call(
        &mut self,
        fn_name: String,
        args: &[BasicValueEnum<'ctx>],
    ) -> BasicValueEnum<'ctx> {
        if let Some(fn_val) = self.module.get_function(&fn_name) {
            match self
                .builder
                .build_call(fn_val, args, "fncall")
                .try_as_basic_value()
                .left()
            {
                Some(value) => value,
                None => panic!("Invalid call produced."),
            }
        } else {
            panic!("Function {} not been decleared", fn_name);
        }
    }

    pub fn stmt_codegen(&mut self, stmt: Vec<Box<Stmt>>) {
        for s in stmt {
            match *s {
                Stmt::LetVar(var_name, expr) => {
                    todo!();
                }
                Stmt::LetArr {
                    arr_name,
                    idx_expr,
                    rhs,
                } => todo!(),
                Stmt::IfStmt {
                    cond,
                    consequence,
                    alternative,
                } => todo!(),
                Stmt::WhileStmt(_, _) => todo!(),
                Stmt::DoStmt(_) => todo!(),
                Stmt::ReturnStmt(_) => todo!(),
            };
        }
    }

    fn let_stmt_codegen(&mut self, stmt: Box<Stmt>) {}
    fn if_stmt_codegen(&mut self, function: FunctionValue<'ctx>, stmt: Box<Stmt>) {}
    fn while_stmt_codegen(&mut self, function: FunctionValue<'ctx>, stmt: Box<Stmt>) {
        let mut cond_bb = self.context.append_basic_block(function, "while_cond");
    }
    fn do_stmt_codegen(&mut self, function: FunctionValue<'ctx>, stmt: Box<Stmt>) {}
    fn return_stmt_codegen(&mut self, function: FunctionValue<'ctx>, stmt: Box<Stmt>) {}

    fn convert_type(&self, class_ty: &ClassType) -> BasicTypeEnum<'ctx> {
        match *class_ty {
            ClassType::Boolean => self.context.bool_type().into(),
            ClassType::Void => panic!("Void type cann not convert to basic type"),
            ClassType::Int => self.context.i32_type().into(),
            ClassType::Char => self.context.i8_type().into(),
            ClassType::Class(ref name) => {
                if let Some(ty) = self.type_cache.get(name) {
                    ty.clone()
                } else {
                    panic!("Type: {} no been declared!", name);
                }
            }
        }
    }

    fn create_new_class(&mut self, name: String, members: &ParamList) {
        if let Some(exist) = self.type_cache.get(&name) {
            panic!("{} alreadly exist. {:?}", name, exist);
        }

        let mems_ty = members
            .iter()
            .map(|(ty, _)| self.convert_type(ty))
            .collect::<Vec<_>>();
        let new_type = self.context.struct_type(&mems_ty, false);
        self.type_cache.insert(name, new_type.as_basic_type_enum());
    }

    fn subroutine_codegen(&mut self, mut subroutine: SubroutineDec) -> FunctionValue<'ctx> {
        let fn_val = self.create_subroutine_fn_type(&subroutine);
        let entry = self
            .context
            .append_basic_block(fn_val, &subroutine.routine_name);
        self.builder.position_at_end(entry);

        self.curr_fn = Some(fn_val);

        // build variables map
        // process subroutine args
        for (idx, arg) in fn_val.get_param_iter().enumerate() {
            let (class_ty, name) = &subroutine.args[idx];
            let ty = self.convert_type(class_ty);
            if self.instance_symbol_tbl.contains_key(name) {
                panic!("subroutine_codegen: variable {} alreadly exist.", name);
            }
            let alloca = self.create_entry_block_alloca(ty, name);
            self.builder.build_store(alloca, arg);
            self.instance_symbol_tbl.insert(
                name.clone(),
                (class_ty.clone(), alloca.as_basic_value_enum()),
            );
        }

        // process subroutine local variables
        for (class_ty, names) in subroutine.body.var_decl.iter() {
            let ty = self.convert_type(class_ty);
            for name in names {
                if self.instance_symbol_tbl.contains_key(name) {
                    panic!("subroutine_codegen: variable {} alreadly exist.", name);
                }
                let alloca = self.create_entry_block_alloca(ty, name);
                self.instance_symbol_tbl.insert(
                    name.clone(),
                    (class_ty.clone(), alloca.as_basic_value_enum()),
                );
            }
        }

        for s in subroutine.body.stmt {
            match *s {
                Stmt::LetVar(_, _) => todo!(),
                Stmt::LetArr {
                    arr_name,
                    idx_expr,
                    rhs,
                } => todo!(),
                Stmt::IfStmt {
                    cond,
                    consequence,
                    alternative,
                } => todo!(),
                Stmt::WhileStmt(_, _) => todo!(),
                Stmt::DoStmt(_) => todo!(),
                Stmt::ReturnStmt(stmt) => {
                    if let Some(expr) = stmt {
                        let ret = self.expr_codegen(expr);
                        self.builder.build_return(Some(&ret));
                    } else {
                        self.builder.build_return(None);
                    }
                }
            }
        }

        fn_val
    }

    fn create_entry_block_alloca(&self, ty: BasicTypeEnum<'ctx>, name: &str) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();
        let entry = self
            .curr_fn
            .and_then(|fn_val| fn_val.get_first_basic_block())
            .unwrap();

        match entry.get_first_instruction() {
            Some(first_inst) => builder.position_before(&first_inst),
            None => builder.position_at_end(entry),
        }
        builder.build_alloca(ty, name)
    }

    fn create_subroutine_fn_type(&mut self, subroutine: &SubroutineDec) -> FunctionValue<'ctx> {
        let args_ty = subroutine
            .args
            .iter()
            .map(|(ty, _)| self.convert_type(ty))
            .collect::<Vec<BasicTypeEnum>>();

        let fn_type = match &subroutine.ret_type {
            ClassType::Void => self.context.void_type().fn_type(args_ty.as_slice(), false),
            ty => self.convert_type(ty).fn_type(args_ty.as_slice(), false),
        };
        let fn_val = self
            .module
            .add_function(subroutine.routine_name.as_str(), fn_type, None);
        for (idx, arg) in fn_val.get_param_iter().enumerate() {
            let (_, ref name) = subroutine.args[idx];
            arg.set_name(name.as_str());
        }

        fn_val
    }
}

fn main() {
    let context = Context::create();
    let module = context.create_module("jackc");
    let builder = context.create_builder();
    let fpm = PassManager::create(&module);
    // fpm.add_reassociate_pass();
    fpm.initialize();

    let void_type = context.void_type();
    let fn_type = void_type.fn_type(&[], false);
    let function = module.add_function("do_nothing", fn_type, None);
    let basic_block = context.append_basic_block(function, "entry");
    println!("{:?}", basic_block);
    builder.position_at_end(basic_block);

    // ------------
    let struct_type = create_struct_type(&context);
    println!("{:?}", struct_type);
    let struct_ptr = builder.build_alloca(struct_type, "tmp_struct");

    let gep = builder
        .build_struct_gep(struct_ptr, 1, "float_access")
        .unwrap();
    println!("{:?}\n", gep);

    let fval = context.f32_type().const_float(7.0);
    let stor = builder.build_store(gep, fval);
    println!("{:?}", stor);

    let val = builder.build_load(struct_ptr, "load_ptr");
    println!("{:?}", val);
    // let mut cc = IRCompiler::new(&context, &builder, &fpm, &module);
}

fn tmp(context: &Context) {
    let f32_type = context.f32_type();
    let i32_type = context.i32_type();
    let i8_type = context.i8_type();
    let struct_type =
        context.struct_type(&[i32_type.into(), f32_type.into(), i8_type.into()], false);

    let sz = context.i64_type().const_zero();
    for val in struct_type.get_field_types() {
        let i = val.size_of().unwrap();
        sz.const_add(i);
    }

    // println!("{:?}\n {:?}", sz, struct_type.size_of());
    println!("{}\n", sz.print_to_string().to_string());

    println!(
        "{}\n",
        struct_type.size_of().unwrap().print_to_string().to_string()
    );
}

fn create_struct_type(context: &Context) -> StructType {
    let f32_type = context.f32_type();
    let i32_type = context.i32_type();
    let i8_type = context.i8_type();
    context.struct_type(&[i32_type.into(), f32_type.into(), i8_type.into()], false)
}

fn gep_tmp() {
    let context = Context::create();
    let builder = context.create_builder();
    let module = context.create_module("struct_gep");
    let void_type = context.void_type();
    let i32_ty = context.i32_type();
    let i32_ptr_ty = i32_ty.ptr_type(AddressSpace::Generic);
    let field_types = &[i32_ty.into(), i32_ty.into()];
    let struct_ty = context.struct_type(field_types, false);
    let struct_ptr_ty = struct_ty.ptr_type(AddressSpace::Generic);
    let fn_type = void_type.fn_type(&[i32_ptr_ty.into(), struct_ptr_ty.into()], false);
    let fn_value = module.add_function("", fn_type, None);
    let entry = context.append_basic_block(fn_value, "entry");

    builder.position_at_end(entry);

    // let i32_ptr = fn_value.get_first_param().unwrap().into_pointer_value();
    // let struct_ptr = fn_value.get_last_param().unwrap().into_pointer_value();

    // assert!(builder.build_struct_gep(i32_ptr, 0, "struct_gep").is_err());
    // assert!(builder.build_struct_gep(i32_ptr, 10, "struct_gep").is_err());
    // assert!(builder
    //     .build_struct_gep(struct_ptr, 0, "struct_gep")
    //     .is_ok());
    // assert!(builder
    //     .build_struct_gep(struct_ptr, 1, "struct_gep")
    //     .is_ok());
    // assert!(builder
    //     .build_struct_gep(struct_ptr, 2, "struct_gep")
    //     .is_err());
}
