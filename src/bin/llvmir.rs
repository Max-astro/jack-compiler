extern crate inkwell;

use std::collections::HashMap;
use std::panic;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::types::{BasicType, BasicTypeEnum, StructType};
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue};
use inkwell::AddressSpace;
use inkwell::IntPredicate;

use jack_compiler::ast::*;
use jack_compiler::defines::TokenType;

// struct ClassTypeDecl<'a, 'ctx> {
//     compiler: IRCompiler<'a, 'ctx>,
//     class_ty: StructType<'ctx>,
//     constructor: Option<FunctionType<'ctx>>,
//     functions: HashMap<String, FunctionType<'ctx>>,
//     methods: HashMap<String, FunctionType<'ctx>>,
//     static_members: HashMap<String, BasicTypeEnum<'ctx>>,
//     field_members: HashMap<String, BasicTypeEnum<'ctx>>,
//     member_offset: HashMap<String, usize>,
// }

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
    pub class_member_idx: HashMap<String, u32>,
    type_cache: HashMap<String, BasicTypeEnum<'ctx>>,
    curr_fn: Option<FunctionValue<'ctx>>,
    this_ptr: Option<PointerValue<'ctx>>,
    pub curr_symbel_tbl: HashMap<String, PointerValue<'ctx>>,
    pub curr_class_name: String,
    // functions: HashMap<String, FunctionValue<'ctx>>,
    // fn_value_opt: Option<FunctionValue<'ctx>>,
}

/* LLVM IR Code Generate */
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
            class_member_idx: HashMap::new(),
            type_cache: HashMap::new(),
            curr_fn: None,
            this_ptr: None,
            curr_symbel_tbl: HashMap::new(),
            curr_class_name: String::new(),
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
                TokenType::This => self.this_ptr.unwrap().as_basic_value_enum(),
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
                let ptr = self.get_var_ptr(&var_name);
                self.builder.build_load(ptr, &var_name)
            }
            Expr::ArrElem(var_name, indexing) => {
                let offset = self.expr_codegen(indexing);
                let arr_ptr_ptr = self.get_var_ptr(&var_name);
                // maybe need to cast here (from T* to T**)
                let arr_ptr = self.builder.build_load(arr_ptr_ptr, "load_arr_ptr");
                let ptr = unsafe {
                    self.builder.build_gep(
                        arr_ptr.into_pointer_value(),
                        &[offset.into_int_value()],
                        "arr_elem_gep",
                    )
                };
                self.builder.build_load(ptr, "load_arr_elem")
            }
            Expr::SubroutineCall {
                class_type, // exist if its a static function or a constructor
                obj_name,
                routine_name,
                args,
            } => {
                if let Some(fn_name) =
                    self.check_and_get_function(class_type.clone(), &routine_name)
                {
                    if obj_name.is_some() {
                        panic!("This subroutine should be a static function, can not apply in a object.");
                    }
                    let args_val = args
                        .into_iter()
                        .map(|expr| self.expr_codegen(expr))
                        .collect::<Vec<_>>();

                    self.subroutine_call(fn_name, args_val.as_slice())
                } else {
                    // process instance's member function
                    let (this_ptr, class_name) = if let Some(name) = obj_name {
                        let this = self.get_var_ptr(&name);
                        let ty = this.get_type().get_element_type().into_struct_type();
                        let class_name = self.get_class_by_type(ty).unwrap();

                        (this, class_name)
                    } else {
                        if let Some(this) = self.this_ptr.clone() {
                            (this, self.curr_class_name.clone())
                        } else {
                            let class_name =
                                Self::get_class_type_name(class_type.as_ref().unwrap());
                            (
                                self.process_constructor(&class_name, &routine_name),
                                class_name,
                            )
                        }
                    };
                    let mut args_val = vec![this_ptr.as_basic_value_enum()];
                    args.into_iter()
                        .for_each(|expr| args_val.push(self.expr_codegen(expr)));

                    let fn_name = format!("class_{}_method_{}", class_name, routine_name);
                    self.subroutine_call(fn_name, args_val.as_slice())
                }
            }
        }
    }

    pub fn stmt_codegen(&mut self, stmt: Box<Stmt>) {
        match *stmt {
            Stmt::LetVar(var_name, expr) => {
                let rhs = self.expr_codegen(expr);
                let ptr = self.get_var_ptr(&var_name);
                self.builder.build_store(ptr, rhs);
            }
            Stmt::LetArr {
                arr_name,
                idx_expr,
                rhs,
            } => {
                let rhs = self.expr_codegen(rhs);
                let offset = self.expr_codegen(idx_expr);
                let arr_head_ptr = self.get_var_ptr(&arr_name);
                // maybe need to cast here (from T* to T**)
                let elem_ptr = self.builder.build_load(arr_head_ptr, "load_arr_ptr");
                let ptr = unsafe {
                    self.builder.build_gep(
                        elem_ptr.into_pointer_value(),
                        &[offset.into_int_value()],
                        "arr_elem_gep",
                    )
                };
                self.builder.build_store(ptr, rhs);
            }
            Stmt::IfStmt {
                cond,
                consequence,
                alternative,
            } => {
                let parent = self.curr_fn.unwrap();
                let comparison = self.expr_codegen(cond);

                let then_block = self.context.append_basic_block(parent, "consequence");
                let else_block = self.context.append_basic_block(parent, "alternative");
                let end_block = self.context.append_basic_block(parent, "end");

                self.builder.build_conditional_branch(
                    comparison.into_int_value(),
                    then_block,
                    else_block,
                );

                // then block
                self.builder.position_at_end(then_block);
                for stmt in consequence {
                    self.stmt_codegen(stmt);
                }
                self.builder.build_unconditional_branch(end_block);

                // else block
                self.builder.position_at_end(else_block);
                if let Some(stmts) = alternative {
                    for stmt in stmts {
                        self.stmt_codegen(stmt);
                    }
                }
                self.builder.build_unconditional_branch(end_block);

                // merge block
                self.builder.position_at_end(end_block);
            }
            Stmt::WhileStmt(cond, stmts) => {
                let parent = self.curr_fn.unwrap();
                // let cond_var = self.create_entry_block_alloca(ty, name)
                let cond_bb = self.context.append_basic_block(parent, "cond");
                let loop_bb = self.context.append_basic_block(parent, "loop_body");
                let end_bb = self.context.append_basic_block(parent, "end");

                self.builder.build_unconditional_branch(cond_bb);

                self.builder.position_at_end(cond_bb);
                let comparison = self.expr_codegen(cond);
                self.builder
                    .build_conditional_branch(comparison.into_int_value(), loop_bb, end_bb);

                // loop body
                self.builder.position_at_end(loop_bb);
                for stmt in stmts {
                    self.stmt_codegen(stmt);
                }
                self.builder.build_unconditional_branch(cond_bb);
                self.builder.position_at_end(end_bb);
            }
            Stmt::DoStmt(expr) => {
                match expr.as_ref() {
                    Expr::SubroutineCall {
                        class_type: _,
                        obj_name: _,
                        routine_name: _,
                        args: _,
                    } => {}
                    _ => panic!("Not a functional call in Do statement"),
                };
                println!("DoStmt codegen expr: {:?}", expr);
                self.expr_codegen(expr);
            }
            Stmt::ReturnStmt(ret) => {
                if let Some(expr) = ret {
                    let ret = self.expr_codegen(expr);
                    self.builder.build_return(Some(&ret));
                } else {
                    self.builder.build_return(None);
                }
            }
        };
    }

    fn subroutine_codegen(&mut self, subroutine: SubroutineDec) -> FunctionValue<'ctx> {
        let (fn_name, is_fn) = match subroutine.routine_type {
            RoutineType::Function => (
                format!(
                    "class_{}_fn_{}",
                    self.curr_class_name.as_str(),
                    subroutine.routine_name
                ),
                true,
            ),
            _ => (
                format!(
                    "class_{}_method_{}",
                    self.curr_class_name.as_str(),
                    subroutine.routine_name
                ),
                false,
            ),
        };

        let fn_val = self
            .module
            .get_function(&fn_name)
            .unwrap_or_else(|| panic!("subroutine_codegen: Function {} doesn't exist", fn_name));
        self.curr_fn = Some(fn_val);

        // build function entry basic block
        let entry = self.context.append_basic_block(fn_val, "entry");
        self.builder.position_at_end(entry);

        // build arg variables ptr into symbel table
        let mut arg_iter = fn_val.get_param_iter();
        if is_fn {
            self.this_ptr = None;
        } else {
            // member function's first arg is 'this' ptr
            let this_val = arg_iter.next().unwrap();
            let this_ty = this_val.get_type();
            let this_ptr = self.create_entry_block_alloca(this_ty, "this_ptr");
            self.builder.build_store(this_ptr, this_val);
            self.curr_symbel_tbl.insert("this".to_string(), this_ptr);
            self.this_ptr.replace(this_val.into_pointer_value());
        }

        for (idx, arg) in arg_iter.enumerate() {
            let (class_ty, arg_name) = &subroutine.args[idx];
            let ty = self.convert_type(class_ty);
            if self.curr_symbel_tbl.contains_key(arg_name) {
                panic!("subroutine_codegen: variable {} alreadly exist.", arg_name);
            }
            let alloca = self.create_entry_block_alloca(ty, arg_name);
            self.builder.build_store(alloca, arg);
            self.curr_symbel_tbl.insert(arg_name.clone(), alloca);
        }

        // process subroutine local variables
        for (class_ty, names) in subroutine.body.var_decl.iter() {
            let ty = self.convert_type(class_ty);
            for name in names {
                if self.curr_symbel_tbl.contains_key(name) {
                    panic!("subroutine_codegen: variable {} alreadly exist.", name);
                }
                let alloca = self.create_entry_block_alloca(ty, name);
                self.curr_symbel_tbl.insert(name.clone(), alloca);
            }
        }

        for s in subroutine.body.stmt {
            self.stmt_codegen(s);
        }

        // pop out arg variables ptr in symbel table
        for (_, arg_name) in &subroutine.args {
            self.curr_symbel_tbl.remove(arg_name);
        }

        // pop out subroutine local variables
        for (_, names) in subroutine.body.var_decl.iter() {
            for local_var_name in names {
                self.curr_symbel_tbl.remove(local_var_name);
            }
        }

        if !is_fn {
            self.curr_symbel_tbl.remove("this");
            self.this_ptr = None;
            self.curr_fn = None;
        }

        fn_val
    }

    fn class_decl_codegen(&mut self, decl: ClassDec) {
        let ClassDec {
            name,
            memb_var,
            subroutines,
        } = decl;

        self.preprocess_class_decl(&name, &memb_var);
        for routine in subroutines {
            self.subroutine_codegen(routine);
        }
        self.class_member_idx.clear(); // remove member variables idx map because these variables can't be accessd by name directly anymore (only by getter/setter)
    }
}

/* ################################### Helper functions ########################################## */
impl<'a, 'ctx> IRCompiler<'a, 'ctx> {
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

    fn get_class_by_type(&self, ty: StructType<'ctx>) -> Option<String> {
        for (k, v) in self.type_cache.iter() {
            if v.clone() == ty.as_basic_type_enum().clone() {
                return Some(k.clone());
            }
        }
        None
    }

    fn create_subroutine_fn_type(
        &mut self,
        class_name: &str,
        this_type: Option<StructType<'ctx>>,
        subroutine: &SubroutineDec,
    ) -> FunctionValue<'ctx> {
        let fn_name;
        let mut args_ty = vec![];
        let has_this;
        if let Some(this) = this_type {
            // is member funcion, the first arg will always be 'this' ptr
            args_ty.push(this.ptr_type(AddressSpace::Generic).as_basic_type_enum());
            fn_name = format!("class_{}_method_{}", class_name, subroutine.routine_name);
            has_this = true;
        } else {
            fn_name = format!("class_{}_fn_{}", class_name, subroutine.routine_name);
            has_this = false;
        }
        subroutine
            .args
            .iter()
            .for_each(|(ty, _)| args_ty.push(self.convert_type(ty)));

        let fn_type = match &subroutine.ret_type {
            ClassType::Void => self.context.void_type().fn_type(args_ty.as_slice(), false),
            ty => self.convert_type(ty).fn_type(args_ty.as_slice(), false),
        };
        let fn_val = self.module.add_function(fn_name.as_str(), fn_type, None);

        let mut iter = fn_val.get_param_iter();
        if has_this {
            iter.next().unwrap().set_name("this");
        }
        for (idx, arg) in iter.enumerate() {
            let (_, ref name) = subroutine.args[idx];
            arg.set_name(name.as_str());
        }

        fn_val
    }

    fn create_new_class(&mut self, name: String, members: &Vec<ClassVarDec>) -> StructType<'ctx> {
        if let Some(exist) = self.type_cache.get(&name) {
            panic!("{} alreadly exist. {:?}", name, exist);
        }

        let mut mems_ty = vec![];
        // let mut idx_cnt = 0;
        for ClassVarDec { field, ty, vars } in members.iter() {
            let ty = self.convert_type(ty);
            if *field {
                // member variables
                vars.iter().for_each(|_| mems_ty.push(ty));
            } else {
                // TODO: global variable name mangling by its class
                for var_name in vars {
                    let glo_var = self
                        .module
                        .add_global(ty, Some(AddressSpace::Global), var_name);
                    self.curr_symbel_tbl
                        .insert(var_name.clone(), glo_var.as_pointer_value());
                }
            }
        }

        let new_type = self.context.struct_type(&mems_ty, false);
        self.type_cache.insert(name, new_type.as_basic_type_enum());
        new_type
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

    // hack for non class name static function call
    fn check_and_get_function(
        &self,
        class_type: Option<ClassType>,
        fn_name: &str,
    ) -> Option<String> {
        let fn_name = if let Some(class) = class_type {
            let class_name = match class {
                ClassType::Class(class_name) => class_name,
                _ => panic!("Not a class"),
            };
            format!("class_{}_fn_{}", class_name, fn_name)
        } else {
            // process static function call with implicit "this" Class name
            format!("class_{}_fn_{}", self.curr_class_name, fn_name)
        };
        if let Some(_) = self.module.get_function(&fn_name) {
            Some(fn_name)
        } else {
            None
        }
    }

    fn get_class_type_name(class_type: &ClassType) -> String {
        match class_type {
            ClassType::Class(class_name) => class_name.clone(),
            _ => panic!("Not a class"),
        }
    }

    fn process_constructor(&mut self, class_name: &str, fn_name: &str) -> PointerValue<'ctx> {
        let fn_name = format!("class_{}_method_{}", class_name, fn_name);
        let fn_val = self.module.get_function(&fn_name).unwrap();
        fn_val.get_first_param().unwrap().into_pointer_value()
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
                None => self
                    .context
                    .bool_type()
                    .const_int(0, false)
                    .as_basic_value_enum(), // hack: return 0 if return type is void
            }
        } else {
            panic!("Function {} not been decleared", fn_name);
        }
    }

    fn get_var_ptr(&mut self, var_name: &String) -> PointerValue<'ctx> {
        if let Some(&memb_idx) = self.class_member_idx.get(var_name) {
            let this = self
                .this_ptr
                .unwrap_or_else(|| panic!("derefence 'this' ptr in non member function"));
            // println!("gep debug: {:?}", this);
            self.builder
                .build_struct_gep(this, memb_idx, "memb_var_ptr")
                .unwrap()
        } else {
            if let Some(ptr) = self.curr_symbel_tbl.get(var_name) {
                ptr.clone()
            } else {
                panic!("expr_codgen: can not find {} in symbel teble", var_name);
            }
        }
    }

    fn predeclare_classes(&mut self, classes: &Vec<ClassDec>) {
        for ClassDec {
            name,
            memb_var,
            subroutines,
        } in classes
        {
            // declare class's struct type, static variables, member variables index
            let cl_ty = self.create_new_class(name.clone(), memb_var);

            // declare static/member functions
            for subroutine in subroutines {
                match subroutine.routine_type {
                    RoutineType::Function => self.create_subroutine_fn_type(name, None, subroutine),
                    RoutineType::Method => {
                        self.create_subroutine_fn_type(name, Some(cl_ty), subroutine)
                    }
                    RoutineType::Constructor => {
                        self.create_subroutine_fn_type(name, Some(cl_ty), subroutine)
                    }
                };
            }
        }
    }

    fn preprocess_class_decl(&mut self, name: &str, memb_var: &Vec<ClassVarDec>) {
        self.curr_class_name = name.to_string();
        let mut idx_cnt = 0;
        for ClassVarDec { field, ty: _, vars } in memb_var.iter() {
            if *field {
                // only consider member variables
                for var_name in vars {
                    self.class_member_idx.insert(var_name.clone(), idx_cnt);
                    idx_cnt += 1;
                }
            }
        }
    }
}

fn main() {
    let mut class_decls = vec![];
    for class_file in std::env::args().skip(1) {
        let ast = jack_compiler::parse_class_decl(&class_file);
        // println!("{:?}\n", ast);
        class_decls.push(ast);
    }

    let context = Context::create();
    let module = context.create_module("jack");
    let builder = context.create_builder();
    let fpm = PassManager::create(&module);
    // fpm_add_passes(&fpm); // show not optimized IR for debug
    fpm.initialize();

    let mut ir = IRCompiler::new(&context, &builder, &fpm, &module);
    ir.predeclare_classes(&class_decls);
    for class in class_decls {
        ir.class_decl_codegen(class);
    }

    ir.module.print_to_stderr();
    println!("\nJIT execution\n");
    let ee = module
        .create_jit_execution_engine(inkwell::OptimizationLevel::None)
        .unwrap();
    let main_func =
        unsafe { ee.get_function::<unsafe extern "C" fn() -> i32>("class_Main_fn_main") };
    let compiled_fn = match main_func {
        Ok(f) => f,
        Err(err) => panic!("!> Error during execution: {:?}", err),
    };

    unsafe {
        // compiled_fn.call();
        println!("=> {}", compiled_fn.call());
    }
}

#[allow(dead_code)]
fn fpm_add_passes(fpm: &PassManager<FunctionValue>) {
    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();
    fpm.add_gvn_pass();
    fpm.add_cfg_simplification_pass();
    fpm.add_basic_alias_analysis_pass();
    fpm.add_promote_memory_to_register_pass();
    fpm.add_reassociate_pass();
}
