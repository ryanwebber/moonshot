use std::collections::HashMap;

use crate::ast;
use crate::ir;

pub struct Compiler {}

pub struct PackagingError {
    pub msg: String,
}

pub struct CompilerError {
    pub msg: String,
}

pub struct ExternalHeader<'a> {
    pub header: &'a ModuleHeader,
    pub imported_path: &'a str,
}

pub struct ProcedureDefinition {
    pub body: ProcedureBody,
    pub layout: ProcedureLayout,
    pub prototype: ProcedurePrototype,
}

pub struct ProcedureBody {
    pub instructions: Vec<ir::Instruction>,
}

pub struct ProcedureLayout {}

pub struct ProcedurePrototype {
    pub name: String,
    pub signature: String,
    pub description: String,
}

impl std::fmt::Display for ProcedurePrototype {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.description)
    }
}

pub struct ImportMap<'a> {
    pub external_headers: &'a [ExternalHeader<'a>],
    pub local_headers: &'a [&'a ModuleHeader],
    pub system_headers: &'a [&'a ModuleHeader],
}

pub struct ModuleHeader {}

pub struct ModuleCompilation {
    pub module_name: String,
    pub procedures: Vec<ProcedureDefinition>,
}

pub struct ProgramCompilation<'a> {
    pub modules: &'a [ModuleCompilation],
}

struct RegistorAllocator {
    next_register: usize,
    lookup_table: HashMap<String, usize>,
}

fn try_compile_expr(
    expr: &ast::Expression,
    reg_allocator: &mut RegistorAllocator,
) -> Result<ir::RVal, CompilerError> {
    match expr {
        ast::Expression::BinOpExpression { lhs, op, rhs } => {
            let lhs_ir = try_compile_expr(&*lhs, reg_allocator)?;
            let rhs_ir = try_compile_expr(&*rhs, reg_allocator)?;
            match op {
                ast::Operator::Addition => Ok(ir::RVal::Add(ir::AddExpr {
                    mode: ir::DataMode::SP1,
                    lhs: Box::new(lhs_ir),
                    rhs: Box::new(rhs_ir),
                })),
            }
        }
        ast::Expression::NumberLiteralExpression { value } => {
            // TODO: Support different binary types
            let value: f32 = value.parse().map_err(|e| CompilerError {
                msg: format!("Unexpected number format: {}", e),
            })?;

            Ok(ir::RVal::Const(ir::ConstExpr {
                value: ir::ConstValue::Float {
                    data: value,
                    precision: ir::Precision::Single,
                },
            }))
        }
        ast::Expression::VarAccess { path } => {
            // TODO: support proper variable lookups
            let path_component = path
                .first()
                .expect("var access should have been parsed with at least one path component");

            let reg = reg_allocator.lookup(path_component).ok_or(CompilerError {
                msg: format!("Use of undeclared variable '{}'", path_component),
            })?;

            Ok(ir::RVal::Reg(ir::RegExpr {
                reg,
                mode: ir::DataMode::SP1,
            }))
        }
    }
}

fn try_compile_proc<'a>(proc: &ast::Procedure<'a>) -> Result<ProcedureBody, CompilerError> {
    let mut instructions: Vec<ir::Instruction> = Vec::new();
    let mut reg_allocator = RegistorAllocator::new();

    for statement in &proc.block.statements {
        match statement {
            ast::Statement::Definition {
                expression, name, ..
            } => {
                let reg = reg_allocator
                    .try_allocate_unused(name)
                    .map_err(|_| CompilerError {
                        msg: format!("Variable '{}' already defined", name),
                    })?;

                let dest_reg_expr = ir::RegExpr {
                    reg,
                    mode: ir::DataMode::SP1,
                };

                let expr_ir = try_compile_expr(expression, &mut reg_allocator)?;
                let set_inst = ir::Instruction::Set(ir::LVal::Reg(dest_reg_expr), expr_ir);
                instructions.push(set_inst);
            }
        }
    }

    let body = ProcedureBody { instructions };

    Ok(body)
}

impl Compiler {
    pub fn check_and_resolve_imports<'a>(_module: &ast::Module<'a>) {
        todo!()
    }

    pub fn check_and_build_header<'a>(
        _module: &ast::Module<'a>,
    ) -> Result<ModuleHeader, CompilerError> {
        todo!()
    }

    pub fn try_compile<'a>(
        module: &ast::Module<'a>,
        _import_map: &ImportMap<'_>,
    ) -> Result<ModuleCompilation, CompilerError> {
        let procedures: Result<Vec<_>, CompilerError> = module
            .procs
            .iter()
            .map(|proc| {
                let body = try_compile_proc(proc)?;
                let layout = ProcedureLayout {};
                let prototype = ProcedurePrototype::from(proc);

                Ok(ProcedureDefinition {
                    body,
                    layout,
                    prototype,
                })
            })
            .collect();

        Ok(ModuleCompilation {
            module_name: String::from(module.name),
            procedures: procedures?,
        })
    }

    pub fn package_modules<'a>(
        modules: &'a [ModuleCompilation],
    ) -> Result<ProgramCompilation<'a>, PackagingError> {
        Ok(ProgramCompilation { modules: modules })
    }
}

impl ProcedurePrototype {
    pub fn from(proc: &ast::Procedure) -> ProcedurePrototype {
        let signature = {
            let mut s = String::from(proc.name);
            s += " (";
            for elem in &proc.input_defn.elements {
                s += &format!("{}:", elem.name);
            }
            s + ")"
        };

        let description = {
            let mut s = String::from("proc ");
            s += proc.name;
            s += " (";
            for (i, elem) in proc.input_defn.elements.iter().enumerate() {
                s += &format!("{}: {}", elem.name, elem.data_type.name);
                if i < proc.input_defn.elements.len() - 1 {
                    s += ", ";
                }
            }
            s += ") -> (";
            for (i, elem) in proc.return_defn.elements.iter().enumerate() {
                s += &format!("{}: {}", elem.name, elem.data_type.name);
                if i < proc.return_defn.elements.len() - 1 {
                    s += ", ";
                }
            }
            s + ")"
        };

        ProcedurePrototype {
            name: String::from(proc.name),
            signature,
            description,
        }
    }
}

impl<'a> ImportMap<'a> {
    pub fn empty() -> ImportMap<'a> {
        ImportMap {
            external_headers: &[],
            local_headers: &[],
            system_headers: &[],
        }
    }
}

impl RegistorAllocator {
    fn new() -> Self {
        RegistorAllocator {
            next_register: 0,
            lookup_table: HashMap::new(),
        }
    }

    fn try_allocate_unused(&mut self, identifier: &str) -> Result<ir::Register, ()> {
        if self.lookup_table.contains_key(identifier) {
            Err(())
        } else {
            let reg = self.next_register;
            self.lookup_table.insert(identifier.to_string(), reg);
            self.next_register += 1;
            Ok(ir::Register(reg))
        }
    }

    fn lookup(&self, identifier: &str) -> Option<ir::Register> {
        self.lookup_table.get(identifier).map(|r| ir::Register(*r))
    }
}
