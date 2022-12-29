use std::collections::HashMap;
use std::hash::Hash;

use crate::ast;
use crate::ir;
use crate::utils;

pub struct PackagingError {
    pub msg: String,
}

pub struct CompilerError {
    pub msg: String,
}

struct ConstantPool {
    counter: utils::Counter<ir::Id>,
    mapping: HashMap<ir::ConstValue, ir::Id>,
}

pub struct ModuleHeader {
    procedures: HashMap<String, (ir::Id, ProcedurePrototype)>,
}

pub struct ModuleCompilation {
    pub constants: HashMap<ir::ConstValue, ir::Id>,
    pub header: ModuleHeader,
    pub module_name: String,
    pub procedures: Vec<ProcedureDefinition>,
}

#[derive(Clone, Debug, PartialEq, Hash)]
pub struct Placeholder {
    pub offset: usize,
    pub words: usize,
}

pub struct ProcedureBody {
    pub instructions: Vec<ir::Instruction>,
}

pub struct ProcedureDefinition {
    pub body: ProcedureBody,
    pub layout: ProcedureLayout,
    pub prototype: ProcedurePrototype,
}

pub struct ProcedureLayout {
    pub placeholders: Vec<Placeholder>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ProcedurePrototype {
    pub name: String,
    pub signature: String,
    pub parameters: Vec<StaticDeclaration>,
    pub returns: Vec<StaticDeclaration>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct StaticDeclaration {
    pub placeholder: Placeholder,
}

struct RegistorAllocator {
    id: ir::Id,
    counter: utils::Counter<usize>,
    lookup_table: HashMap<String, ir::Register>,
    placeholders: Vec<Placeholder>,
}

pub struct ProgramCompilation {
    pub constants: HashMap<ir::Id, ir::ConstValue>,
    pub main_proc: ir::Id,
    pub procedures: Vec<(ir::Id, ProcedureDefinition)>,
}

fn try_compile_expr(
    expr: &ast::Expression,
    register_allocator: &mut RegistorAllocator,
    constant_pool: &mut ConstantPool,
) -> Result<ir::RVal, CompilerError> {
    match expr {
        ast::Expression::CallExpression {
            function: ast::FunctionSignature::Symbolic(operator),
            arguments,
        } => match operator {
            ast::Operator::Addition => {
                if arguments.len() != 2 {
                    todo!("Function param check failure")
                } else {
                    let lhs_ir = try_compile_expr(&arguments[0], register_allocator, constant_pool)?;
                    let rhs_ir = try_compile_expr(&arguments[1], register_allocator, constant_pool)?;
                    Ok(ir::RVal::Add(ir::AddExpr {
                        mode: ir::DataMode::SP1,
                        lhs: Box::new(lhs_ir),
                        rhs: Box::new(rhs_ir),
                    }))
                }
            }
        },
        ast::Expression::CallExpression {
            function: ast::FunctionSignature::Named(_signature),
            ..
        } => {
            todo!("Function call")
        }
        ast::Expression::Dereference { name } => {
            let reg = register_allocator.lookup(name).ok_or(CompilerError {
                msg: format!("Use of undeclared variable '{}'", name),
            })?;

            Ok(ir::RVal::Reg(ir::RegExpr {
                reg,
                mode: ir::DataMode::SP1,
            }))
        }
        ast::Expression::NumberLiteralExpression { value } => {
            // TODO: Support different binary types
            let value: i32 = value.parse().map_err(|e| CompilerError {
                msg: format!("Unexpected number format: {}", e),
            })?;

            let const_value = ir::ConstValue::Float {
                base: value,
                exponent: 0,
                precision: ir::Precision::Single,
            };

            let const_id = constant_pool.get_or_allocate(const_value);

            Ok(ir::RVal::Const(ir::ConstExpr { id: const_id }))
        }
    }
}

fn try_compile_proc_body<'a>(
    proc: &ast::Procedure<'a>,
    register_allocator: &mut RegistorAllocator,
    constant_pool: &mut ConstantPool,
) -> Result<ProcedureBody, CompilerError> {
    let mut instructions: Vec<ir::Instruction> = Vec::new();

    for statement in &proc.block.statements {
        match statement {
            ast::Statement::Definition { declaration, expression } => {
                let reg = register_allocator
                    .try_allocate_unused(declaration.name)
                    .map_err(|_| CompilerError {
                        msg: format!("Variable '{}' already defined", declaration.name),
                    })?;

                let dest_reg_expr = ir::RegExpr {
                    reg,
                    mode: ir::DataMode::SP1,
                };

                let expr_ir = try_compile_expr(expression, register_allocator, constant_pool)?;
                let set_inst = ir::Instruction::Set(ir::LVal::Reg(dest_reg_expr), expr_ir);
                instructions.push(set_inst);
            }
            ast::Statement::Assignment { var_name, expression } => {
                let reg = register_allocator.lookup(var_name).ok_or(CompilerError {
                    msg: format!("Use of undeclared variable '{}'", var_name),
                })?;

                let dest_reg_expr = ir::RegExpr {
                    reg,
                    mode: ir::DataMode::SP1,
                };

                let expr_ir = try_compile_expr(expression, register_allocator, constant_pool)?;
                let set_inst = ir::Instruction::Set(ir::LVal::Reg(dest_reg_expr), expr_ir);
                instructions.push(set_inst);
            }
        }
    }

    let body = ProcedureBody { instructions };

    Ok(body)
}

fn try_compile_proc<'a>(
    proc: &ast::Procedure<'a>,
    header: &ModuleHeader,
    constant_pool: &mut ConstantPool,
) -> Result<ProcedureDefinition, CompilerError> {
    let prototype = ProcedurePrototype::from(proc);
    let id = header
        .procedures
        .get(&prototype.signature)
        .expect("Unable to find id for procedure in the containing module header")
        .0;

    let mut register_allocator = RegistorAllocator::new(id);

    // Pre-allocate parameters at the top of the workspace
    for p in &proc.parameter_list.declarations {
        _ = register_allocator.try_allocate_unused(p.name);
    }

    // Pre-allocate return slots at the top of the workspace
    for r in &proc.return_list.declarations {
        _ = register_allocator.try_allocate_unused(r.name);
    }

    let body = try_compile_proc_body(proc, &mut register_allocator, constant_pool)?;
    let layout = ProcedureLayout {
        placeholders: register_allocator.placeholders,
    };

    Ok(ProcedureDefinition { body, layout, prototype })
}

pub fn check_and_resolve_imports<'a>(_module: &ast::Module<'a>) {
    todo!()
}

pub fn check_and_build_header<'a>(
    module: &ast::Module<'a>,
    ids: &mut dyn utils::IdentifierAllocator<ir::Id>,
) -> Result<ModuleHeader, CompilerError> {
    let mut header = ModuleHeader {
        procedures: HashMap::new(),
    };

    for proc in &module.procs {
        let prototype = ProcedurePrototype::from(proc);
        let id = ids.generate_id();
        header.procedures.insert(prototype.signature.clone(), (id, prototype));
    }

    Ok(header)
}

pub fn check_and_compile<'a>(module: &ast::Module<'a>, header: ModuleHeader) -> Result<ModuleCompilation, CompilerError> {
    let mut constant_pool = ConstantPool::new();
    let procedures: Result<Vec<_>, CompilerError> = module
        .procs
        .iter()
        .map(|proc| try_compile_proc(proc, &header, &mut constant_pool))
        .collect();

    Ok(ModuleCompilation {
        constants: constant_pool.mapping,
        header,
        module_name: String::from(module.name),
        procedures: procedures?,
    })
}

pub fn package_modules(modules: Vec<ModuleCompilation>) -> Result<ProgramCompilation, PackagingError> {
    // Find the main proc
    let main_proc = {
        let candidates: Vec<_> = modules
            .iter()
            .filter(|m| m.module_name == "_")
            .flat_map(|m| {
                m.procedures.iter().filter_map(|p| {
                    if p.prototype.name == "main" {
                        m.header.procedures.get(&p.prototype.signature).map(|x| x.0)
                    } else {
                        None
                    }
                })
            })
            .collect();

        match candidates[..] {
            [main] => main,
            [] => {
                return Err(PackagingError {
                    msg: format!("A procedure named 'main' must be specified in an anonymous module."),
                })
            }
            [..] => {
                return Err(PackagingError {
                    msg: format!("Multiple 'main' procedure candidates found."),
                })
            }
        }
    };

    // Copy constants
    let constants: HashMap<ir::Id, ir::ConstValue> = modules
        .iter()
        .flat_map(|m| m.constants.iter())
        .map(|(a, b)| (b.clone(), *a))
        .collect();

    // Copy procedures
    let procedures: Vec<_> = modules
        .into_iter()
        .flat_map(|module| {
            let mut procs: Vec<(ir::Id, ProcedureDefinition)> = Vec::with_capacity(module.procedures.len());

            for proc in module.procedures {
                let id = module
                    .header
                    .procedures
                    .get(&proc.prototype.signature)
                    .expect("Unable to find id for procedure in the containing module header")
                    .0;

                procs.push((id, proc))
            }

            procs
        })
        .collect();

    let compilation = ProgramCompilation {
        constants,
        main_proc,
        procedures,
    };

    Ok(compilation)
}

impl ConstantPool {
    fn new() -> Self {
        Self {
            counter: utils::Counter::new(ir::Id(0)),
            mapping: HashMap::new(),
        }
    }

    fn get_or_allocate(&mut self, value: ir::ConstValue) -> ir::Id {
        *self.mapping.entry(value).or_insert_with(|| self.counter.next().0)
    }
}

impl ProcedurePrototype {
    pub fn from(proc: &ast::Procedure) -> ProcedurePrototype {
        let name = proc.name.to_string();
        let signature = {
            let mut s = String::from(proc.name);
            s += " (";
            for p in &proc.parameter_list.declarations {
                s += &format!("{}:", p.name);
            }
            s + ")"
        };

        let parameters: Vec<_> = proc
            .parameter_list
            .declarations
            .iter()
            .enumerate()
            .map(|(offset, _)| StaticDeclaration {
                placeholder: Placeholder {
                    offset: offset,
                    words: 1,
                },
            })
            .collect();

        let returns: Vec<_> = proc
            .return_list
            .declarations
            .iter()
            .enumerate()
            .map(|(offset, _)| StaticDeclaration {
                placeholder: Placeholder {
                    offset: offset + parameters.len(),
                    words: 1,
                },
            })
            .collect();

        ProcedurePrototype {
            name,
            signature,
            parameters,
            returns,
        }
    }
}

impl std::fmt::Display for ProcedurePrototype {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.signature)
    }
}

impl RegistorAllocator {
    fn new(id: ir::Id) -> Self {
        RegistorAllocator {
            id,
            counter: utils::Counter::new(0),
            lookup_table: HashMap::new(),
            placeholders: Vec::new(),
        }
    }

    fn try_allocate_unused(&mut self, identifier: &str) -> Result<ir::Register, ()> {
        if self.lookup_table.contains_key(identifier) {
            Err(())
        } else {
            // TODO: Support registers of different size
            let offset = self.counter.next().0;
            let reg = ir::Register { id: self.id, offset };

            self.lookup_table.insert(identifier.to_string(), reg);
            self.placeholders.push(Placeholder { offset, words: 1 });

            Ok(reg)
        }
    }

    fn lookup(&self, identifier: &str) -> Option<ir::Register> {
        self.lookup_table.get(identifier).map(|r| *r)
    }
}
