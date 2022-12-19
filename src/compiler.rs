use std::collections::HashMap;

use crate::ast;
use crate::ir;
use crate::utils;
use crate::utils::Counter;

pub struct Compiler {}

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

pub struct ProcedureDefinition {
    pub body: ProcedureBody,
    pub layout: ProcedureLayout,
    pub prototype: ProcedurePrototype,
}

pub struct ProcedureBody {
    pub instructions: Vec<ir::Instruction>,
}

pub struct ProcedureLayout {
    pub placeholders: Vec<Placeholder>,
}

#[derive(Clone, Debug, PartialEq, Hash)]
pub struct ProcedurePrototype {
    pub name: String,
    pub signature: String,
}

struct RegistorAllocator {
    counter: utils::Counter<ir::Id>,
    lookup_table: HashMap<String, ir::Register>,
    placeholders: Vec<Placeholder>,
}

pub struct Placeholder {
    pub id: ir::Id,
    pub words: usize,
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
        ast::Expression::BinOpExpression { lhs, op, rhs } => {
            let lhs_ir = try_compile_expr(&*lhs, register_allocator, constant_pool)?;
            let rhs_ir = try_compile_expr(&*rhs, register_allocator, constant_pool)?;
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
        ast::Expression::VarAccess { path } => {
            // TODO: support proper variable lookups
            let path_component = path
                .first()
                .expect("var access should have been parsed with at least one path component");

            let reg = register_allocator
                .lookup(path_component)
                .ok_or(CompilerError {
                    msg: format!("Use of undeclared variable '{}'", path_component),
                })?;

            Ok(ir::RVal::Reg(ir::RegExpr {
                reg,
                mode: ir::DataMode::SP1,
            }))
        }
    }
}

fn try_compile_proc<'a>(
    proc: &ast::Procedure<'a>,
    register_allocator: &mut RegistorAllocator,
    constant_pool: &mut ConstantPool,
) -> Result<ProcedureBody, CompilerError> {
    let mut instructions: Vec<ir::Instruction> = Vec::new();

    for statement in &proc.block.statements {
        match statement {
            ast::Statement::Definition {
                expression, name, ..
            } => {
                let reg =
                    register_allocator
                        .try_allocate_unused(name)
                        .map_err(|_| CompilerError {
                            msg: format!("Variable '{}' already defined", name),
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

impl Compiler {
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
            header
                .procedures
                .insert(prototype.signature.clone(), (id, prototype));
        }

        Ok(header)
    }

    pub fn check_and_compile<'a>(
        module: &ast::Module<'a>,
        header: ModuleHeader,
    ) -> Result<ModuleCompilation, CompilerError> {
        let mut constant_pool = ConstantPool::new();
        let procedures: Result<Vec<_>, CompilerError> = module
            .procs
            .iter()
            .map(|proc| {
                let mut register_allocator = RegistorAllocator::new(Counter::new(ir::Id(0)));
                let body = try_compile_proc(proc, &mut register_allocator, &mut constant_pool)?;
                let prototype = ProcedurePrototype::from(proc);
                let layout = ProcedureLayout {
                    placeholders: register_allocator.placeholders,
                };

                Ok(ProcedureDefinition {
                    body,
                    layout,
                    prototype,
                })
            })
            .collect();

        Ok(ModuleCompilation {
            constants: constant_pool.mapping,
            header,
            module_name: String::from(module.name),
            procedures: procedures?,
        })
    }

    pub fn package_modules(
        modules: Vec<ModuleCompilation>,
    ) -> Result<ProgramCompilation, PackagingError> {
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
                        msg: format!(
                            "A procedure named 'main' must be specified in an anonymous module."
                        ),
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
                let mut procs: Vec<(ir::Id, ProcedureDefinition)> =
                    Vec::with_capacity(module.procedures.len());

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
}

impl ConstantPool {
    fn new() -> Self {
        Self {
            counter: utils::Counter::new(ir::Id(0)),
            mapping: HashMap::new(),
        }
    }

    fn get_or_allocate(&mut self, value: ir::ConstValue) -> ir::Id {
        *self
            .mapping
            .entry(value)
            .or_insert_with(|| self.counter.next().0)
    }
}

impl ProcedurePrototype {
    pub fn from(proc: &ast::Procedure) -> ProcedurePrototype {
        let name = proc.name.to_string();
        let signature = {
            let mut s = String::from(proc.name);
            s += " (";
            for elem in &proc.input_defn.elements {
                s += &format!("{}:", elem.name);
            }
            s + ")"
        };

        ProcedurePrototype { name, signature }
    }
}

impl std::fmt::Display for ProcedurePrototype {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.signature)
    }
}

impl RegistorAllocator {
    fn new(counter: utils::Counter<ir::Id>) -> Self {
        RegistorAllocator {
            counter,
            lookup_table: HashMap::new(),
            placeholders: Vec::new(),
        }
    }

    fn try_allocate_unused(&mut self, identifier: &str) -> Result<ir::Register, ()> {
        if self.lookup_table.contains_key(identifier) {
            Err(())
        } else {
            // TODO: Support registers of different size
            let id = self.counter.next().0;
            let reg = ir::Register { id, offset: 0 };

            self.lookup_table.insert(identifier.to_string(), reg);
            self.placeholders.push(Placeholder { id, words: 1 });

            Ok(reg)
        }
    }

    fn lookup(&self, identifier: &str) -> Option<ir::Register> {
        self.lookup_table.get(identifier).map(|r| *r)
    }
}
