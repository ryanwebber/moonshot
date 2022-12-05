use crate::ast;
use crate::ir;

pub struct Compiler {}

#[derive(Debug)]
pub struct CompilerErr<'a> {
    _dummy: &'a (),
    msg: String,
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

struct RegistorAllocator {
    next_register: usize,
}

fn compile_expr<'a>(
    expr: &ast::Expression,
    reg_allocator: &mut RegistorAllocator,
) -> Result<ir::RVal, CompilerErr<'a>> {
    match expr {
        ast::Expression::BinOpExpression { lhs, op, rhs } => {
            let lhs_ir = compile_expr(&*lhs, reg_allocator)?;
            let rhs_ir = compile_expr(&*rhs, reg_allocator)?;
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
            let value: f32 = value.parse().map_err(|e| CompilerErr {
                _dummy: &(),
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
            let reg = reg_allocator.lookup(
                path.first()
                    .expect("var access should have at least one path compunent"),
            )?;

            Ok(ir::RVal::Reg(ir::RegExpr {
                reg,
                mode: ir::DataMode::SP1,
            }))
        }
    }
}

fn compile_proc<'a>(proc: &ast::Procedure<'a>) -> Result<ProcedureBody, CompilerErr<'a>> {
    let mut instructions: Vec<ir::Instruction> = Vec::new();
    let mut reg_allocator = RegistorAllocator::new();

    for statement in &proc.block.statements {
        match statement {
            ast::Statement::Definition { expression, .. } => {
                let dest_reg_expr = ir::RegExpr {
                    reg: reg_allocator.allocate(),
                    mode: ir::DataMode::SP1,
                };

                let expr_ir = compile_expr(expression, &mut reg_allocator)?;
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
    ) -> Result<ModuleHeader, CompilerErr<'a>> {
        todo!()
    }

    pub fn check_and_compile<'a>(
        module: &ast::Module<'a>,
        _import_map: &ImportMap<'_>,
    ) -> Result<ModuleCompilation, CompilerErr<'a>> {
        let procedures: Result<Vec<_>, CompilerErr<'a>> = module
            .procs
            .iter()
            .map(|proc| {
                let body = compile_proc(proc)?;
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
            signature: signature,
            description: description,
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
        RegistorAllocator { next_register: 0 }
    }

    fn allocate(&mut self) -> ir::Register {
        let register = ir::Register(self.next_register);
        self.next_register += 1;
        register
    }

    fn lookup<'a>(&self, _identifier: &str) -> Result<ir::Register, CompilerErr<'a>> {
        Ok(ir::Register(1337))
    }
}
