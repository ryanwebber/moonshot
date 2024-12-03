use std::{borrow::Cow, collections::HashMap, rc::Rc};

use anyhow::Ok;

use crate::{
    agc::{Address, Instruction},
    ast::{AssemblyOperand, BinaryOperator, Block, Directive, Expression, Statement, ValueDeclaration, ValueIdentifier},
    generator::{Archive, Generator, Label, Location, Output, Slot},
    loader::{ModuleIdentifier, Program, SourceReference},
    parser::moonshot::ProgramFragmentParser,
    types::Numeric,
};

const STD_LIB_SOURCES: &[(&'static str, &'static str)] = &[
    ("$tests", include_str!("stdlib/tests.moon")),
    // TODO: More standard library sources
];

pub struct Compiler {}

impl Compiler {
    pub fn new() -> Self {
        Self {}
    }

    pub fn compile(&self, program: Program) -> anyhow::Result<Output> {
        State::new().compile(program)
    }
}

struct State {
    generator: Generator,
    constant_pool: ConstantPool,
}

impl State {
    fn new() -> Self {
        Self {
            generator: Generator::new(),
            constant_pool: ConstantPool::new(),
        }
    }

    pub fn compile(mut self, program: Program) -> anyhow::Result<Output> {
        let mut world = World::new();
        let mut label_generator = LabelGenerator::new();

        // Emit some data slots for stdlib stuff
        self.generator.data_mut().enqueue_comment("DSKY REGISTERS AND FLAGS");
        self.generator.data_mut().reserve_word(Label::from_static("DSKYREG1"));
        self.generator.data_mut().reserve_word(Label::from_static("DSKYSGN1"));
        self.generator.data_mut().reserve_word(Label::from_static("DSKYREG2"));
        self.generator.data_mut().reserve_word(Label::from_static("DSKYSGN2"));
        self.generator.data_mut().reserve_word(Label::from_static("DSKYREG3"));
        self.generator.data_mut().reserve_word(Label::from_static("DSKYSGN3"));
        self.generator.data_mut().reserve_word(Label::from_static("DSKYPROG"));
        self.generator.data_mut().reserve_word(Label::from_static("DSKYNOUN"));
        self.generator.data_mut().reserve_word(Label::from_static("DSKYVERB"));
        self.generator.data_mut().reserve_word(Label::from_static("DSKYMASK"));

        // First pass: catalog all functions and top level declarations
        for (identifier, unit) in program.compilation_units.into_iter() {
            let mut functions = Vec::new();
            for directive in unit.fragment.directives.iter() {
                match directive {
                    Directive::Include { .. } => {}
                    Directive::UserProgram { .. } => {}
                    Directive::State {
                        name, parameters, body, ..
                    } => {
                        let label: Label = label_generator.generate('S');
                        let header = Header::create(label_generator.generate('W'), &parameters);
                        functions.push(Function {
                            name: name.clone(),
                            source: unit.reference.clone(),
                            entry_point: label.clone(),
                            body: body.clone(),
                            kind: FunctionKind::State,
                            header,
                        });
                    }
                    Directive::Subroutine { name, parameters, body } => {
                        let label = label_generator.generate('F');
                        let header = Header::create(label_generator.generate('W'), &parameters);
                        functions.push(Function {
                            name: name.clone(),
                            source: unit.reference.clone(),
                            entry_point: label.clone(),
                            body: body.clone(),
                            kind: FunctionKind::Subroutine,
                            header,
                        });
                    }
                }
            }

            world.insert_module(identifier, Module::new(unit.aliases, functions));
        }

        // Second pass: compile all functions
        for (identifier, module) in world.iter_modules() {
            for function in module.iter_functions() {
                let comment = format!(
                    "{} :: {} ({})",
                    match function.kind {
                        FunctionKind::State => "STATE",
                        FunctionKind::Subroutine => "SUBROUTINE",
                    },
                    function.name,
                    function.source
                );

                self.generator
                    .code_mut()
                    .enqueue_line_break()
                    .enqueue_label(function.entry_point.clone())
                    .enqueue_comment(comment);

                let environment = Environment::wrapping(&world, identifier.clone());
                let mut workspace = Workspace::create(function.clone());

                self.compile_function(&environment, &mut workspace, &function)?;

                workspace.write(self.generator.data_mut());
            }
        }

        // Compile in the SYSMAIN entry point
        self.generator
            .code_mut()
            .enqueue_label(Label::from_static("SYSMAIN"))
            .enqueue_comment("SYSMAIN ENTRY POINT")
            .enqueue_line_break();

        // TODO: Instead of this, let's jump into some runtime code
        let main_function = world.modules.iter().find_map(|(_, module)| module.find_function("main"));
        self.generator.code_mut().append(if let Some(main) = main_function {
            Instruction::TC(Address::Relative {
                label: main.entry_point.clone(),
                offset: 0,
            })
        } else {
            Instruction::TC(Address::Relative {
                label: Label::from_static("SYSMAIN"),
                offset: 0,
            })
        });

        // Emit an exit instruction just to avoid falling through into constants
        self.generator
            .code_mut()
            .append(Instruction::TC(Address::Relative {
                label: Label::from_static("EXIT"),
                offset: 0,
            }))
            .with_comment("END OF FUNCTIONS")
            .with_label(Label::from_static("SYSEXIT"))
            .with_preceding_line_break();

        if !self.constant_pool.constants.is_empty() {
            self.generator
                .code_mut()
                .enqueue_line_break()
                .enqueue_comment("CONSTANT POOL")
                .enqueue_label(ConstantPool::LABEL);

            for constant in self.constant_pool.into_slots() {
                self.generator.code_mut().append(Instruction::DEC(constant));
            }
        }

        Ok(self.generator.into_output())
    }

    fn compile_function(
        &mut self,
        environment: &Environment,
        workspace: &mut Workspace,
        function: &Function,
    ) -> anyhow::Result<()> {
        // Store the Q register in the return slot so we can ret to it
        self.generator.copy_reg_q_to_slot(workspace.function.header.ret_addr_slot());

        for statement in function.body.statements.iter() {
            match statement {
                Statement::Definition(definition) => {
                    self.generator.code_mut().enqueue_comment(format!(
                        "{} := {}",
                        definition.declaration.identifier,
                        definition.expression.brief()
                    ));

                    let lhs = workspace.insert(definition.declaration.identifier.clone());
                    let rhs = self.compile_expression(environment, workspace, &definition.expression)?;
                    if let Some(rhs) = rhs {
                        self.generator.copy_slot_to_reg_a(rhs);
                        self.generator.copy_reg_a_to_slot(lhs);
                    } else {
                        anyhow::bail!("RHS expression does not produce a value: {}", definition.expression.brief());
                    }
                }
                Statement::Expression(expression) => {
                    self.compile_expression(environment, workspace, expression)?;
                }
                Statement::InlineAssembly(source) => {
                    let instruction = if let Some(operand) = &source.operand {
                        let address: Address = match operand {
                            AssemblyOperand::Label(label) => Label::from_static(label.clone()).into(),
                            AssemblyOperand::Reference(name) => workspace
                                .resolve(name)
                                .ok_or_else(|| anyhow::anyhow!("Variable reference not found: {}", name))?
                                .into(),
                        };

                        Instruction::from_str_with_address(source.instruction.as_str(), address)
                    } else {
                        Instruction::from_str(source.instruction.as_str())
                    };

                    let Some(instruction) = instruction else {
                        anyhow::bail!("Unknown assembly instruction: {}", source.instruction);
                    };

                    self.generator.code_mut().append(instruction);
                }
                Statement::Return(expression) => {
                    if let Some(expression) = expression {
                        self.generator
                            .code_mut()
                            .enqueue_comment(format!("RETURN {}", expression.brief()));
                        let ret_value = self.compile_expression(environment, workspace, expression)?;
                        if let Some(ret_value) = ret_value {
                            self.generator.copy_slot_to_reg_a(ret_value);
                            self.generator.copy_reg_a_to_slot(workspace.function.header.ret_value_slot());
                        } else {
                            anyhow::bail!("Return expression does not produce a value: {}", expression.brief());
                        }
                    }

                    // Put the return value back in the Q register and return
                    self.generator.code_mut().enqueue_comment("EARLY RETURN");
                    self.generator.copy_slot_to_reg_q(workspace.function.header.ret_addr_slot());
                    self.generator.code_mut().append(Instruction::RETURN);
                }
            }
        }

        // Put the return value back in the Q register and return
        self.generator.copy_slot_to_reg_q(workspace.function.header.ret_addr_slot());
        self.generator.code_mut().append(Instruction::RETURN);

        Ok(())
    }

    fn compile_expression(
        &mut self,
        environment: &Environment,
        workspace: &mut Workspace,
        expression: &Expression,
    ) -> anyhow::Result<Option<Slot>> {
        match expression {
            Expression::BinaryOperation { op, lhs, rhs } => {
                let lhs_slot = self.compile_expression(environment, workspace, lhs)?;
                let rhs_slot = self.compile_expression(environment, workspace, rhs)?;
                let lhs_slot = lhs_slot.ok_or_else(|| anyhow::anyhow!("LHS does not produce a value: {}", lhs.brief()))?;
                let rhs_slot = rhs_slot.ok_or_else(|| anyhow::anyhow!("RHS does not produce a value: {}", rhs.brief()))?;
                let temporary = workspace.allocate_temporary();
                match op {
                    BinaryOperator::Addition => {
                        // Copy RHS to TEMP, since this is most likely to be a no-op during optimization
                        self.generator.copy_slot_to_reg_a(rhs_slot);
                        self.generator.copy_reg_a_to_slot(temporary.clone());

                        // Copy LHS to A
                        self.generator.copy_slot_to_reg_a(lhs_slot);

                        // Add A and RHS together
                        self.generator.code_mut().append(Instruction::ADS(temporary.clone().into()));
                    }
                }

                Ok(Some(temporary))
            }
            Expression::FunctionCall { function, arguments } => {
                let resolved_function = match function {
                    ValueIdentifier::Implicit(function_name) => environment.current_module().find_function(&function_name),
                    ValueIdentifier::Namespaced(module_alias, function_name) => {
                        let module = environment
                            .find_included_module(module_alias)
                            .ok_or_else(|| anyhow::anyhow!("Module not found: {}", module_alias))?;

                        module.find_function(function_name)
                    }
                };

                let Some(resolved_function) = resolved_function else {
                    anyhow::bail!("Function not found: {}", function);
                };

                for (i, argument) in arguments.iter().enumerate() {
                    let lhs = resolved_function
                        .header
                        .resolve_parameter_slot(&argument.name)
                        .ok_or_else(|| {
                            anyhow::anyhow!(
                                "Parameter not found: {} in function {}",
                                argument.name,
                                resolved_function.name
                            )
                        })?;

                    self.generator
                        .code_mut()
                        .enqueue_comment(format!("ARGUMENT '{}'", argument.name));
                    let rhs = self
                        .compile_expression(environment, workspace, &argument.expression)?
                        .ok_or_else(|| anyhow::anyhow!("Argument {} does not produce a value", i))?;

                    self.generator.copy_slot_to_reg_a(rhs);
                    self.generator.copy_reg_a_to_slot(lhs);
                }

                self.generator
                    .code_mut()
                    .append(Instruction::TC(Address::Relative {
                        label: resolved_function.entry_point.clone(),
                        offset: 0,
                    }))
                    .with_comment(format!("CALL '{}'", resolved_function.name));

                Ok(Some(resolved_function.header.ret_value_slot()))
            }
            Expression::NumberLiteral(value) => {
                let slot = self.constant_pool.get_or_insert(value.clone());
                Ok(Some(slot))
            }
            Expression::VariableReference(identifier) => match identifier {
                ValueIdentifier::Implicit(name) => workspace
                    .resolve(name)
                    .ok_or_else(|| anyhow::anyhow!("Variable not found: {}", name))
                    .map(|s| Some(s)),
                ValueIdentifier::Namespaced(..) => unimplemented!("Support for namespaced identifiers"),
            },
        }
    }
}

struct World {
    stdlib: Stdlib,
    modules: HashMap<ModuleIdentifier, Module>,
}

impl World {
    fn new() -> Self {
        Self {
            stdlib: Stdlib::new(),
            modules: HashMap::new(),
        }
    }

    fn insert_module(&mut self, identifier: ModuleIdentifier, module: Module) {
        self.modules.insert(identifier, module);
    }

    fn iter_modules<'a>(&'a self) -> impl Iterator<Item = (ModuleIdentifier, &Module)> + 'a {
        let module_iter = self.modules.iter().map(|(id, module)| (id.clone(), module));
        let stdlib_iter = self
            .stdlib
            .modules
            .iter()
            .map(|(name, module)| (ModuleIdentifier::from(name), module));

        module_iter.chain(stdlib_iter)
    }
}

struct Module {
    aliases: HashMap<String, ModuleIdentifier>,
    functions: HashMap<String, Rc<Function>>,
}

impl Module {
    fn new(aliases: HashMap<String, ModuleIdentifier>, functions: impl IntoIterator<Item = Function>) -> Self {
        Self {
            aliases,
            functions: {
                let mut map = HashMap::new();
                for function in functions.into_iter() {
                    map.insert(function.name.clone(), Rc::new(function));
                }

                map
            },
        }
    }

    fn find_function(&self, name: &str) -> Option<&Function> {
        self.functions.get(name).map(|f| f.as_ref())
    }

    fn iter_functions<'a>(&'a self) -> impl Iterator<Item = Rc<Function>> + 'a {
        self.functions.values().cloned()
    }
}

struct Environment<'a> {
    world: &'a World,
    current_module: ModuleIdentifier,
}

impl<'a> Environment<'a> {
    fn wrapping(world: &'a World, current_module: ModuleIdentifier) -> Self {
        Self { world, current_module }
    }
}

impl Environment<'_> {
    fn current_module(&self) -> &Module {
        self.world.modules.get(&self.current_module).unwrap()
    }

    fn find_included_module(&self, name: &str) -> Option<&Module> {
        if name.starts_with('$') {
            println!("Looking up stdlib module: {}", name);
            let module = self.world.stdlib.get_module(name);
            if let Some(m) = module {
                for f in m.functions.keys() {
                    println!("  - {}", f);
                }
            }

            module
        } else {
            self.world.modules.get(self.current_module().aliases.get(name)?)
        }
    }
}

#[derive(Debug, Clone)]
struct Function {
    name: String,
    source: SourceReference,
    kind: FunctionKind,
    entry_point: Label,
    header: Header,
    body: Block,
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
enum FunctionKind {
    State,
    Subroutine,
}

#[derive(Debug, Clone)]
struct Header {
    data_start: Label,
    parameters: Vec<String>,
}

impl Header {
    const DATA_FRAME_VARIABLE_OFFSET: usize = 2;

    fn create(data_start: Label, parameters: &[ValueDeclaration]) -> Self {
        Self {
            data_start,
            parameters: parameters.iter().map(|p| p.identifier.clone()).collect(),
        }
    }

    fn ret_addr_slot(&self) -> Slot {
        Slot {
            label: self.data_start.clone(),
            offset: 0,
            location: Location::Erasable,
        }
    }

    fn ret_value_slot(&self) -> Slot {
        Slot {
            label: self.data_start.clone(),
            offset: 1,
            location: Location::Erasable,
        }
    }

    fn resolve_parameter_slot(&self, name: &str) -> Option<Slot> {
        self.parameters.iter().position(|p| p == name).map(|i| Slot {
            label: self.data_start.clone(),
            offset: (i + Self::DATA_FRAME_VARIABLE_OFFSET) as i32,
            location: Location::Erasable,
        })
    }

    fn reserved_slot_count(&self) -> usize {
        self.parameters.len() + Self::DATA_FRAME_VARIABLE_OFFSET
    }

    fn write_slots(&self, data: &mut Archive) {
        // Return address slot
        data.append(Instruction::ERASE);

        // Return value slot
        data.append(Instruction::ERASE);

        // Parameter slots
        for (i, parameter) in self.parameters.iter().enumerate() {
            data.append(Instruction::ERASE).with_comment(format!(
                "PARAMETER '{}' (+{})",
                parameter,
                i + Self::DATA_FRAME_VARIABLE_OFFSET
            ));
        }
    }
}

struct Workspace {
    function: Rc<Function>,
    locals: Vec<(Option<String>, i32)>,
}

impl Workspace {
    fn create(function: Rc<Function>) -> Self {
        Self {
            function,
            locals: Vec::new(),
        }
    }

    fn insert(&mut self, name: String) -> Slot {
        let offset = (self.locals.len() + self.function.header.reserved_slot_count()) as i32;
        self.locals.push((Some(name), offset));
        Slot {
            offset,
            label: self.function.header.data_start.clone(),
            location: Location::Erasable,
        }
    }

    fn allocate_temporary(&mut self) -> Slot {
        let offset = (self.locals.len() + self.function.header.reserved_slot_count()) as i32;
        self.locals.push((None, offset));
        Slot {
            offset,
            label: self.function.header.data_start.clone(),
            location: Location::Erasable,
        }
    }

    fn resolve(&self, name: &str) -> Option<Slot> {
        for (local, offset) in self.locals.iter() {
            if matches!(local, Some(local) if local == name) {
                return Some(Slot {
                    label: self.function.header.data_start.clone(),
                    offset: *offset,
                    location: Location::Erasable,
                });
            }
        }

        if let Some(offset) = self.function.header.resolve_parameter_slot(name) {
            return Some(offset);
        }

        None
    }

    fn write(&self, data: &mut Archive) {
        data.enqueue_line_break()
            .enqueue_comment(format!("WORKSPACE :: {} ({})", self.function.name, self.function.source))
            .enqueue_label(self.function.header.data_start.clone());

        // Header slots
        self.function.header.write_slots(data);

        // Local slots
        for (name, offset) in self.locals.iter() {
            data.append(Instruction::ERASE).with_comment(if let Some(name) = name {
                format!("LOCAL '{}' (+{})", name, offset)
            } else {
                format!("TEMPORARY (+{})", offset)
            });
        }
    }
}

struct LabelGenerator {
    next_identifier: usize,
}

impl LabelGenerator {
    pub fn new() -> Self {
        Self { next_identifier: 0 }
    }

    pub fn generate(&mut self, prefix: char) -> Label {
        let prefix = prefix.to_ascii_uppercase();
        let label = Label::Generated {
            prefix,
            identifier: self.next_identifier,
        };

        self.next_identifier += 1;
        label
    }
}

struct ConstantPool {
    constants: HashMap<Numeric, i32>,
}

impl ConstantPool {
    const LABEL: Label = Label::Static(Cow::Borrowed("CONSTS"));

    pub fn new() -> Self {
        Self {
            constants: HashMap::new(),
        }
    }

    pub fn get_or_insert(&mut self, value: Numeric) -> Slot {
        let next_offset = self.constants.len();
        let offset = self.constants.entry(value).or_insert_with(|| next_offset as i32);
        Slot {
            label: Self::LABEL.clone(),
            offset: *offset,
            location: Location::Fixed,
        }
    }

    pub fn into_slots(&self) -> Vec<Numeric> {
        // Sort the constants by their offset to get them in order
        let mut constants: Vec<_> = self.constants.iter().collect();
        constants.sort_by_key(|(_, offset)| *offset);
        constants.into_iter().map(|(value, _)| value.clone()).collect()
    }
}

struct Stdlib {
    modules: HashMap<String, Module>,
}

impl Stdlib {
    fn new() -> Self {
        // HACK: we're creating a label generator here which we shouldn't do
        // but we're going to use different prefixes so it should be fine
        let mut label_generator = LabelGenerator::new();
        let mut modules = HashMap::new();
        let parser = ProgramFragmentParser::new();
        for (module_name, source) in STD_LIB_SOURCES.iter() {
            let fragment = parser
                .parse(source)
                .expect(&format!("Failed to parse standard library source '{}'", module_name));

            let mut functions = Vec::new();
            for directive in fragment.directives.into_iter() {
                match directive {
                    Directive::Subroutine { name, parameters, body } => {
                        let label = label_generator.generate('X');
                        let header = Header::create(label_generator.generate('Z'), &parameters);
                        functions.push(Function {
                            name: name.clone(),
                            source: SourceReference::Labelled(format!("{}::{}", module_name, name)),
                            entry_point: label.clone(),
                            body: body.clone(),
                            kind: FunctionKind::Subroutine,
                            header,
                        });
                    }
                    _ => {}
                }
            }

            let module = Module::new(HashMap::new(), functions);
            modules.insert(module_name.to_string(), module);
        }

        Self { modules }
    }

    fn get_module(&self, name: &str) -> Option<&Module> {
        self.modules.get(name)
    }
}

#[cfg(test)]
mod test {
    use std::borrow::Cow;

    use crate::compiler::Label;

    #[test]
    fn test_static_label() {
        let label = Label::from_static("TEST");
        assert!(matches!(label, Label::Static(Cow::Borrowed(..))));
        assert_eq!(label.to_string(), String::from("TEST"));
    }

    #[test]
    fn test_label_generator() {
        let mut generator = super::LabelGenerator::new();
        assert_eq!(
            generator.generate('T'),
            Label::Generated {
                prefix: 'T',
                identifier: 0
            }
        );

        assert_eq!(
            generator.generate('t'),
            Label::Generated {
                prefix: 'T',
                identifier: 1
            }
        );

        assert_eq!(generator.generate('t').to_string(), String::from("T0000002"));
    }
}
