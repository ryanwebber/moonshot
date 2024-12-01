use std::{collections::HashMap, path::PathBuf, rc::Rc};

use crate::{
    agc::{Address, Instruction},
    ast::{Block, Directive, Expression, Statement, ValueDeclaration, ValueIdentifier},
    generator::{Archive, Generator, Label, Location, Output, Slot},
    loader::Program,
    types::Numeric,
};

pub struct Compiler {}

impl Compiler {
    pub fn new() -> Self {
        Self {}
    }

    pub fn compile(&self, program: &Program) -> anyhow::Result<Output> {
        State::new().compile(program)
    }
}

struct State {
    generator: Generator,
    label_generator: LabelGenerator,
    constant_pool: ConstantPool,
}

impl State {
    fn new() -> Self {
        Self {
            generator: Generator::new(),
            label_generator: LabelGenerator::new(),
            constant_pool: ConstantPool::new(),
        }
    }

    pub fn compile(mut self, program: &Program) -> anyhow::Result<Output> {
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

        let mut environment = Environment::new();

        for unit in &program.compilation_units {
            for directive in &unit.fragment().directives {
                match directive {
                    Directive::Include { .. } => {}
                    Directive::UserProgram { .. } => {}
                    Directive::State {
                        name, parameters, body, ..
                    } => {
                        let label: Label = self.label_generator.generate('S');
                        let header = Header::create(self.label_generator.generate('W'), parameters);
                        environment.insert_function(Function {
                            name: name.clone(),
                            path: unit.path().clone(),
                            entry_point: label.clone(),
                            body: body.clone(),
                            kind: FunctionKind::State,
                            header,
                        });
                    }
                    Directive::Subroutine { name, parameters, body } => {
                        let label = self.label_generator.generate('F');
                        let header = Header::create(self.label_generator.generate('W'), parameters);
                        environment.insert_function(Function {
                            name: name.clone(),
                            path: unit.path().clone(),
                            entry_point: label.clone(),
                            body: body.clone(),
                            kind: FunctionKind::Subroutine,
                            header,
                        });
                    }
                }
            }
        }

        for function in environment.iter_functions().collect::<Vec<_>>() {
            let comment = format!(
                "{} :: {} ({})",
                match function.kind {
                    FunctionKind::State => "STATE",
                    FunctionKind::Subroutine => "SUBROUTINE",
                },
                function.name,
                function.path.display()
            );

            self.generator
                .code_mut()
                .enqueue_line_break()
                .enqueue_label(function.entry_point.clone())
                .enqueue_comment(comment);

            let mut workspace = Workspace::create(function.clone());
            self.compile_function(&environment, &function.body, &mut workspace)?;
            workspace.write(self.generator.data_mut());
        }

        // Emit an exit instruction just to avoid falling through into constants
        self.generator
            .code_mut()
            .append(Instruction::TC(Address::Relative {
                label: Label::from_static("EXIT"),
                offset: 0,
            }))
            .with_comment("END OF FUNCTIONS")
            .with_preceding_line_break();

        if !self.constant_pool.constants.is_empty() {
            self.generator
                .code_mut()
                .enqueue_line_break()
                .enqueue_comment("CONSTANT POOL");
            for (constant, label) in self.constant_pool.constants.into_iter() {
                self.generator.code_mut().append(Instruction::DEC(constant)).with_label(label);
            }
        }

        Ok(self.generator.into_output())
    }

    fn compile_function(&mut self, environment: &Environment, block: &Block, workspace: &mut Workspace) -> anyhow::Result<()> {
        // Store the Q register in the return slot so we can ret to it
        self.generator.copy_reg_q_to_slot(workspace.function.header.ret_addr_slot());

        for statement in block.statements.iter() {
            match statement {
                Statement::Definition(definition) => {
                    self.generator.code_mut().enqueue_comment(format!(
                        "{} := {}",
                        definition.declaration.identifier,
                        definition.expression.brief()
                    ));

                    let lhs = workspace.insert(definition.declaration.identifier.clone());
                    let rhs = self.compile_expression(environment, &definition.expression, &workspace)?;
                    if let Some(rhs) = rhs {
                        self.generator.copy_slot_to_reg_a(rhs);
                        self.generator.copy_reg_a_to_slot(lhs);
                    } else {
                        anyhow::bail!("RHS expression does not produce a value: {}", definition.expression.brief());
                    }
                }
                Statement::Expression(expression) => {
                    self.compile_expression(environment, expression, &workspace)?;
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
        expression: &Expression,
        workspace: &Workspace,
    ) -> anyhow::Result<Option<Slot>> {
        match expression {
            Expression::NumberLiteral(value) => {
                let label = self.constant_pool.get_or_insert(&mut self.label_generator, value.clone());
                Ok(Some(Slot {
                    label,
                    offset: 0,
                    location: Location::Fixed,
                }))
            }
            Expression::VariableReference(identifier) => match identifier {
                ValueIdentifier::Implicit(name) => workspace
                    .resolve(name)
                    .ok_or_else(|| anyhow::anyhow!("Variable not found: {}", name))
                    .map(|s| Some(s)),
                ValueIdentifier::Namespaced(..) => unimplemented!("Support for namespaced identifiers"),
            },
            Expression::FunctionCall { function, arguments } => {
                let resolved_function = match function {
                    ValueIdentifier::Implicit(name) => environment.resolve_function(name),
                    ValueIdentifier::Namespaced(..) => unimplemented!("Support for namespaced identifiers"),
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
                        .compile_expression(environment, &argument.expression, workspace)?
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
        }
    }
}

struct Environment {
    functions: HashMap<String, Rc<Function>>,
}

impl Environment {
    fn new() -> Self {
        Self {
            functions: HashMap::new(),
        }
    }

    fn insert_function(&mut self, function: Function) {
        self.functions.insert(function.name.clone(), Rc::new(function));
    }

    fn resolve_function(&self, name: &str) -> Option<&Function> {
        self.functions.get(name).map(|f| f.as_ref())
    }

    fn iter_functions<'a>(&'a self) -> impl Iterator<Item = Rc<Function>> + 'a {
        self.functions.values().cloned()
    }
}

#[derive(Debug, Clone)]
struct Function {
    name: String,
    path: PathBuf,
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
    data_frame: Label,
    parameters: Vec<String>,
}

impl Header {
    const DATA_FRAME_VARIABLE_OFFSET: usize = 2;

    fn create(data_frame: Label, parameters: &[ValueDeclaration]) -> Self {
        Self {
            data_frame,
            parameters: parameters.iter().map(|p| p.identifier.clone()).collect(),
        }
    }

    fn ret_addr_slot(&self) -> Slot {
        Slot {
            label: self.data_frame.clone(),
            offset: 0,
            location: Location::Erasable,
        }
    }

    fn ret_value_slot(&self) -> Slot {
        Slot {
            label: self.data_frame.clone(),
            offset: 1,
            location: Location::Erasable,
        }
    }

    fn resolve_parameter_slot(&self, name: &str) -> Option<Slot> {
        self.parameters.iter().position(|p| p == name).map(|i| Slot {
            label: self.data_frame.clone(),
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
    locals: Vec<(String, i32)>,
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
        self.locals.push((name, offset));
        Slot {
            label: self.function.header.data_frame.clone(),
            offset: offset,
            location: Location::Erasable,
        }
    }

    fn resolve(&self, name: &str) -> Option<Slot> {
        for (local, offset) in self.locals.iter() {
            if local == name {
                return Some(Slot {
                    label: self.function.header.data_frame.clone(),
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
            .enqueue_comment(format!(
                "WORKSPACE :: {} ({})",
                self.function.name,
                self.function.path.display()
            ))
            .enqueue_label(self.function.header.data_frame.clone());

        // Header slots
        self.function.header.write_slots(data);

        // Local slots
        for (name, offset) in self.locals.iter() {
            data.append(Instruction::ERASE)
                .with_comment(format!("LOCAL '{}' (+{})", name, offset));
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
    constants: HashMap<Numeric, Label>,
}

impl ConstantPool {
    pub fn new() -> Self {
        Self {
            constants: HashMap::new(),
        }
    }

    pub fn get_or_insert(&mut self, label_generator: &mut LabelGenerator, value: Numeric) -> Label {
        if let Some(label) = self.constants.get(&value) {
            return label.clone();
        }

        let label = label_generator.generate('C');
        self.constants.insert(value, label.clone());
        label
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
