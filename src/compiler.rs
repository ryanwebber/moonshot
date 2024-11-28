use std::{
    collections::{HashMap, HashSet},
    fmt::{self, Formatter},
};

use crate::{agc::Instruction, loader::Program};

pub struct Compiler {}

impl Compiler {
    pub fn new() -> Self {
        Self {}
    }

    pub fn compile(&self, program: &Program) -> anyhow::Result<Output> {
        Ok(Output {
            instructions: vec![
                Instruction::NOOP,
                Instruction::TC,
                Instruction::AD,
                Instruction::CAF,
                Instruction::RETURN,
                Instruction::NOOP,
                Instruction::TC,
                Instruction::NOOP,
                Instruction::TC,
            ],
            labels: {
                let mut labels = HashMap::new();
                labels.insert(0, "START".to_string());
                labels.insert(3, "MIDVERYLONG".to_string());
                labels.insert(5, "END".to_string());
                labels
            },
            comments: {
                let mut comments = HashMap::new();
                comments.insert(0, "Wow a comment at the beginning".to_string());
                comments.insert(1, "This is a comment".to_string());
                comments.insert(5, "This is another comment".to_string());
                comments
            },
            line_breaks: {
                let mut line_breaks = HashSet::new();
                line_breaks.insert(2);
                line_breaks.insert(5);
                line_breaks
            },
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Output {
    pub instructions: Vec<Instruction>,
    pub labels: HashMap<usize, String>,
    pub comments: HashMap<usize, String>,
    pub line_breaks: HashSet<usize>,
}

impl Output {
    pub fn pretty<'a>(&'a self) -> impl std::fmt::Display + 'a {
        PrettyOutput { output: self }
    }
}

struct PrettyOutput<'a> {
    output: &'a Output,
}

impl std::fmt::Display for PrettyOutput<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let output = self.output;

        for (i, instruction) in output.instructions.iter().enumerate() {
            if output.line_breaks.contains(&i) {
                writeln!(f)?;
            }

            if let Some(comment) = output.comments.get(&i) {
                writeln!(f, "                # {}", comment)?;
            }

            if let Some(label) = output.labels.get(&i) {
                writeln!(f, "{: <8.8}        {}", label, instruction)?;
            } else {
                writeln!(f, "                {}", instruction)?;
            }
        }

        Ok(())
    }
}

struct LabelGenerator {
    prefixes: HashMap<String, usize>,
}

impl LabelGenerator {
    pub fn new() -> Self {
        Self {
            prefixes: HashMap::new(),
        }
    }

    pub fn generate(&mut self, prefix: &str) -> Label {
        let prefix = prefix.to_uppercase();
        let entry = self.prefixes.entry(prefix.clone());
        let index = entry.or_insert(0);
        *index += 1;
        Label(format!("{:.4}{:0>4}", prefix, index))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Label(String);

impl From<&str> for Label {
    fn from(s: &str) -> Self {
        Label(s.to_string())
    }
}

impl Label {
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[cfg(test)]
mod test {
    use crate::compiler::Label;

    #[test]
    fn test_label_generator() {
        let mut generator = super::LabelGenerator::new();
        assert_eq!(generator.generate("test"), Label::from("TEST0001"));
        assert_eq!(generator.generate("TEST"), Label::from("TEST0002"));
        assert_eq!(generator.generate("biGtEst"), Label::from("BIGT0001"));
        assert_eq!(generator.generate("S"), Label::from("S0001"));
    }
}
