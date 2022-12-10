pub mod ast;
pub mod compiler;
pub mod generator;
pub mod ir;
pub mod parser;
pub mod sexpr;
pub mod tokenizer;
pub mod types;

struct ReportableError {
    msg: String,
}

impl From<compiler::CompilerError> for ReportableError {
    fn from(e: compiler::CompilerError) -> Self {
        ReportableError {
            msg: e.msg,
        }
    }
}

impl From<compiler::PackagingError> for ReportableError {
    fn from(e: compiler::PackagingError) -> Self {
        ReportableError {
            msg: e.msg,
        }
    }
}

impl From<generator::GeneratorError> for ReportableError {
    fn from(_e: generator::GeneratorError) -> Self {
        ReportableError {
            msg: String::from("Code generation failed"),
        }
    }
}

impl<'a> From<parser::SyntaxError<'a>> for ReportableError {
    fn from(e: parser::SyntaxError<'a>) -> Self {
        ReportableError { msg: e.to_string() }
    }
}

impl From<tokenizer::TokenizingError> for ReportableError {
    fn from(_e: tokenizer::TokenizingError) -> Self {
        ReportableError {
            msg: String::from("Unexpected token"),
        }
    }
}

impl std::fmt::Display for ReportableError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
}

fn try_compile<'a>(rope: &'a str) -> Result<String, ReportableError> {
    let scanner = tokenizer::Scanner::new(&rope);
    let tokens = {
        let tokens: Result<Vec<tokenizer::Token<'a>>, _> = scanner.iter().collect();
        tokens?
    };

    let parse = parser::Parser::try_parse_all(&tokens)?;
    let modules = {
        let modules: Result<Vec<compiler::ModuleCompilation>, _> = parse
            .modules
            .iter()
            .map(|m| compiler::Compiler::check_and_compile(m, &compiler::ImportMap::empty()))
            .collect();
        
        modules?
    };

    let program = compiler::Compiler::package_modules(&modules)?;
    let buffer = {
        let mut buf: Vec<u8> = Vec::new();
        let _ = generator::Generator::try_generate(&program, &mut Box::new(&mut buf))?;
        buf
    };

    Ok(std::str::from_utf8(buffer.as_slice()).unwrap().to_string())
}

fn main() {
    let rope = indoc::indoc! {r#"
        module _ {
            import sys;
            import other from "./other.pf";

            proc main (x: i1, y: i2) -> (z: i3) {
                let abc: i15 = 78.1;
                let x: i15 = (6 + ((2 + abc) + 0));
                let y: i15 = x;
            }

            proc foo i15 -> i15 {
                let bar: i15 = 1;
                let baz: (_: i15) = 2;
            }
        }

        module dsky {
        }
    "#};


    match try_compile(rope) {
        Ok(out) => {
            println!("{}", out);
        },
        Err(e) => {
            println!("Error: {}", e);
        }
    }
}
