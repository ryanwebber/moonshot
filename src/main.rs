pub mod agc;
pub mod ast;
pub mod compiler;
pub mod generator;
pub mod ir;
pub mod optimizer;
pub mod parser;
pub mod sexpr;
pub mod tokenizer;
pub mod types;
pub mod utils;

struct ReportableError {
    msg: String,
}

impl From<compiler::CompilerError> for ReportableError {
    fn from(e: compiler::CompilerError) -> Self {
        ReportableError { msg: e.msg }
    }
}

impl From<compiler::PackagingError> for ReportableError {
    fn from(e: compiler::PackagingError) -> Self {
        ReportableError { msg: e.msg }
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
    let mut ids = utils::Counter(ir::Id(0));

    let modules = {
        let modules: Result<Vec<compiler::ModuleCompilation>, _> = parse
            .modules
            .iter()
            .map(|module| {
                compiler::Compiler::check_and_build_header(module, &mut ids).map(|header| compiler::Compiler::check_and_compile(module, header))?
            })
            .collect();

        modules?
    };

    let program = compiler::Compiler::package_modules(modules)?;

    println!("#");
    println!("# PROCEDURE LIST");
    println!("#");

    println!("");

    for (id, procdef) in &program.procedures {
        println!("# [{}] {} {{", id, procdef.prototype.signature);
        for instruction in &procdef.body.instructions {
            println!("# \t{}", sexpr::Value::from(instruction));
        }
        println!("# }}\n");
    }

    let contents = {
        generator::Generator::try_generate(&program)?
            .release_to_yul_assembly()
            .expect("Unable to generate assembly source due to internal error")
    };

    Ok(contents)
}

fn main() {
    let rope = indoc::indoc! {r#"
        module _ {
            import sys;
            import other from "./other.pf";

            proc main (x: i1, y: i2) -> (z: i3) {
                let abc: i15 = 78;
                let x: i15 = (abc + 2);
                let y: i15 = (1 + x);
                let z: i15 = y;
            }

            proc foo () -> () {
                let k: i15 = 0;
            }
        }
    "#};

    match try_compile(rope) {
        Ok(out) => {
            println!("{}", out);
        }
        Err(e) => {
            println!("{}", e);
        }
    }
}
