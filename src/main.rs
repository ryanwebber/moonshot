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

impl<'a> From<parser::SyntaxError<'a>> for ReportableError {
    fn from(e: parser::SyntaxError<'a>) -> Self {
        ReportableError { msg: e.to_string() }
    }
}

impl<'a> From<tokenizer::TokenizingError<'a>> for ReportableError {
    fn from(e: tokenizer::TokenizingError<'a>) -> Self {
        match e {
            tokenizer::TokenizingError::UnexpectedEOF => ReportableError {
                msg: String::from("Unexpected end of source"),
            },
            tokenizer::TokenizingError::UnexpectedToken { cursor } => ReportableError {
                msg: format!("Unexpected token: {}", cursor.peekable().peek().unwrap_or(&'?')),
            },
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

    let parse = parser::try_parse_all(&tokens)?;

    println!("#");
    println!("# AST");
    println!("#");
    println!();
    for line in sexpr::Value::from(&parse).to_string().lines() {
        println!("# {}", line)
    }
    println!();

    let mut ids = utils::Counter(ir::Id(0));

    let modules = {
        let modules: Result<Vec<compiler::ModuleCompilation>, _> = parse
            .modules
            .iter()
            .map(|module| {
                compiler::check_and_build_header(module, &mut ids).map(|header| compiler::check_and_compile(module, header))?
            })
            .collect();

        modules?
    };

    let program = compiler::package_modules(modules)?;

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
        generator::generate(&program)
            .release_to_yul_assembly()
            .expect("Unable to generate assembly source due to internal error")
    };

    Ok(contents)
}

fn main() {
    let rope = indoc::indoc! {r#"
        module _ {
            proc main () -> () {
                let x: i15 = 1 + add(a: 2, b: 3);
            }

            proc add (a: i15, b: i15) -> (c: i15) {
                c = a + b;
            }
        }
    "#};

    match try_compile(rope) {
        Ok(out) => {
            println!("{}", out);
        }
        Err(e) => println!("{}", e),
    }
}
