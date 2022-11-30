pub mod ast;
pub mod compiler;
pub mod ir;
pub mod parser;
pub mod sexpr;
pub mod tokenizer;
pub mod types;

fn main() {
    let rope = indoc::indoc! {r#"
        module _ {
            import sys;
            import other from "./other.pf";

            proc main (x: i1, y: i2) -> (z: i3) {
                let x: i15 = (6 + ((2 + 5) + 0));
                let x: i15 = 9;
            }

            proc foo i15 -> i15 {
                let bar: i15 = 1;
                let baz: (_: i15) = 1;
            }
        }

        module _ {
        }
    "#};

    let scanner = tokenizer::Scanner::new(&rope);

    println!("==  Rope  ==");
    println!();
    println!("{}", rope);

    let tokenization: Result<Vec<_>, _> = scanner.iter().collect();
    let tokens = match tokenization {
        Ok(tokens) => {
            println!("== Tokens ==");
            println!();
            println!("Tokenized {} token(s)...", tokens.len());

            tokens
        }
        Err(err) => {
            println!("== Errors ==");
            println!();
            println!("[Tokenization Error]: {:#?}", err);
            println!();
            return;
        }
    };

    println!();

    let mut stream = parser::TokenStream::new(&tokens);
    let parse = match parser::Parser::try_parse(&mut stream) {
        Ok(ast) => {
            println!("== Parse ==");
            println!();
            let tree = sexpr::IntoValue::into(&ast);
            println!("{}", tree);
            println!();
            ast
        }
        Err(err) => {
            println!("== Errors ==");
            println!();
            println!("{}", err);
            println!();
            return;
        }
    };

    let compilations: Result<Vec<_>, _> = parse
        .modules
        .iter()
        .map(|m| compiler::Compiler::check_and_compile(m, &compiler::ImportMap::empty()))
        .collect();

    match compilations {
        Ok(compilations) => {
            println!("== Compilations ==");
            for c in &compilations {
                println!("");
                println!("  module {} {{", c.module_name);
                for proc in &c.procedures {
                    println!("      {} {{", proc.prototype);
                    for instr in &proc.body.instructions {
                        println!("        {}", sexpr::IntoValue::into(instr));
                    }
                    println!("      }}")
                }
                println!("  }}");
            }
        }
        Err(err) => {
            println!("== Errors ==");
            println!();
            println!("{:?}", err);
            println!();
            return;
        }
    }

    println!();
}
