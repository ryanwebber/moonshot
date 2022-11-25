pub mod parser;
pub mod sexpr;
pub mod tokenizer;

fn main() {
    let rope = indoc::indoc! {"
        (6 + ((2 1 1) + 0))
    "};

    let _ = indoc::indoc! {"
        let a = guard x + 5 {
            overflow => yield 0,
            divByZero => yield 0,
        };
    "};

    let scanner = tokenizer::Scanner::new(&rope);

    println!("==  Rope  ==");
    println!();
    println!("{}", rope);
    println!();

    let tokenization: Result<Vec<_>, _> = scanner.iter().collect();
    let tokens = match tokenization {
        Ok(tokens) => {
            println!("== Tokens ==");
            println!();
            for t in &tokens {
                println!(" [{:#?}] => {}", t.kind, t.range);
            }

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
    match parser::Parser::try_parse_expr(&mut stream) {
        Ok(ast) => {
            println!("== Parse ==");
            println!();
            let tree: sexpr::Value<'_> = sexpr::IntoValue::into(&ast);
            println!("{}", tree);
            println!();
        }
        Err(err) => {
            println!("== Errors ==");
            println!();
            println!("{}", err);
            println!();
            return;
        }
    };
}
