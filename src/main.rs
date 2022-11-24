pub mod parser;
pub mod sexpr;
pub mod tokenizer;

fn main() {
    let rope = indoc::indoc! {"
        6 + 0
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
            println!("Tokenization error: {:#?}", err);
            println!();
            return;
        }
    };

    println!();

    let mut stream = parser::TokenStream::new(&tokens);
    let parser = parser::Parser::new();
    match parser.try_parse_expr(&mut stream) {
        Ok(ast) => {
            println!("== Parse ==");
            println!();
            let tree: sexpr::Value<'_> = sexpr::IntoValue::into(&ast);
            println!("{}", tree);
        }
        Err(err) => {
            println!("== Errors ==");
            println!();
            println!("Parse error: {}", err.description);
            println!();
            return;
        }
    };
}
