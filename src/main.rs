pub mod parser;
pub mod sexpr;
pub mod tokenizer;

fn main() {
    let rope = indoc::indoc! {"
        module _ {
            proc main () -> () {
                let x: i15 = (6 + ((2 + 5) + 0));
                let x: i15 = 9;
            }
        }
    "};

    let scanner = tokenizer::Scanner::new(&rope);

    println!("==  Rope  ==");
    println!();
    println!("{}", rope);

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
    match parser::Parser::try_parse_module(&mut stream) {
        Ok(ast) => {
            println!("== Parse ==");
            println!();
            let tree = sexpr::IntoValue::into(&ast);
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
