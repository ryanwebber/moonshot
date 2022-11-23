pub mod tokenizer;

fn main() {
    let source = String::from("Hello world!");
    let scanner = tokenizer::Scanner::new(&source);

    println!("Tokens:");
    for t in scanner.iter() {
        println!(" - Token: {}", t.range);
    }
}
