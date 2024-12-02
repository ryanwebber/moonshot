use std::path::PathBuf;

use moonshot::{
    compiler::Compiler,
    loader::{FileSystemSourceReader, SourceLoader},
};

fn try_main() -> anyhow::Result<()> {
    let Some(file_path) = &std::env::args().nth(1) else {
        eprintln!("Usage: moonshot <file>");
        return Ok(());
    };

    let file_path = PathBuf::from(file_path);
    let source_reader = FileSystemSourceReader::new_with_cwd()?;
    let source_loader = SourceLoader::new();
    let program = source_loader.parse_and_load(source_reader, &file_path)?;

    let compiler = Compiler::new();
    let output = compiler.compile(&program)?;
    println!("{}", output.to_yul_assembly());

    Ok(())
}

fn main() {
    if let Err(e) = try_main() {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}
