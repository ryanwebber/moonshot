use std::{io::Write, path::PathBuf};

use moonshot::{
    compiler::Compiler,
    loader::{Program, SourceReader, SourceReference},
    parser,
};
use regex::Regex;

mod cases;

pub trait TestCase {
    const NAME: &'static str;
    const SOURCE: &'static str;

    fn finish(_asserval: i16) {
        // Noop
    }
}

pub fn run_test<T: TestCase>() {
    if std::env::var("MOONSHOT_RUN_INTEGRATION_TESTS").is_err() {
        println!("Skipping AGC tests: {}", T::NAME);
        return;
    }

    let ast = parser::parse(T::SOURCE).expect("Failed to parse source");
    let program = Program::single_source(SourceReference::Labelled(String::from(T::NAME)), ast);
    let output = Compiler::new().compile(&program).expect("Compilation failed");
    let assembly = output.to_yul_assembly();

    // Write the assembly to a  file so we can call yaAGC on it
    let tmp_dir = std::env::temp_dir();
    let test_asm_file_path = PathBuf::from(format!("{}.asm", T::NAME));
    let test_bin_file_path = PathBuf::from(format!("{}.asm.bin", T::NAME));

    println!("Temp dir: {:?}", tmp_dir);
    println!("Assembly path: {:?}", test_asm_file_path);
    println!("AGC rope path: {:?}", test_bin_file_path);

    std::fs::write(tmp_dir.join(&test_asm_file_path), assembly).expect("Failed to write assembly to file");

    // Invoke yaAGC, which will produce a `.bin` file beside the `.asm` file
    {
        let yaagc_output = std::process::Command::new("yaYUL")
            .current_dir(&tmp_dir)
            .env("CWD", &tmp_dir)
            .arg(&test_asm_file_path)
            .output()
            .expect("Failed to run yaAGC");

        assert!(yaagc_output.status.success());
    }

    // Invoke yaYUL on the `.bin` file, which will produce a `.lst` file beside the `.bin` file.
    // Because we're starting up the debugger, we'll need to pipe in some input to stdin
    {
        let input = "break EXIT\nrun\nprint ASSRTVAL\nquit\n";
        let child = std::process::Command::new("yaAGC")
            .current_dir(&tmp_dir)
            .env("CWD", &tmp_dir)
            .arg(&test_bin_file_path)
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .spawn()
            .expect("Failed to run yaYUL");

        {
            let mut stdin = child.stdin.as_ref().expect("Failed to open stdin");
            stdin.write_all(input.as_bytes()).expect("Failed to write to stdin");
        }

        // Wait on the child process to finish
        let output = child.wait_with_output().expect("Failed to wait on child process");

        // Extract the `ASSRTVAL` from the output
        let stdout_as_str = std::str::from_utf8(&output.stdout).expect("Failed to convert stdout to string");

        let regex = Regex::new(r"\(agc\) \$1 = (\d+)").expect("Failed to compile regex");
        let captures = regex
            .captures_iter(stdout_as_str)
            .next()
            .and_then(|captures| captures.get(1))
            .expect("Failed to find capture group");

        // Parse the assertion value as an octal number
        let value: i16 = i16::from_str_radix(captures.as_str(), 8).expect("Failed to parse assertion value");
        T::finish(value);
    }
}

struct ErrorThrowingSourceReader;

impl SourceReader for ErrorThrowingSourceReader {
    fn read(&self, _: &PathBuf) -> anyhow::Result<String> {
        Err(anyhow::anyhow!("Source loading not supported during tests"))
    }
}
