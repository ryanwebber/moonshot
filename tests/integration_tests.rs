use std::{io::Write, path::PathBuf};

use moonshot::{
    compiler::Compiler,
    loader::{Program, SourceReader, SourceReference},
    parser,
};
use regex::Regex;

mod cases;

pub struct TestCase {
    pub name: &'static str,
    pub source: &'static str,
}

impl TestCase {
    pub fn run(&self, f: impl FnOnce(i16)) {
        run_test(self, f);
    }
}

fn run_test(test_case: &TestCase, f: impl FnOnce(i16)) {
    if std::env::var("MOONSHOT_RUN_INTEGRATION_TESTS").is_err() {
        println!("Skipping AGC tests: {}", test_case.name);
        return;
    }

    let test_slug: String = if test_case.name.ends_with(".rs") {
        let path = PathBuf::from(test_case.name);
        path.file_stem()
            .expect("Failed to get file stem")
            .to_str()
            .expect("Failed to convert OsStr to str")
            .to_string()
    } else {
        test_case.name.to_string()
    };

    let ast = parser::parse(&test_case.source).expect("Failed to parse source");
    let program = Program::new_with_source(SourceReference::Labelled(test_slug.clone()), ast);

    let output = Compiler::new().compile(program).expect("Compilation failed");
    let assembly = output.to_yul_assembly();

    // Write the assembly to a  file so we can call yaAGC on it
    let tmp_dir = std::env::temp_dir();
    let test_asm_file_path = PathBuf::from(format!("{}.asm", test_slug));
    let test_bin_file_path = PathBuf::from(format!("{}.asm.bin", test_slug));

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

        // Dump child stderr to our stderr
        _ = std::io::stderr().write_all(&yaagc_output.stderr);

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
        f(value);
    }
}

struct ErrorThrowingSourceReader;

impl SourceReader for ErrorThrowingSourceReader {
    fn read(&self, _: &PathBuf) -> anyhow::Result<String> {
        Err(anyhow::anyhow!("Source loading not supported during tests"))
    }
}
