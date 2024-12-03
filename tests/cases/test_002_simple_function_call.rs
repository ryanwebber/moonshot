use crate::TestCase;

const TEST_CASE: TestCase = TestCase {
    name: file!(),
    source: indoc::indoc! {r#"
        state main () [] {
            $tests::exit_with(value: test());
        }

        sub test() -> WORD {
            return 5678;
        }
    "#},
};

#[test]
fn test() {
    TEST_CASE.run(|result| {
        assert_eq!(result, 5678);
    });
}
