use crate::TestCase;

const TEST_CASE: TestCase = TestCase {
    name: "test_01_harness_validation",
    source: indoc::indoc! {r#"
        state main () [] {
            $tests::exit_with(value: 1234);
        }
    "#},
};

#[test]
fn test() {
    TEST_CASE.run(|result| {
        assert_eq!(result, 1234);
    });
}
