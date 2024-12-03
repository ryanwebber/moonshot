use crate::TestCase;

const TEST_CASE: TestCase = TestCase {
    name: file!(),
    source: indoc::indoc! {r#"
        state main () [] {
            let a: WORD = 1234;
            $tests::exit_with(value: a + 1);
        }
    "#},
};

#[test]
fn test() {
    TEST_CASE.run(|result: i16| {
        assert_eq!(result, 1235);
    });
}
