use crate::{run_test, TestCase};

#[test]
fn test() {
    struct Test;

    impl TestCase for Test {
        const NAME: &'static str = "test_01_harness_validation";
        const SOURCE: &'static str = indoc::indoc! {r#"
            state main () [] {
                let x: i15 = 1234;
                @asm CAE (x);
                @asm XCH ASSRTVAL;
                @asm TC EXIT;
            }
        "#};

        fn finish(asserval: i16) {
            assert_eq!(asserval, 1234);
        }
    }

    run_test::<Test>();
}
