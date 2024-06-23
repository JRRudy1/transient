//! Entry-point for tests that should fail to compile with an expected error message

#[test]
#[cfg_attr(miri, ignore)]
fn variance_tests() {
    let t = trybuild::TestCases::new();
    t.compile_fail("tests/fail/*.rs");
}
