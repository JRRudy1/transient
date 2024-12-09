//! Entry-point for tests that should fail to compile with an expected error message

#[rustversion::all(since(1.82), stable)]
#[test]
#[cfg_attr(miri, ignore)]
fn variance_tests() {
    let t = trybuild::TestCases::new();
    t.compile_fail("tests/fail/*.rs");
}
