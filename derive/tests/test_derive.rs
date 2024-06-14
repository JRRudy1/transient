
#[test]
fn test_pass() {
    let t = trybuild::TestCases::new();
    t.pass("tests/pass/*.rs");
}

#[test]
fn test_fail() {
    let t = trybuild::TestCases::new();
    t.compile_fail("tests/fail/*.rs");
}

#[test]
fn test_expand() {
    macrotest::expand("tests/expand/*.rs");
}
