
#[test]
#[allow(unused_variables)]
fn test_derive() {
    let t = trybuild::TestCases::new();
    t.pass("tests/pass/*.rs");
    t.compile_fail("tests/fail/*.rs");
    macrotest::expand("tests/expand/*.rs");
}
