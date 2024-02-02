
#[test]
#[allow(unused_variables)]
fn test_derive() {
    let t = trybuild::TestCases::new();
    // t.pass("tests/pass/02-one-type-param.rs");
    // t.pass("tests/other/03-two-type-params.rs");
    // t.pass("tests/other/04-no-lifetime.rs");

    t.pass("tests/pass/*.rs");
    t.compile_fail("tests/fail/*.rs");
    macrotest::expand("tests/expand/*.rs");
}
