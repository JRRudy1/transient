
#[test]
#[allow(unused_variables)]
fn macro_tests() {
    let t = trybuild::TestCases::new();
    t.pass("tests/01-no-type-params.rs");
    t.pass("tests/02-one-type-param.rs");
    t.pass("tests/03-two-type-params.rs");
    t.pass("tests/04-no-lifetime.rs");
    t.compile_fail("tests/05-extra-lifetime.rs");
}