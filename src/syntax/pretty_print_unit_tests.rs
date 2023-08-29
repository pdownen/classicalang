use combine::EasyParser;

use crate::{
    parsing::parse::*,
    syntax::sequential::{AtomicPat, CopatOp, Decl, DeconsOp, Lit, Name, Pat, Modul, PrettyPrint},
};

#[test]
fn pretty_test() {
    let indentation_width = 80;
    let t = "x -> y;";
    let parsed = match whole_input(modul()).easy_parse(t) {
        Ok((v1, _s)) => v1,
        Err(e) => panic!("An error occurred: {:?}", e)
    };
    let pretty_str = parsed.to_pretty(indentation_width);
    let pretty_str = pretty_str.as_str();
    let parsed_pretty = match whole_input(modul()).easy_parse(pretty_str) {
        Ok((v1, _s)) => v1,
        Err(e) => panic!("An error occurred: {:?}", e)
    };
    assert_eq!(
        parsed,
        parsed_pretty
    );
}