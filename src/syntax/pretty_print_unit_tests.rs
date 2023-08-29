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
        Err(e) => {
            //println!("{:?}", e);
            //assert!(false, "An error occurred: {:?}", e);
            //Modul::top();
            panic!("An error occurred: {:?}", e);
        }
    };
    //assert_eq!(
        //parsed,
        //whole_input(modul()).easy_parse(parsed.to_pretty()).map(|(v, _s)| v)
    //);
}