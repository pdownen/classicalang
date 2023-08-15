extern crate quickcheck;

#[cfg(test)]
use combine::EasyParser;
use quickcheck_macros::quickcheck;

use crate::{
    parsing::parse::*,
    syntax::sequential::{Lit, Name},
};
use quickcheck::{Arbitrary, Gen, TestResult};

impl Arbitrary for Lit {
    fn arbitrary(g: &mut Gen) -> Lit {
        match u32::arbitrary(g) % 4 {
            0 => Lit::int(i64::arbitrary(g)),
            1 => Lit::flt(f64::arbitrary(g)),
            2 => Lit::str(String::arbitrary(g)),
            _ => {
                let mut c: char = char::arbitrary(g);
                while !c.is_alphabetic() {
                    c = char::arbitrary(g);
                }
                let name = c.to_uppercase().to_string() + String::arbitrary(g).as_str();
                Lit::sym(Name::id(name.as_str()))
            }
        }
    }
}

#[quickcheck]
fn lit_int_parses<'a>(num: i64) -> TestResult {
    let x = num.to_string();

    let result = lit().easy_parse(x.as_str());
    match result {
        Ok((v, _s)) => TestResult::from_bool(Lit::int(num) == v),
        Err(_) => TestResult::from_bool(false),
    }
}

#[quickcheck]
fn lit_flt_parses<'a>(num: f64) -> TestResult {
    let x = format!("{num}");
    println!("\n{x}");

    let result = lit().easy_parse(x.as_str());
    match result {
        Ok((v, _s)) => TestResult::from_bool(Lit::flt(num) == v),
        Err(_) => TestResult::from_bool(false),
    }
}

// add scientific notation, use default parser
#[quickcheck]
fn lit_flt_print_parse<'a>(num: f64) -> bool {
    let x = num.to_string();

    let result = lit().easy_parse(x.as_str());
    match result {
        Ok((v, _s)) => {    
            println!("{x} parses as {v}");
            Lit::flt(num) == v
        },
        Err(_) => false,
    }
}

/*
#[test]
fn lit_flt() {
    assert!(lit_flt_print_parse(0.0));
}

#[quickcheck]
fn lit_flt_parses(num: f64) -> bool {
    lit_flt_print_parse(num)
}
*/

/* 
#[quickcheck]
fn lit_quoted_str_parses(str: String) -> bool {
    let quoted_str = Lit::str(str.clone()).to_string();

    let result = lit().easy_parse(quoted_str.as_str());
    match result {
        Ok((v, _s)) => {
            assert_eq!(Lit::str(str.clone()), v);
            Lit::str(str.clone()) == v
        },
        Err(_) => false,
    }
}
*/

#[quickcheck]
fn lit_parses(literal: Lit) -> bool {
    let printed = literal.to_string();
    let parsed = lit().easy_parse(printed.as_str());

    match parsed {
        Ok((v, _s)) => v == literal,
        Err(_) => false,
    }
}


// arbitrary cases should pick evenly between
// stopping and continuing dot/app cases


