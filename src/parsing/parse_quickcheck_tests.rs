extern crate quickcheck;

#[cfg(test)]
use combine::EasyParser;
use quickcheck_macros::quickcheck;

use crate::{
    parsing::parse::*,
    syntax::sequential::{Lit, Name},
};
use quickcheck::{Arbitrary, Gen, TestResult};

#[derive(Clone)]
struct AsciiString { str: String }

impl ToString for AsciiString {
    fn to_string(&self) -> String {
        self.str.to_string()
    }
}

impl Arbitrary for AsciiString {
    fn arbitrary(g: &mut Gen) -> AsciiString {
        let mut ascii_str = {
            let mut c: char = char::arbitrary(g);
            while (!c.is_ascii() && c != '_') || c.is_ascii_control() {
                c = char::arbitrary(g);
            }
            c.to_string()
        };

        while u32::arbitrary(g) % 10 != 0 {
            let mut c: char = char::arbitrary(g);
            while !c.is_ascii() || c.is_ascii_control() {
                c = char::arbitrary(g);
            }
            ascii_str.push(c);
        }

        AsciiString { str: ascii_str }
    }
}

impl Arbitrary for Name {
    fn arbitrary(g: &mut Gen) -> Name {
        let mut name = {
            let mut c: char = char::arbitrary(g);
            while !c.is_ascii_alphabetic() && c != '_' {
                c = char::arbitrary(g);
            }
            c.to_string()
        };

        while u32::arbitrary(g) % 10 != 0 {
            let mut c: char = char::arbitrary(g);
            while !c.is_ascii_alphanumeric() && c != '_' {
                c = char::arbitrary(g);
            }
            name.push(c);
        }

        Name::id(name.as_str())
    }
}

impl Arbitrary for Lit {
    fn arbitrary(g: &mut Gen) -> Lit {
        match u32::arbitrary(g) % 4 {
            0 => Lit::int(i64::arbitrary(g)),
            1 => Lit::flt(f64::arbitrary(g)),
            2 => Lit::str(AsciiString::arbitrary(g).to_string()),
            _ => {
                let mut c: char = char::arbitrary(g);
                while !c.is_ascii_alphabetic() {
                    c = char::arbitrary(g);
                }
                let name = format!("{}{}", c.to_uppercase(), Name::arbitrary(g));
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

/* 
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
*/

// skipping inf for now
#[quickcheck]
fn lit_flt_print_parse<'a>(num: f64) -> bool {
    if num.is_nan() || num.is_infinite() {
        return true;
    }

    let x = format!("{num:?}");
    let result = lit().easy_parse(x.as_str());

    match result {
        Ok((v, _s)) => Lit::flt(num) == v,
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
    let printed = {
        match literal {
            Lit::Flt(f) => format!("{f:?}"),
            _ => literal.to_string() 
        }
    }; 
    let parsed = lit().easy_parse(printed.as_str());

    match parsed {
        Ok((v, _s)) => v == literal,
        Err(_) => false,
    }
}


// arbitrary cases should pick evenly between
// stopping and continuing dot/app cases


