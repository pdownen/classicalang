extern crate quickcheck;

use combine::{EasyParser, Parser};
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

#[cfg(test)]
#[quickcheck]
fn lit_int_parses<'a>(num: i64) -> TestResult {
    let x = num.to_string();

    let result = lit().easy_parse(x.as_str());
    match result {
        Ok((v, _s)) => TestResult::from_bool(Lit::int(num) == v),
        Err(_) => TestResult::from_bool(false),
    }
}

fn lit_flt_parses(num: f64) -> bool {
    let f = format!("{num:?}");
    let result = lit().easy_parse(f.as_str());

    match result {
        Ok((Lit::Flt(f), _s)) => (num == f) || (num.is_nan() && f.is_nan()),
        _ => false,
    }
}

#[quickcheck]
fn lit_flt_parses_all(num: f64) -> bool {
    lit_flt_parses(num)
}

#[test]
fn lit_flt_parses_some() {
    assert!(lit_flt_parses(0.0));
    assert!(lit_flt_parses(52348746278463287456462238423.0));
    assert!(lit_flt_parses(f64::INFINITY));
    assert!(lit_flt_parses(f64::NEG_INFINITY));
    assert!(lit_flt_parses(f64::NAN));
}

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

fn lit_parses(literal: Lit) -> bool {
    let printed = literal.to_string(); 
    let parsed = lit().easy_parse(printed.as_str());

    match parsed {
        Ok((Lit::Flt(f), _s)) if f.is_nan() => printed.parse::<f64>().unwrap().is_nan(),
        Ok((v, _s)) => v == literal,
        Err(_) => false,
    }
}

fn lit_parses_all(lit: Lit) -> bool {
    lit_parses(lit)
}

#[test]
fn lit_parses_some() {
    assert!(lit_parses(Lit::int(0)));
    assert!(lit_parses(Lit::int(25)));
    assert!(lit_parses(Lit::flt(0.0)));
    assert!(lit_parses(Lit::flt(0.0001235)));
    assert!(lit_parses(Lit::str("!!@  #### rZ".to_owned())));
    assert!(lit_parses(Lit::str("\\ \" \\".to_owned())));
    assert!(lit_parses(Lit::sym(Name::id("Symbol"))));
    assert!(lit_parses(Lit::sym(Name::id("Symbolic_name2"))));
}


// arbitrary cases should pick evenly between
// stopping and continuing dot/app cases


