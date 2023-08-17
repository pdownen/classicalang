extern crate quickcheck;

use std::io::LineWriter;

use combine::{EasyParser, Parser};
use quickcheck_macros::quickcheck;

use crate::{
    parsing::parse::*,
    syntax::{sequential::{Lit, Name, Pat, DeconsOp}, nested::Decons},
};
use quickcheck::{Arbitrary, Gen, TestResult, empty_shrinker};

#[derive(Clone, Debug)]
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
            while (!c.is_ascii() || c.is_ascii_control()) && c != '\n' {
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
            _ => Lit::str(AsciiString::arbitrary(g).to_string()),
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

    // fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
    //     match *self {
    //         Lit::Int(i) => Box::new(Lit::int(i)),
    //         Lit::Flt(_) => todo!(),
    //         Lit::Str(_) => todo!(),
    //         Lit::Sym(_) => todo!(),
    //     }
    // }
}

// attempted to do shrink
// impl Iterator for Lit {
//     type Item = Lit;

//     fn next(&mut self) -> Option<Self::Item> {
//         match self {
//             Lit::Int(i) => Some(Lit::int(i.shrink().next().unwrap())),
//             Lit::Flt(f) => Some(Lit::flt(f.shrink().next().unwrap())),
//             Lit::Str(s) => Some(Lit::str(s.shrink().next().unwrap())),
//             Lit::Sym(_) => todo!(),
//         }
//     }
// }

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

fn lit_quoted_str_parses(str: AsciiString) -> bool {
    let quoted_str = Lit::str(str.to_string()).to_string();

    let result = lit().easy_parse(quoted_str.as_str());
    match result {
        Ok((v, _s)) => {
            Lit::str(str.to_string()) == v
        },
        Err(_) => false,
    }
}

#[quickcheck]
fn lit_quoted_str_parses_all(str: AsciiString) -> bool {
    lit_quoted_str_parses(str)
}

#[test]
fn lit_quoted_str_parses_some() {
    assert!(lit_parses(Lit::str("".to_owned())));
    assert!(lit_parses(Lit::str("\n\nok\n".to_owned())));
}

fn lit_parses(literal: Lit) -> bool {
    let printed = literal.to_string(); 
    let parsed = lit().easy_parse(printed.as_str());

    match parsed {
        Ok((Lit::Flt(f), _s)) if f.is_nan() => printed.parse::<f64>().unwrap().is_nan(),
        Ok((v, _s)) => v == literal,
        Err(_) => false,
    }
}

#[quickcheck]
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
    assert!(lit_parses(Lit::str(r#"\\ \" \\"#.to_owned())));
    assert!(lit_parses(Lit::sym(Name::id("Symbol"))));
    assert!(lit_parses(Lit::sym(Name::id("Symbolic_name2"))));
}


// arbitrary co/pat cases should pick evenly between
// stopping and continuing dot/app cases

impl Arbitrary for DeconsOp {
    fn arbitrary(g: &mut Gen) -> DeconsOp {
        DeconsOp::App(Pat::arbitrary(g))
    }
}

impl Arbitrary for Pat {
    fn arbitrary(g: &mut Gen) -> Pat {
        match u32::arbitrary(g) % 4 {
            0 => Pat::Unused,
            1 | 2 => {
                let mut c: char = char::arbitrary(g);
                while !c.is_ascii_alphabetic() {
                    c = char::arbitrary(g);
                }
                let name = format!("{}{}", c.to_lowercase(), Name::arbitrary(g));
                Pat::Var(Name::id(name.as_str()))
            }
            _ => {
                let head = Lit::arbitrary(g).mtch();
                let mut tail = vec![];
                while u32::arbitrary(g) % 2 != 0 {
                    tail.push(DeconsOp::arbitrary(g));
                }

                Pat::Struc(head.extend(tail))
            }
        }
    }
}

fn pat_parses(pattern: Pat) -> bool {
    let printed = pattern.to_string(); 
    let parsed = pat().easy_parse(printed.as_str());


    match parsed {
        Ok((v, _s)) => {
        println!("
        {printed}
        {pattern:?}
            parses as 
        {v:?}"
        );
            v == pattern
        },
        Err(_) => false,
    }
}

// #[quickcheck]
// fn pat_parses_all(pat: Pat) -> bool {
//     pat_parses(pat)
// }

#[test]
fn pat_parses_some() {
    assert!(pat_parses(
        Pat::Struc(
            Lit::Sym(Name::id("Sym")).mtch()
            .extend(vec![
                DeconsOp::App(Pat::Struc(Lit::int(1).mtch())),
                DeconsOp::App(Pat::Unused)
            ])
        )
    ));
}
