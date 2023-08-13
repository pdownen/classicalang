extern crate quickcheck;
#[cfg(test)]
use combine::EasyParser;
use quickcheck_macros::quickcheck;

use crate::{
    parsing::parse::*,
    syntax::sequential::{Lit, Name},
};
use quickcheck::{Arbitrary, Gen};

impl Arbitrary for Lit {
    fn arbitrary(g: &mut Gen) -> Lit {
        match i32::arbitrary(g) % 4 {
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
/* 
#[quickcheck]
fn lit_int_parses<'a>(num: i64) -> bool {
    let x = num.to_string().clone();
    let y = x.as_str();
    match lit().easy_parse(y) {
        Ok((v, _s)) => Lit::int(num) == v,
        Err(_) => false,
    }
}

#[quickcheck]
fn lit_quoted_str_parses<'a>(str: String) -> bool {
    match lit().easy_parse(str.as_str()) {
        Ok((v, _s)) => Lit::str(str.clone()) == v,
        Err(_) => false,
    }
}

#[quickcheck]
fn lit_parses<'a>(literal: Lit) -> bool {
    let x = literal.to_string();
    let y: &str = x.as_str();

    match lit().easy_parse(y) {
        Ok((v, _s)) => v == literal,
        Err(_) => false,
    }
}
*/

