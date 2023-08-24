extern crate quickcheck;

use std::io::LineWriter;

use combine::{EasyParser, Parser};
use num::Integer;
use quickcheck_macros::quickcheck;

use crate::{
    parsing::parse::*,
    syntax::sequential::{
        AtomicPat, Copat, CopatOp, Decl, Decons, DeconsOp, Expr, ExprHead, ExprOp, Lit, Modul,
        Name, Pat,
    },
};
use quickcheck::{empty_shrinker, Arbitrary, Gen, TestResult};

#[derive(Clone, Debug)]
struct AsciiString {
    str: String,
}

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

    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        Box::new(self.id.shrink().map(|s| Name { id: s }))
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

    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        match self {
            Lit::Int(i) => Box::new(i.shrink().map(Lit::int)),
            Lit::Flt(f) => Box::new(f.shrink().map(Lit::flt)),
            Lit::Str(s) => Box::new(s.shrink().map(Lit::str)),
            Lit::Sym(sym) => Box::new(sym.shrink().map(Lit::sym)),
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

fn lit_quoted_str_parses(str: AsciiString) -> bool {
    let quoted_str = Lit::str(str.to_string()).to_string();

    let result = lit().easy_parse(quoted_str.as_str());
    match result {
        Ok((v, _s)) => Lit::str(str.to_string()) == v,
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

impl Arbitrary for Decons {
    fn arbitrary(g: &mut Gen) -> Self {
        let head = Lit::arbitrary(g).mtch();
        let mut tail = vec![DeconsOp::arbitrary(g)];
        while u32::arbitrary(g) % 2 != 0 {
            tail.push(DeconsOp::arbitrary(g));
        }

        head.extend(tail)
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        Box::new(
            (self.head.clone(), self.tail.clone())
                .shrink()
                .map(|(h, t)| h.mtch().extend(t)),
        )
    }
}

impl Arbitrary for DeconsOp {
    fn arbitrary(g: &mut Gen) -> DeconsOp {
        DeconsOp::App(Pat::arbitrary(g))
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        match self {
            DeconsOp::App(pat) => Box::new(pat.shrink().map(DeconsOp::App)),
        }
    }
}

impl Arbitrary for AtomicPat {
    fn arbitrary(g: &mut Gen) -> AtomicPat {
        match u32::arbitrary(g) % 4 {
            0 => AtomicPat::Unused,
            1 | 2 => {
                let mut c: char = char::arbitrary(g);
                while !c.is_ascii_alphabetic() {
                    c = char::arbitrary(g);
                }
                let name = format!("{}{}", c.to_lowercase(), Name::arbitrary(g));
                AtomicPat::Var(Name::id(name.as_str()))
            }
            _ => AtomicPat::Const(Lit::arbitrary(g)),
        }
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        match self {
            AtomicPat::Unused => empty_shrinker(),
            AtomicPat::Var(v) => Box::new(v.shrink().map(AtomicPat::Var)),
            AtomicPat::Const(c) => Box::new(c.shrink().map(AtomicPat::Const)),
        }
    }
}

impl Arbitrary for Pat {
    fn arbitrary(g: &mut Gen) -> Pat {
        match u32::arbitrary(g) % 3 {
            0 | 1 => Pat::Atom(AtomicPat::arbitrary(g)),
            _ => Pat::Struc(Decons::arbitrary(g)),
        }
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        match self {
            Pat::Atom(a) => Box::new(a.shrink().map(Pat::Atom)),
            Pat::Struc(decons) => Box::new(decons.shrink().map(Pat::Struc)),
        }
    }
}

fn pat_parses(pattern: Pat) -> bool {
    let printed = pattern.to_string();
    let parsed = pat().easy_parse(printed.as_str());

    match parsed {
        Ok((v, _s)) => v == pattern,
        Err(_) => false,
    }
}

// #[quickcheck]
// fn pat_parses_all(pat: Pat) -> bool {
//     pat_parses(pat)
// }

#[test]
fn pat_parses_some() {
    // assert!(pat_parses(Pat::Struc(
    //     Name::id("Sym1").sym().mtch().extend(vec![
    //         DeconsOp::App(Pat::Struc(
    //             Name::id("Sym2")
    //                 .sym()
    //                 .mtch()
    //                 .push(DeconsOp::App(Pat::Struc(Lit::int(1).mtch())))
    //         )),
    //         DeconsOp::App(Pat::blank()),
    //         DeconsOp::App(Pat::blank())
    //     ])
    // )));
    //
    assert!(pat_parses(pat().easy_parse("Sym1 (Sym2 a b c)").unwrap().0));
}

impl Arbitrary for CopatOp {
    fn arbitrary(g: &mut Gen) -> Self {
        match u32::arbitrary(g) % 2 {
            0 => CopatOp::App(Pat::arbitrary(g)),
            _ => {
                loop {
                    let lit = Lit::arbitrary(g);

                    if let Lit::Str(_) = lit.clone() {
                        return CopatOp::Dot(lit);
                    }
                    if let Lit::Sym(_) = lit.clone() {
                        return CopatOp::Dot(lit);
                    }
                };
            },
        }
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        match self {
            CopatOp::App(pat) => Box::new(pat.shrink().map(CopatOp::App)),
            CopatOp::Dot(lit) => Box::new(lit.shrink().map(CopatOp::Dot)),
        }
    }
}

impl Arbitrary for Copat {
    fn arbitrary(g: &mut Gen) -> Self {
        let mut copat = AtomicPat::arbitrary(g).this();
        while u32::arbitrary(g) % 2 != 0 {
            copat = copat.push(CopatOp::arbitrary(g));
        }
        copat
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        Box::new(
            (self.head.clone(), self.tail.clone())
                .shrink()
                .map(|(h, t)| h.this().extend(t)),
        )
    }
}

fn copat_parses(copattern: Copat) -> bool {
    let printed = copattern.to_string();
    let parsed = copat().easy_parse(printed.as_str());

    match parsed {
        Ok((v, _s)) => {
            // println!("
            // {printed}
            // {copattern:?}
            //     parses as
            // {}
            // {v:?}", v.to_string()
            // );
        
            v == copattern
        },
        Err(_) => false,
    }
}

// #[quickcheck]
// fn copat_parses_all(copat: Copat) -> bool {
//     copat_parses(copat)
// }

#[test]
fn copat_parses_some() {
    assert!(copat_parses(
        copat().easy_parse("Sym1 (Sym2 a b c)").unwrap().0
    ));
    assert!(copat_parses(
        (Name::id("Symbol").sym().switch().this())
            .app(Name::id("x").bind().atom())
            .app(Name::id("y").bind().atom())
    ));
}

impl Arbitrary for ExprHead {
    fn arbitrary(g: &mut Gen) -> Self {
        match u32::arbitrary(g) % 3 {
            0 => {
                let mut c: char = char::arbitrary(g);
                while !c.is_ascii_alphabetic() {
                    c = char::arbitrary(g);
                }
                let name = format!("{}{}", c.to_lowercase(), Name::arbitrary(g));
                ExprHead::Var(Name::id(name.as_str()))
            },
            1 => ExprHead::Const(Lit::arbitrary(g)),
            _ => ExprHead::Lambda(Modul::arbitrary(g))
        }
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        match self {
            ExprHead::Var(v) => Box::new(v.shrink().map(ExprHead::Var)),
            ExprHead::Const(l) => Box::new(l.shrink().map(ExprHead::Const)),
            ExprHead::Lambda(m) => Box::new(m.shrink().map(ExprHead::Lambda)),
        }
    }
}

impl Arbitrary for ExprOp {
    fn arbitrary(g: &mut Gen) -> Self {
        match u32::arbitrary(g) % 2 {
            0 => ExprOp::App(Expr::arbitrary(g)),
            _ => ExprOp::Dot(Lit::arbitrary(g))
        }
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        match self {
            ExprOp::App(expr) => Box::new(expr.shrink().map(ExprOp::App)),
            ExprOp::Dot(lit) => Box::new(lit.shrink().map(ExprOp::Dot)),
        }
    }
}

impl Arbitrary for Expr {
    fn arbitrary(g: &mut Gen) -> Self {
        let mut expr = ExprHead::arbitrary(g).head();
        while u32::arbitrary(g) % 2 != 0 {
            expr = expr.push(ExprOp::arbitrary(g));
        }
        expr
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        Box::new(
            (self.head.clone(), self.tail.clone())
                .shrink()
                .map(|(h, t)| h.head().extend(t))
        )
    }
}

fn expr_parses(expression: Expr) -> bool {
    let printed = expression.to_string();
    let parsed = expr().easy_parse(printed.as_str());

    match parsed {
        Ok((v, _s)) => {
        println!("
        {printed}
        {expression:?}
            parses as
        {}
        {v:?}", v.to_string()
        );
            v == expression
        },
        Err(_) => false,
    }
}

// #[quickcheck]
// fn expr_parses_all(expr: Expr) -> bool {
//     expr_parses(expr)
// }

#[test]
fn expr_parses_some() {
    assert!(expr_parses(Name::id("x").refer()));
    assert!(expr_parses(Lit::int(3).cnst()));
    assert!(expr_parses(expr().easy_parse("Sym1 (Sym2 a b c)").unwrap().0));
}

impl Arbitrary for Decl {
    fn arbitrary(g: &mut Gen) -> Self {
        match u32::arbitrary(g) % 3 {
            0 => Decl::Include(Expr::arbitrary(g)),
            1 => Decl::Method(Copat::arbitrary(g), Expr::arbitrary(g)),
            _ => Decl::Bind(Pat::arbitrary(g),Expr::arbitrary(g))
        }
    }

    // fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
    //     match self {
    //         Decl::Include(e) => Box::new(e.shrink().map(Decl::Include)),
    //         Decl::Method(c, e) => Box::new(
    //             (c.clone(), e.clone())
    //                 .shrink()
    //                 .map(|(c, e)| Decl::Method(c, e))
    //         ),
    //         Decl::Bind(p, e) => Box::new(
    //             (p.clone(), e.clone())
    //                 .shrink()
    //                 .map(|(p, e)| Decl::Bind(p, e))
    //         ),
    //     }
    // }
}

fn decl_parses(declaration: Decl) -> bool {
    let printed = declaration.to_string();
    let parsed = decl().easy_parse(printed.as_str());

    match parsed {
        Ok((v, _s)) => {
        println!("
        {printed}
        {declaration:?}
            parses as
        {}
        {v:?}", v.to_string()
        );
            v == declaration
        },
        Err(_) => false,
    }
}

#[quickcheck]
fn decl_parses_all(decl: Decl) -> bool {
    decl_parses(decl)
}

#[test]
fn decl_parses_some() {
    assert!(decl_parses(decl().easy_parse("include Symbol").unwrap().0));
    assert!(decl_parses(decl().easy_parse("Symbol(x)(y)(z) -> x").unwrap().0));
    assert!(decl_parses(decl().easy_parse("a <- b").unwrap().0));
}

impl Arbitrary for Modul {
    fn arbitrary(g: &mut Gen) -> Self {
        let mut decls = vec![Decl::arbitrary(g)];
        while u32::arbitrary(g) % 2 != 0 {
            decls.push(Decl::arbitrary(g));
        }
        Modul { defns: decls }
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        Box::new(self.defns.shrink().map(|ds| Modul { defns: ds }))
    }
}

// fn modul_parses(module: Modul) -> bool {
//     let printed = module.to_string();
//     let parsed = modul().easy_parse(printed.as_str());

//     match parsed {
//         Ok((v, _s)) => {
//         println!("
//         {printed}
//         {module:?}
//             parses as
//         {}
//         {v:?}", v.to_string()
//         );
//             v == module
//         },
//         Err(_) => false,
//     }
// }

// // #[quickcheck]
// // fn modul_parses_all(modul: Modul) -> bool {
// //     modul_parses(modul)
// // }

// #[test]
// fn modul_parses_some() {
//     assert!(modul_parses(modul().easy_parse("include Symbol;").unwrap().0));
//     assert!(modul_parses(modul().easy_parse("
//         x -> 2;
//         a <- b;
//     ").unwrap().0));
//     assert!(modul_parses(modul().easy_parse("
//         fact 0 -> 1;
//         fact n -> times n fact(minus n 1);
//     ").unwrap().0));
//     assert!(modul_parses(modul().easy_parse("
//         factorial list -> map {
//             fact 0 <- 1;
//             fact n -> times n fact(minus n 1);
//         } list;
//     ").unwrap().0));
// }
