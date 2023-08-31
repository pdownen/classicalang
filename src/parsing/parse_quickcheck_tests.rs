extern crate quickcheck;

use std::io::LineWriter;

use combine::{EasyParser, Parser, Stream};
use num::Integer;
use quickcheck_macros::quickcheck;

use crate::{
    parsing::parse::*,
    syntax::sequential::{
        AtomicPat, Copat, CopatOp, Decl, Decons, DeconsOp, Expr, ExprHead, ExprOp, Lit, Modul,
        Name, Pat, PrettyPrint, MAX_LINE_WIDTH,
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
            while !c.is_ascii_alphabetic() {
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
        match self.id.len() {
            0 | 1 => empty_shrinker(),
            _ => Box::new(
                self.id
                    .shrink()
                    .filter(|s| {
                        s.len() > 0
                            && s.starts_with(|c: char| c.is_alphabetic())
                            && s.chars().all(|c| c.is_alphanumeric() || c == '_')
                    })
                    .map(|s| Name { id: s }),
            ),
        }
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
            Lit::Sym(s) => match s.id.len() {
                0 | 1 => empty_shrinker(),
                _ => Box::new(
                    s.shrink()
                        .filter(|n| n.id.starts_with(|c: char| c.is_uppercase()))
                        .map(|n| n.sym()),
                ),
            },
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
        Ok((f, _s)) => Lit::flt(num) == f,
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
    assert!(lit_parses(Lit::str("".to_owned()), |s| s.to_string()));
    assert!(lit_parses(Lit::str("\n\nok\n".to_owned()), |s| s.to_string()));
}

fn lit_parses(literal: Lit, printer: impl Fn(&Lit) -> String) -> bool {
    let printed = printer(&literal);
    let parsed = lit().easy_parse(printed.as_str());

    match parsed {
        Ok((Lit::Flt(f), _s)) if f.is_nan() => printed.parse::<f64>().unwrap().is_nan(),
        Ok((v, _s)) => v == literal,
        Err(_) => false,
    }
}

#[quickcheck]
fn lit_parses_basic(lit: Lit) -> bool {
    lit_parses(lit, |l| l.to_string())
}

#[quickcheck]
fn lit_parses_pretty(lit: Lit) -> bool {
    lit_parses(lit, |l| l.to_pretty(MAX_LINE_WIDTH))
}

#[test]
fn lit_parses_some() {
    let format_printer = |l: &Lit| l.to_string();

    assert!(lit_parses(Lit::int(0), format_printer));
    assert!(lit_parses(Lit::int(25), format_printer));
    assert!(lit_parses(Lit::flt(0.0), format_printer));
    assert!(lit_parses(Lit::flt(0.0001235), format_printer));
    assert!(lit_parses(
        Lit::str("!!@  #### rZ".to_owned()),
        format_printer
    ));
    assert!(lit_parses(
        Lit::str(r#"\\ \" \\"#.to_owned()),
        format_printer
    ));
    assert!(lit_parses(Lit::sym(Name::id("Symbol")), format_printer));
    assert!(lit_parses(
        Lit::sym(Name::id("Symbolic_name2")),
        format_printer
    ));
}

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
        if self.tail.len() > 1 {
            return Box::new(
                (self.head.clone(), self.tail.clone())
                    .shrink()
                    .map(|(h, t)| h.mtch().extend(t)),
            );
        }
        empty_shrinker()
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
            AtomicPat::Var(v) => match v.id.len() {
                0 | 1 => empty_shrinker(),
                _ => Box::new(
                    v.shrink()
                        .filter(|n| n.id.starts_with(|c: char| c.is_lowercase()))
                        .map(|n| n.bind()),
                ),
            },
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

fn pat_parses(pattern: Pat, printer: impl Fn(&Pat) -> String) -> bool {
    let printed = printer(&pattern);
    let parsed = pat().easy_parse(printed.as_str());

    match parsed {
        Ok((v, _s)) => v == pattern,
        Err(_) => false,
    }
}

#[quickcheck]
fn pat_parses_basic(pat: Pat) -> bool {
    pat_parses(pat, |p| p.to_string())
}

#[quickcheck]
fn pat_parses_all(pat: Pat) -> bool {
    pat_parses(pat, |p| p.to_pretty(MAX_LINE_WIDTH))
}

#[test]
fn pat_parses_some() {
    // assert!(pat_parses(Pat::Struc(
    //     Name::id("Sym1").sym().mtch().extend(vec![
    //         DeconsOp::App(Pat::Struc(
    //             Name::id("Sym2")
    //                 .sym()
    //                 .mtch()arb
    //                 .push(DeconsOp::App(Pat::Struc(Lit::int(1).mtch())))
    //         )),
    //         DeconsOp::App(Pat::blank()),
    //         DeconsOp::App(Pat::blank())
    //     ])
    // ), |p| p.to_string()));
}

impl Arbitrary for CopatOp {
    fn arbitrary(g: &mut Gen) -> Self {
        match u32::arbitrary(g) % 2 {
            0 => CopatOp::App(Pat::arbitrary(g)),
            _ => loop {
                let lit = Lit::arbitrary(g);

                if let Lit::Str(_) = lit.clone() {
                    return CopatOp::Dot(lit);
                }
                if let Lit::Sym(_) = lit.clone() {
                    return CopatOp::Dot(lit);
                }
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

fn copat_parses(copattern: Copat, printer: impl Fn(&Copat) -> String) -> bool {
    let printed = printer(&copattern);
    let parsed = copat().easy_parse(printed.as_str());

    match parsed {
        Ok((v, _s)) => v == copattern,
        Err(_) => false,
    }
}

#[quickcheck]
fn copat_parses_basic(copat: Copat) -> bool {
    copat_parses(copat, |c| c.to_string())
}

#[quickcheck]
fn copat_parses_pretty(copat: Copat) -> bool {
    copat_parses(copat, |c| c.to_pretty(MAX_LINE_WIDTH))
}

#[test]
fn copat_parses_some() {
    // assert!(copat_parses(
    //     copat().easy_parse("Sym1 (Sym2 a b c)").unwrap().0
    // ));
    // assert!(copat_parses(
    //     (Name::id("Symbol").sym().switch().this())
    //         .app(Name::id("x").bind().atom())
    //         .app(Name::id("y").bind().atom())
    // ));
}

impl Arbitrary for ExprHead {
    fn arbitrary(g: &mut Gen) -> Self {
        match u32::arbitrary(g) % 7 {
            0 | 1 | 2 => {
                let mut c: char = char::arbitrary(g);
                while !c.is_ascii_alphabetic() {
                    c = char::arbitrary(g);
                }
                let name = format!("{}{}", c.to_lowercase(), Name::arbitrary(g));
                ExprHead::Var(Name::id(name.as_str()))
            }
            3 | 4 | 5 => {
                let mut lit = Lit::arbitrary(g);
                while let Lit::Flt(x) = lit {
                    if x.is_infinite() {
                        lit = Lit::arbitrary(g);
                    } else {
                        break;
                    }
                }

                ExprHead::Const(lit)
            }
            _ => ExprHead::Lambda(Modul::arbitrary(g)),
        }
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        match self {
            ExprHead::Var(v) => match v.id.len() {
                0 | 1 => empty_shrinker(),
                _ => Box::new(
                    v.shrink()
                        .filter(|n| n.id.starts_with(|c: char| c.is_lowercase()))
                        .map(ExprHead::Var),
                ),
            },
            ExprHead::Const(l) => Box::new(l.shrink().map(ExprHead::Const)),
            ExprHead::Lambda(m) => Box::new(m.shrink().map(ExprHead::Lambda)),
        }
    }
}

impl Arbitrary for ExprOp {
    fn arbitrary(g: &mut Gen) -> Self {
        match u32::arbitrary(g) % 2 {
            0 => ExprOp::App(Expr::arbitrary(g)),
            _ => loop {
                let lit = Lit::arbitrary(g);

                if let Lit::Str(_) = lit.clone() {
                    return ExprOp::Dot(lit);
                }
                if let Lit::Sym(_) = lit.clone() {
                    return ExprOp::Dot(lit);
                }
            },
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
        if self.tail.len() > 1 {
            return Box::new(
                (self.head.clone(), self.tail.clone())
                    .shrink()
                    .map(|(h, t)| h.head().extend(t)),
            );
        }
        empty_shrinker()
    }
}

fn expr_parses(expression: Expr, printer: impl Fn(&Expr) -> String) -> bool {
    let printed = printer(&expression);
    let parsed = expr().easy_parse(printed.as_str());

    match parsed {
        Ok((v, _s)) => v == expression,
        Err(_) => false,
    }
}

#[quickcheck]
fn expr_parses_basic(expr: Expr) -> bool {
    expr_parses(expr, |e| e.to_string())
}

#[quickcheck]
fn expr_parses_pretty(expr: Expr) -> bool {
    expr_parses(expr, |e| e.to_pretty(MAX_LINE_WIDTH))
}

#[test]
fn expr_parses_some() {
    assert!(expr_parses(Name::id("x").refer(), |e| e.to_string()));
    assert!(expr_parses(Lit::int(3).cnst(), |e| e.to_string()));
}

impl Arbitrary for Decl {
    fn arbitrary(g: &mut Gen) -> Self {
        match u32::arbitrary(g) % 3 {
            0 => Decl::Include(Expr::arbitrary(g)),
            1 => Decl::Method(Copat::arbitrary(g), Expr::arbitrary(g)),
            _ => Decl::Bind(Pat::arbitrary(g), Expr::arbitrary(g)),
        }
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        match self {
            Decl::Include(e) => Box::new(e.shrink().map(Decl::Include)),
            Decl::Method(c, e) => Box::new(
                (c.clone(), e.clone())
                    .shrink()
                    .map(|(c, e)| Decl::Method(c, e)),
            ),
            Decl::Bind(p, e) => Box::new(
                (p.clone(), e.clone())
                    .shrink()
                    .map(|(p, e)| Decl::Bind(p, e)),
            ),
        }
    }
}

fn decl_parses(declaration: Decl, printer: impl Fn(&Decl) -> String) -> bool {
    let printed = printer(&declaration);
    let parsed = decl().easy_parse(printed.as_str());

    match parsed {
        Ok((v, _s)) => v == declaration,
        Err(_) => false,
    }
}

#[quickcheck]
fn decl_parses_basic(decl: Decl) -> bool {
    decl_parses(decl, |p| p.to_string())
}

#[quickcheck]
fn decl_parses_pretty(decl: Decl) -> bool {
    decl_parses(decl, |p| p.to_pretty(MAX_LINE_WIDTH))
}

#[test]
fn decl_parses_some() {
    assert_eq!(
        decl().easy_parse("include Symbol").map(|(v, _s)| v),
        Ok(Decl::Include(Name::id("Symbol").sym().cnst()))
    );
    assert_eq!(
        decl().easy_parse("Symbol(x)(y)(z) -> x").map(|(v, _s)| v),
        Ok(Decl::Method(
            Name::id("Symbol")
                .sym()
                .switch()
                .this()
                .app(Name::id("x").bind().atom())
                .app(Name::id("y").bind().atom())
                .app(Name::id("z").bind().atom()),
            Name::id("x").refer()
        ))
    );
    assert_eq!(
        decl().easy_parse("a <- b").map(|(v, _s)| v),
        Ok(Decl::Bind(
            Name::id("a").bind().atom(),
            Name::id("b").refer()
        ))
    );
}

impl Arbitrary for Modul {
    fn arbitrary(g: &mut Gen) -> Self {
        let mut decls = vec![Decl::arbitrary(g)];
        while u32::arbitrary(g) % 3 == 0 {
            decls.push(Decl::arbitrary(g));
        }
        Modul { defns: decls }
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        Box::new(self.defns.shrink().map(|ds| Modul { defns: ds }))
    }
}

fn modul_parses(module: Modul, printer: impl Fn(&Modul) -> String) -> bool {
    let printed = printer(&module);
    let parsed = modul().easy_parse(printed.as_str());

    match parsed {
        Ok((v, _s)) => {
            println!(
                "{module}{module:?}
            parses as\n{}{v:?}\n",
                printer(&v)
            );

            v == module
        }
        Err(_) => false,
    }
}

#[quickcheck]
fn modul_parses_basic(modul: Modul) -> bool {
    modul_parses(modul, |m| m.to_string())
}

#[quickcheck]
fn modul_parses_pretty(modul: Modul) -> bool {
    modul_parses(modul, |m| m.to_pretty(MAX_LINE_WIDTH))
}

fn modul_parser_parses_str(str: &str) -> bool {
    let result = whole_input(modul()).easy_parse(str);

    result.map_or(false, |(m, _s)| {
        let str2 = m.to_string();
        let result2 = whole_input(modul()).easy_parse(str2.as_str());

        result2.map_or(false, |(m2, _s2)| {
            println!("{str}\n{m:?}\n\tParses as\n{}\n{m2:?}", m2.to_string());
            m == m2
        })
    })
}

#[test]
fn modul_parses_some() {
    assert!(modul_parser_parses_str("include Symbol;"));
    assert!(modul_parser_parses_str(
        "
        x -> 2;
        a <- b;
    "
    ));
    assert!(modul_parser_parses_str(
        "
        fact 0 -> 1;
        fact n -> times n (fact(minus n 1));
    "
    ));

    assert_eq!(
        whole_input(modul())
            .easy_parse(
                "
            factorial list -> map {
                fact 0 -> 1;
                fact n -> times n (fact (minus n 1));
            } list;
        "
            )
            .map(|(v, _s)| v),
        Ok(Modul::top().then(
            Name::id("factorial")
                .bind()
                .this()
                .app(Name::id("list").bind().atom())
                .goes_to(
                    Name::id("map")
                        .refer()
                        .app(
                            Modul::top()
                                .then(
                                    Name::id("fact")
                                        .bind()
                                        .this()
                                        .app(Lit::int(0).switch().atom())
                                        .goes_to(Lit::int(1).cnst())
                                )
                                .then(
                                    Name::id("fact")
                                        .bind()
                                        .this()
                                        .app(Name::id("n").bind().atom())
                                        .goes_to(
                                            Name::id("times")
                                                .refer()
                                                .app(Name::id("n").refer())
                                                .app(
                                                    Name::id("fact").refer().app(
                                                        Name::id("minus")
                                                            .refer()
                                                            .app(Name::id("n").refer())
                                                            .app(Lit::int(1).cnst())
                                                    )
                                                )
                                        )
                                )
                                .lambda()
                        )
                        .app(Name::id("list").refer())
                )
        ))
    );
}
