use combine::EasyParser;

use crate::{
    parsing::parse::*,
    syntax::sequential::{AtomicPat, CopatOp, Decl, DeconsOp, Lit, Name, Pat},
};

use super::parse::{modul, whole_input};

#[test]
fn lit_test() {
    assert_eq!(
        lit().easy_parse("1234").map(|(v, _s)| v),
        Ok(Lit::Int(1234))
    );
    assert_eq!(lit().easy_parse("0").map(|(v, _s)| v), Ok(Lit::Int(0)));

    assert_eq!(
        lit().easy_parse("1.00001").map(|(v, _s)| v),
        Ok(Lit::Flt(1.00001))
    );

    assert_eq!(
        lit()
            .easy_parse(r#""string\" \\ test!2@""#)
            .map(|(v, _s)| v),
        Ok(Lit::Str(r#"string" \ test!2@"#.to_owned()))
    );

    assert_eq!(
        lit().easy_parse("Testvar").map(|(v, _s)| v),
        Ok(Name::id("Testvar").sym())
    );
    assert_eq!(
        lit().easy_parse("Name").map(|(v, _s)| v),
        Ok(Name::id("Name").sym())
    );
}

#[test]
fn decons_op_test() {
    assert_eq!(
        decons_op().easy_parse("x").map(|(v, _s)| v),
        Ok(DeconsOp::App(Name::id("x").bind().atom()))
    );
    assert_eq!(
        decons_op().easy_parse("X").map(|(v, _s)| v),
        Ok(DeconsOp::App(Name::id("X").sym().switch().atom()))
    );
    assert_eq!(
        decons_op().easy_parse("1").map(|(v, _s)| v),
        Ok(DeconsOp::App(Lit::Int(1).switch().atom()))
    );
}

#[test]
fn decons_test() {
    // assert_eq!(
    //     decons().easy_parse("123").map(|(v, _s)| v),
    //     Ok(Lit::int(123).mtch())
    // );

    // assert_eq!(
    //     decons().easy_parse("\"text\"").map(|(v, _s)| v),
    //     Ok(Lit::str("text".to_owned()).mtch())
    // );

    assert_eq!(
        decons().easy_parse("F(x)(y)").map(|(v, _s)| v),
        Ok((Name::id("F").sym().mtch())
            .app(Name::id("x").bind().atom())
            .app(Name::id("y").bind().atom()))
    );

    assert_eq!(
        decons().easy_parse("G(1)(a)(b)").map(|(v, _s)| v),
        Ok((Name::id("G").sym().mtch())
            .app(Lit::int(1).switch().atom())
            .app(Name::id("a").bind().atom())
            .app(Name::id("b").bind().atom()))
    );
}

#[test]
fn pat_test() {
    assert_eq!(wildcard_pat().easy_parse("_").map(|(v, _s)| v), Ok('_'));

    assert_eq!(
        variable().easy_parse("x").map(|(v, _s)| v),
        Ok(Name::id("x"))
    );

    assert_eq!(
        atomic_pat().easy_parse("x").map(|(v, _s)| v),
        Ok(AtomicPat::Var(Name::id("x")))
    );
    assert_eq!(
        atomic_pat().easy_parse("\"patterns\"").map(|(v, _s)| v),
        Ok(Lit::Str("patterns".to_owned()).switch())
    );

    assert_eq!(
        pat().easy_parse("_").map(|(v, _s)| v), 
        Ok(Pat::blank())
    );
    assert_eq!(
        pat().easy_parse("X").map(|(v, _s)| v),
        Ok(Name::id("X").sym().switch().atom())
    );
    assert_eq!(
        pat().easy_parse("45").map(|(v, _s)| v),
        Ok(Lit::Int(45).switch().atom())
    );
    assert_eq!(
        pat().easy_parse("\"patterns\"").map(|(v, _s)| v),
        Ok(Lit::Str("patterns".to_owned()).switch().atom())
    );

    assert_eq!(
        decons().easy_parse("G(1)").map(|(v, _s)| v),
        Ok(Name::id("G")
            .sym().mtch()
            .app(Lit::Int(1).switch().atom()))
    );
    assert_eq!(
        decons()
            .easy_parse("F(\"argrz\")(\"arg2\")")
            .map(|(v, _s)| v),
        Ok((Name::id("F").sym().mtch())
            .app(Lit::Str("argrz".to_owned()).switch().atom())
            .app(Lit::Str("arg2".to_owned()).switch().atom()))
    );

    assert_eq!(
        pat().easy_parse("H(X(0)(_))(1.002)").map(|(v, _s)| v),
        Ok((Name::id("H").sym().mtch())
            .app(
                (Name::id("X").sym().mtch())
                    .app(Lit::Int(0).switch().atom())
                    .app(AtomicPat::Unused.atom())
                    .decons()
            )
            .app(Lit::Flt(1.002).switch().atom())
            .decons())
    );
    assert_eq!(
        pat()
            .easy_parse(
                "H( X 0  _ ) 
                            (1.002) 
                            (v)"
            )
            .map(|(v, _s)| v),
        Ok((Name::id("H").sym().mtch())
            .app(
                (Lit::Sym(Name::id("X")).mtch())
                    .app(Lit::Int(0).switch().atom())
                    .app(AtomicPat::Unused.atom())
                    .decons()
            )
            .app(Lit::Flt(1.002).switch().atom())
            .app(Name::id("v").bind().atom())
            .decons())
    );
}

#[test]
fn copat_op_test() {
    assert_eq!(
        copat_op().easy_parse("(xariable)").map(|(v, _s)| v),
        Ok(CopatOp::App(Name::id("xariable").bind().atom()))
    );

    assert_eq!(
        copat_op().easy_parse(".X").map(|(v, _s)| v),
        Ok(CopatOp::Dot(Lit::Sym(Name::id("X"))))
    );

    assert_eq!(
        copat().easy_parse("10(y)").map(|(v, _s)| v),
        Ok(Lit::Int(10)
            .switch()
            .this()
            .app(Name::id("y").bind().atom()))
    );
    assert_eq!(
        copat().easy_parse("X.2").map(|(v, _s)| v),
        Ok(Lit::Sym(Name::id("X")).switch().this().dot(Lit::int(2)))
    );
    assert_eq!(
        copat().easy_parse("2.X").map(|(v, _s)| v),
        Ok(Lit::int(2).switch().this().dot(Lit::Sym(Name::id("X"))))
    );
}

#[test]
fn copat_test() {
    assert_eq!(
        copat().easy_parse("_").map(|(v, _s)| v),
        Ok(AtomicPat::blank().this())
    );
    assert_eq!(
        copat().easy_parse("10.X").map(|(v, _s)| v),
        Ok(Lit::Int(10).switch().this().dot(Name::id("X").sym()))
    );
    assert_eq!(
        copat().easy_parse("x.20.Z").map(|(v, _s)| v),
        Ok(Name::id("x")
            .bind()
            .this()
            .dot(Lit::Int(20))
            .dot(Name::id("Z").sym()))
    );
}

#[test]
fn expr_test() {
    assert_eq!(
        expr().easy_parse("2").map(|(v, _s)| v),
        Ok(Lit::int(2).cnst())
    );
    assert_eq!(
        expr().easy_parse(r#""str""#).map(|(v, _s)| v),
        Ok(Lit::str("str".to_owned()).cnst())
    );
    assert_eq!(
        expr().easy_parse("f(3)").map(|(v, _s)| v),
        Ok(Name::id("f").refer().app(Lit::int(3).cnst()))
    );
    assert_eq!(
        expr()
            .easy_parse(r#"Class(3)("arg1").Symbol("arg2").1"#)
            .map(|(v, _s)| v),
        Ok((Name::id("Class").sym().cnst())
            .app(Lit::int(3).cnst())
            .app(Lit::str("arg1".to_owned()).cnst())
            .dot(Name::id("Symbol").sym())
            .app(Lit::str("arg2".to_owned()).cnst())
            .dot(Lit::int(1)))
    );
    assert_eq!(
        expr()
            .easy_parse(
                r#"Class(3)("arg1")
                        .Symbol("arg2").1"#
            )
            .map(|(v, _s)| v),
        Ok((Name::id("Class").sym().cnst())
            .app(Lit::int(3).cnst())
            .app(Lit::str("arg1".to_owned()).cnst())
            .dot(Name::id("Symbol").sym())
            .app(Lit::str("arg2".to_owned()).cnst())
            .dot(Lit::int(1)))
    );
}

/*

#[test]
fn decl_test() {
    assert_eq!(
        decl().easy_parse("include var").map(|(v, _s)| v),
        Ok(Decl::Include(Name::id("var").refer()))
    );
    assert_eq!(
        decl().easy_parse("include Sym").map(|(v, _s)| v),
        Ok(Decl::Include(Name::id("Sym").sym().cnst()))
    );

    assert_eq!(
        decl().easy_parse("five <- 5").map(|(v, _s)| v),
        Ok(Decl::Method(
            Name::id("five").bind().this(),
            Lit::Int(5).cnst()
        ))
    );
}

#[test]
fn modul_test() {
    let input = "
        include Symbol;
        x <- 1;
        y <- 2;
    ";

    let expected_defns = vec![
        Decl::Include(Name::id("Symbol").sym().cnst()),
        Decl::Method(Name::id("x").bind().this(), Lit::Int(1).cnst()),
        Decl::Method(Name::id("y").bind().this(), Lit::Int(2).cnst()),
    ];

    let result = whole_input(modul()).easy_parse(input);

    match result {
        Ok((parsed_modul, _)) => {
            assert_eq!(parsed_modul.defns.len(), expected_defns.len());

            for (parsed_decl, expected_decl) in parsed_modul.defns.iter().zip(&expected_defns) {
                assert_eq!(parsed_decl, expected_decl);
            }
        }
        Err(err) => {
            panic!("Parsing error: {:?}", err);
        }
    }
}
*/
