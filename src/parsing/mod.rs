mod examples;

use crate::syntax::sequential::{Lit, Modul, Name, Pat, PatHead, PatOp, CopatOp, Copat, ExprHead, ExprTail, ExprOp, Expr, Decl};

extern crate combine;

use combine::{
    chainl1, eof, many1, sep_by,
    parser::{char::{char, digit, spaces, lower, string}, repeat::take_until, self, sequence, range::take_while},
    value, EasyParser, Parser, StdParseResult, Stream, attempt, between, many, parser::char::upper, satisfy, none_of
};

fn natural<I>() -> impl Parser<I, Output = i64>
where
    I: Stream<Token = char>,
{
    many1(digit()).map(|s: String| s.parse::<i64>().unwrap())
}

fn sign<I>() -> impl Parser<I, Output = i64>
where
    I: Stream<Token = char>,
{
    (char('-').with(value(-1)))
        .or(char('+').with(value(1)))
        .or(value(1))
}

fn integer<I>() -> impl Parser<I, Output = i64>
where
    I: Stream<Token = char>,
{
    sign().and(natural()).map(|(s, n)| s * n)
}

fn float_digits<I>() -> impl Parser<I, Output = f64>
where
    I: Stream<Token = char>,
{
    many1(digit())
        .skip(char('.'))
        .and(many1(digit()))
        .map(|(s1, s2): (String, String)| {
                let s = s1 + "." + s2.as_str();
                s.parse::<f64>().unwrap()
            }
        )
}

fn float<I>() -> impl Parser<I, Output = f64>
where
    I: Stream<Token = char>,
{
    sign().and(float_digits()).map(|(s, f)| (s as f64) * f)
}

fn string_quoted<I>() -> impl Parser<I, Output = String>
where
    I: Stream<Token = char>,
{
    
    let string_char = none_of("\"\\".chars());
    let escape_sequence = char('\\')
        .with(char('"').or(char('\\')).or(char('n').map(|_| '\n')));
    // look for other escape sequences

    char('"')
        .with(many(string_char.or(escape_sequence)))
        .skip(char('"'))
}

fn symbol<I>() -> impl Parser<I, Output = Name> 
where
    I: Stream<Token = char>
{
    upper().and(many(satisfy(|c: char| c.is_alphanumeric() || c == '_' || c == '\''))).map(
        |(c, mut s): (char, String)| {
            s.insert(0, c);
            Name::id(&s)
        }
    )
}

fn lit<I>() -> impl Parser<I, Output = Lit> 
where
    I: Stream<Token = char>
{
    attempt(float().map(|f| Lit::Flt(f)))
        .or(integer().map(|i| Lit::Int(i)))
        .or(string_quoted().map(|i| Lit::Str(i)))
        .or(symbol().map(|name| Lit::Sym(name)))
}

#[test]
fn lit_test() {
    assert_eq!(
        lit().easy_parse("1234").map(|(v, _s)| v),
        Ok(Lit::Int(1234))
    );
    assert_eq!(
        lit().easy_parse("0").map(|(v, _s)| v),
        Ok(Lit::Int(0))
    );

    assert_eq!(
        lit().easy_parse("1.00001").map(|(v, _s)| v),
        Ok(Lit::Flt(1.00001))
    );

    assert_eq!(
        lit().easy_parse(r#""string\" \\ test!2@""#).map(|(v, _s)| v),
        Ok(Lit::Str(r#"string" \ test!2@"#.to_owned()))
    );

    assert_eq!(
        lit().easy_parse("Testvar").map(|(v, _s)| v),
        Ok(Lit::Sym( Name{ id: "Testvar".to_owned() }) )
    );
    assert_eq!(
        lit().easy_parse("Name").map(|(v, _s)| v),
        Ok(Lit::Sym( Name{ id: "Name".to_owned() }) )
    );
}

fn wildcard_pat<I>() -> impl Parser<I, Output = char> 
where
    I: Stream<Token = char>
{
    char('_')
}

fn variable<I>() -> impl Parser<I, Output = Name> 
where
    I: Stream<Token = char>
{
    lower().and(many(satisfy(|c: char| c.is_alphanumeric() || c == '_' || c == '\'')))
    .map(
        |(c, mut s): (char, String)| {
            s.insert(0, c);
            Name::id(&s)
        }
    )
}

fn pat_head<I>() -> impl Parser<I, Output = PatHead> 
where
    I: Stream<Token = char>
{
    wildcard_pat().map(|_| PatHead::Unused)
        .or(variable().map(|name| PatHead::Var(name)))
        .or(lit().map(|v| PatHead::Const(v)))
        .skip(spaces())
}

fn pat_op<I>() -> impl Parser<I, Output = PatOp>
where
    I: Stream<Token = char> 
{
    let pat_: fn(&mut I) -> StdParseResult<Pat, I> 
        = |input| pat().parse_stream(input).into();

    between(char('(').skip(spaces()), char(')').skip(spaces()), pat_)
        .map(|p| PatOp::App(p))
}

fn pat<I>() -> impl Parser<I, Output = Pat> 
where
    I: Stream<Token = char>
{
    pat_head().and(many(pat_op())).map(
        |(pat, tail)| {
            pat.head().extend(tail)
        }
    )
}

#[test]
fn pat_test() {
    assert_eq!(
        wildcard_pat().easy_parse("_").map(|(v, _s)| v),
        Ok('_')
    );

    assert_eq!(
        variable().easy_parse("x").map(|(v, _s)| v),
        Ok(Name::id("x"))
    );

    assert_eq!(
        pat_head().easy_parse("_").map(|(v, _s)| v),
        Ok(PatHead::Unused)
    );
    assert_eq!(
        pat_head().easy_parse("x").map(|(v, _s)| v),
        Ok(PatHead::Var(Name::id("x")))
    );
    assert_eq!(
        pat_head().easy_parse("X").map(|(v, _s)| v),
        Ok(PatHead::Const(Name::id("X").sym()))
    );
    assert_eq!(
        pat_head().easy_parse("45").map(|(v, _s)| v),
        Ok(PatHead::Const(Lit::Int(45)))
    );
    assert_eq!(
        pat_head().easy_parse("\"patterns\"").map(|(v, _s)| v),
        Ok(PatHead::Const(Lit::Str("patterns".to_owned())))
    );
    
    assert_eq!(
        pat().easy_parse("_").map(|(v, _s)| v),
        Ok(Pat::blank())
    );
    assert_eq!(
        pat().easy_parse("f").map(|(v, _s)| v),
        Ok(PatHead::Var(Name::id("f")).head())
    );
    assert_eq!(
        pat().easy_parse("g(1)").map(|(v, _s)| v),
        Ok(PatHead::Var(
            Name::id("g")).head()
            .app(PatHead::Const(Lit::Int(1)).head()))
    );
    assert_eq!(
        pat().easy_parse("f(\"argrz\")(\"arg2\")").map(|(v, _s)| v),
        Ok(PatHead::Var(
            Name::id("f")).head()
            .app(PatHead::Const(Lit::Str("argrz".to_owned())).head())
            .app(PatHead::Const(Lit::Str("arg2".to_owned())).head()))
    );
    assert_eq!(
        pat().easy_parse("h(X(0)(_))(1.002)").map(|(v, _s)| v),
        Ok(PatHead::Var(
            Name::id("h")).head()
            .app(PatHead::Const(Lit::Sym(Name::id("X"))).head()
                .app(PatHead::Const(Lit::Int(0)).head())
                .app(PatHead::Unused.head()))
            .app(PatHead::Const(Lit::Flt(1.002)).head()))
    );
    assert_eq!(
        pat().easy_parse("h( X(0)(_)) 
                            (1.002) 
                            (v)"
        ).map(|(v, _s)| v),
        Ok(PatHead::Var(
            Name::id("h")).head()
            .app(PatHead::Const(Lit::Sym(Name::id("X"))).head()
                .app(PatHead::Const(Lit::Int(0)).head())
                .app(PatHead::Unused.head()))
            .app(PatHead::Const(Lit::Flt(1.002)).head())
            .app(PatHead::Var(Name::id("v")).head()))
    );
    
}

fn copat_op<I>() -> impl Parser<I, Output = CopatOp>
where
    I: Stream<Token = char>,
{
    let copat_: fn(&mut I) -> StdParseResult<Pat, I>
        = |input| pat().parse_stream(input).into();

    between(char('(').skip(spaces()), char(')'), copat_)
        .map(|p| CopatOp::App(p))
        .or(char('.').with(lit().map(|v| CopatOp::Dot(v))))
        .skip(spaces())
}

fn copat<I>() -> impl Parser<I, Output = Copat>
where
    I: Stream<Token = char>,
{
    pat().and(many(copat_op())).map(
        |(pat, tail)| {
		    pat.this().extend(tail)
        }
    )
}

#[test]
fn copat_op_test() {
    assert_eq!(
        copat_op().easy_parse("(xariable)").map(|(v, _s)| v),
        Ok(CopatOp::App(Name::id("xariable").bind()))
    );

    assert_eq!(
        copat_op().easy_parse(".X").map(|(v, _s)| v),
        Ok(CopatOp::Dot(Lit::Sym(Name::id("X"))))
    );

    assert_eq!(
        copat().easy_parse("10(y)").map(|(v, _s)| v),
        Ok(
            Lit::Int(10).mtch()
                .app(Name::id("y").bind()).this()
        )
    );
    assert_eq!(
        copat().easy_parse("X.2").map(|(v, _s)| v),
        Ok(
            Lit::Sym(Name::id("X")).mtch().this()
                .dot(Lit::int(2))
        )
    );
    assert_eq!(
        copat().easy_parse("2.X").map(|(v, _s)| v),
        Ok(
            Lit::int(2).mtch().this()
                .dot(Lit::Sym(Name::id("X")))
        )
    );
}


#[test]
fn copat_test() {
    assert_eq!(
        copat().easy_parse("_").map(|(v, _s)| v),
        Ok(Pat::blank().this())
    );
    assert_eq!(
        copat().easy_parse("10.X").map(|(v, _s)| v),
        Ok(
            Lit::Int(10).mtch().this()
                .dot(Name::id("X").sym())
        )
    );
    assert_eq!(
        copat().easy_parse("x.20.Z").map(|(v, _s)| v),
        Ok(
            Name::id("x").bind().this()
                .dot(Lit::Int(20))
                .dot(Name::id("Z").sym())
        )
    );

}

fn expr_head<I>() -> impl Parser<I, Output = ExprHead> 
where
    I: Stream<Token = char>
{
    variable().map(|n: Name| ExprHead::Var(n))
        .or(lit().map(|l: Lit| ExprHead::Const(l)))
        .skip(spaces())
}

fn expr_op<I>() -> impl Parser<I, Output = ExprOp> 
where
    I: Stream<Token = char>
{ 
    let expr_: fn(&mut I) -> StdParseResult<Expr, I> 
        = |input| expr().parse_stream(input).into();

    between(
        char('(').skip(spaces()), 
        char(')'), 
        expr_.map(|e| ExprOp::App(e)))
    .or(
        spaces()
        .with(char('.').skip(spaces()).with(lit().map(|l| ExprOp::Dot(l)))))
    .skip(spaces())
}

fn expr_tail<I>() -> impl Parser<I, Output = ExprTail> 
where
    I: Stream<Token = char>
{
    many(expr_op())
}

fn expr<I>() -> impl Parser<I, Output = Expr> 
where
    I: Stream<Token = char>
{
    expr_head().and(expr_tail())
        .map(|(h, t): (ExprHead, ExprTail)| h.head().extend(t))
}

#[test]
fn expr_test() {
    assert_eq!(
        expr().parse("2").map(|(v, _s)| v),
        Ok(Lit::int(2).cnst())
    );
    assert_eq!(
        expr().parse(r#""str""#).map(|(v, _s)| v),
        Ok(Lit::str("str".to_owned()).cnst())
    );
    assert_eq!(
        expr().parse("f(3)").map(|(v, _s)| v),
        Ok(
            Name::id("f").refer()
                .app(Lit::int(3).cnst())
        )
    );
    assert_eq!(
        expr().parse(r#"Class(3)("arg1").Symbol("arg2").1"#).map(|(v, _s)| v),
        Ok(
            Name::id("Class").sym().cnst()
                .app(Lit::int(3).cnst())
                .app(Lit::str("arg1".to_owned()).cnst())
                .dot(Name::id("Symbol").sym())
                    .app(Lit::str("arg2".to_owned()).cnst())
                .dot(Lit::int(1))
        )
    );
    assert_eq!(
        expr().parse(r#"Class(3)("arg1")
                        .Symbol("arg2").1"#).map(|(v, _s)| v),
        Ok(
            Name::id("Class").sym().cnst()
                .app(Lit::int(3).cnst())
                .app(Lit::str("arg1".to_owned()).cnst())
                .dot(Name::id("Symbol").sym())
                    .app(Lit::str("arg2".to_owned()).cnst())
                .dot(Lit::int(1))
        )
    );
}

fn decl<I>() -> impl Parser<I, Output = Decl> 
where
    I: Stream<Token = char>
{
    attempt(string("include").skip(spaces()).with(expr())
        .map(|expr| Decl::Include(expr)))
    .or(
        copat()
            .skip(char('='))
            .skip(spaces())
            .and(expr())
            .map(|(copat, expr)| Decl::Method(copat, expr))
    )
}

#[test]
fn decl_test() {
    assert_eq!(
        decl().parse("include var").map(|(v, _s)| v),
        Ok(
            Decl::Include(Name::id("var").refer())
        )
    );
    assert_eq!(
        decl().parse("include Sym").map(|(v, _s)| v),
        Ok(
            Decl::Include(Name::id("Sym").sym().cnst())
        )
    );

    assert_eq!(
        decl().easy_parse("five = 5").map(|(v, _s)| v),
        Ok(
            Decl::Method(Name::id("five").bind().this(), Lit::Int(5).cnst())
        )
    );
}

fn decl_list<I>() -> impl Parser<I, Output = Vec<Decl>>
where
    I: Stream<Token = char>,
{
    many(decl().skip(char(';')).skip(spaces()))
}

fn modul<I>() -> impl Parser<I, Output = Modul>
where
    I: Stream<Token = char>,
{
    spaces().with(decl_list().map(|defns| Modul {defns}))
}

#[test]
fn modul_test() {
    let input = "
        x = 1;
        y = 2;
        include 3;
    ";

    let expected_defns = vec![
        Decl::Method(
            Name::id("x").bind().this(),
            Lit::Int(1).cnst(),
        ),
        Decl::Method(
            Name::id("y").bind().this(),
            Lit::Int(2).cnst(),
        ),
        Decl::Include(Lit::Int(3).cnst()),
    ];

    let result = modul().easy_parse(input);

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
