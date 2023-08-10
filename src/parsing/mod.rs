mod examples;

use crate::syntax::sequential::{
    Copat, CopatOp, Decl, Decons, DeconsOp, Expr, ExprHead, ExprOp, ExprTail, Lit, Modul, Name, Pat,
};

extern crate combine;

use combine::{
    attempt, between, chainl1, eof, many, many1, none_of,
    parser::char::upper,
    parser::{
        self,
        char::{char, digit, lower, spaces, string},
        range::take_while,
        repeat::take_until,
        sequence,
    },
    satisfy, sep_by, value, EasyParser, Parser, StdParseResult, Stream,
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
        })
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
    let escape_sequence = char('\\').with(char('"').or(char('\\')).or(char('n').map(|_| '\n')));
    // look for other escape sequences

    char('"')
        .with(many(string_char.or(escape_sequence)))
        .skip(char('"'))
}

fn symbol<I>() -> impl Parser<I, Output = Name>
where
    I: Stream<Token = char>,
{
    upper()
        .and(many(satisfy(|c: char| {
            c.is_alphanumeric() || c == '_' || c == '\''
        })))
        .map(|(c, mut s): (char, String)| {
            s.insert(0, c);
            Name::id(&s)
        })
}

fn lit<I>() -> impl Parser<I, Output = Lit>
where
    I: Stream<Token = char>,
{
    attempt(float().map(Lit::Flt))
        .or(integer().map(Lit::Int))
        .or(string_quoted().map(Lit::Str))
        .or(symbol().map(Lit::Sym))
}

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

fn wildcard_pat<I>() -> impl Parser<I, Output = char>
where
    I: Stream<Token = char>,
{
    char('_')
}

fn variable<I>() -> impl Parser<I, Output = Name>
where
    I: Stream<Token = char>,
{
    lower()
        .and(many(satisfy(|c: char| {
            c.is_alphanumeric() || c == '_' || c == '\''
        }))).skip(spaces())
        .map(|(c, mut s): (char, String)| {
            s.insert(0, c);
            Name::id(&s)
        })
}

fn decons<I>() -> impl Parser<I, Output = Decons>
where
    I: Stream<Token = char>,
{
    lit()
        .and(many(decons_op()))
        .map(|(c, ops)| c.mtch().extend(ops))
}

fn decons_op<I>() -> impl Parser<I, Output = DeconsOp>
where
    I: Stream<Token = char>,
{
    let pat_: fn(&mut I) -> StdParseResult<Pat, I> = |input| pat().parse_stream(input).into();

    between(char('(').skip(spaces()), char(')').skip(spaces()), pat_).map(DeconsOp::App)
}

#[test]
fn decons_op_test() {
    assert_eq!(
        decons_op().easy_parse("(x)").map(|(v, _s)| v),
        Ok(DeconsOp::App(Name::id("x").bind()))
    );

    assert_eq!(
        decons_op().easy_parse("(X)").map(|(v, _s)| v),
        Ok(DeconsOp::App(Name::id("X").sym().mtch().decons()))
		);
}

#[test]
fn decons_test() {
    assert_eq!(
        decons().easy_parse("123").map(|(v, _s)| v),
        Ok(Lit::int(123).mtch())
    );

    assert_eq!(
        decons().easy_parse("\"text\"").map(|(v, _s)| v),
        Ok(Lit::str("text".to_owned()).mtch())
    );

    assert_eq!(
        decons().easy_parse("F(x)(y)").map(|(v, _s)| v),
        Ok((Name::id("F").sym().mtch())
            .app(Name::id("x").bind())
            .app(Name::id("y").bind())
					 )
    );

    assert_eq!(
        decons().easy_parse("G(1)(a)(b)").map(|(v, _s)| v),
        Ok((Name::id("G").sym().mtch())
            .app(Lit::int(1).mtch().decons())
            .app(Name::id("a").bind())
            .app(Name::id("b").bind())
            )
    );
}

fn pat<I>() -> impl Parser<I, Output = Pat>
where
    I: Stream<Token = char>,
{
    (wildcard_pat().map(|_| Pat::blank()))
        .or(variable().map(Pat::Var))
        .or(decons().map(Pat::Struc))
}

#[test]
fn pat_test() {
    assert_eq!(wildcard_pat().easy_parse("_").map(|(v, _s)| v), Ok('_'));

    assert_eq!(
        variable().easy_parse("x").map(|(v, _s)| v),
        Ok(Name::id("x"))
    );

    assert_eq!(pat().easy_parse("_").map(|(v, _s)| v), Ok(Pat::blank()));
    assert_eq!(
        pat().easy_parse("x").map(|(v, _s)| v),
        Ok(Pat::Var(Name::id("x")))
    );

    assert_eq!(
        decons().easy_parse("X").map(|(v, _s)| v),
        Ok(Name::id("X").sym().mtch())
    );
    assert_eq!(
        decons().easy_parse("45").map(|(v, _s)| v),
        Ok(Lit::Int(45).mtch())
    );
    assert_eq!(
        decons().easy_parse("\"patterns\"").map(|(v, _s)| v),
        Ok(Lit::Str("patterns".to_owned()).mtch())
    );

    assert_eq!(
        pat().easy_parse("X").map(|(v, _s)| v),
        Ok(Name::id("X").sym().mtch().decons())
    );
    assert_eq!(
        pat().easy_parse("45").map(|(v, _s)| v),
        Ok(Lit::Int(45).mtch().decons())
    );
    assert_eq!(
        pat().easy_parse("\"patterns\"").map(|(v, _s)| v),
        Ok(Lit::Str("patterns".to_owned()).mtch().decons())
    );

    assert_eq!(
        decons_op().easy_parse("(1)").map(|(v, _s)| v),
        Ok(DeconsOp::App(Lit::Int(1).mtch().decons()))
    );
    assert_eq!(
        decons().easy_parse("G(1)").map(|(v, _s)| v),
        Ok(Name::id("G").sym().mtch().app(Lit::Int(1).mtch().decons()))
    );
    assert_eq!(
        decons()
            .easy_parse("F(\"argrz\")(\"arg2\")")
            .map(|(v, _s)| v),
        Ok((Name::id("F").sym().mtch())
            .app(Lit::Str("argrz".to_owned()).mtch().decons())
            .app(Lit::Str("arg2".to_owned()).mtch().decons()))
    );
    assert_eq!(
        decons().easy_parse("H(X(0)(_))(1.002)").map(|(v, _s)| v),
        Ok((Name::id("H").sym().mtch())
            .app(
                (Lit::Sym(Name::id("X")).mtch())
                    .app(Lit::Int(0).mtch().decons())
                    .app(Pat::Unused)
                    .decons()
            )
            .app(Lit::Flt(1.002).mtch().decons()))
    );
    assert_eq!(
        pat()
            .easy_parse(
                "H( X(0)(_)) 
                            (1.002) 
                            (v)"
            )
            .map(|(v, _s)| v),
        Ok((Name::id("H").sym().mtch())
            .app(
                (Lit::Sym(Name::id("X")).mtch())
                    .app(Lit::Int(0).mtch().decons())
                    .app(Pat::Unused)
                    .decons()
            )
            .app(Lit::Flt(1.002).mtch().decons())
            .decons())
    );
}

fn copat_op<I>() -> impl Parser<I, Output = CopatOp>
where
    I: Stream<Token = char>,
{
    let copat_: fn(&mut I) -> StdParseResult<Pat, I> = |input| pat().parse_stream(input).into();

    between(char('(').skip(spaces()), char(')'), copat_)
        .map(|p| CopatOp::App(p))
        .or(char('.').with(lit().map(CopatOp::Dot)))
        .skip(spaces())
}

fn copat<I>() -> impl Parser<I, Output = Copat>
where
    I: Stream<Token = char>,
{
    pat()
        .and(many(copat_op()))
        .map(|(pat, tail)| pat.this().extend(tail))
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
        Ok(Lit::Int(10)
            .mtch()
            .app(Name::id("y").bind())
            .decons()
            .this())
    );
    assert_eq!(
        copat().easy_parse("X.2").map(|(v, _s)| v),
        Ok(Lit::Sym(Name::id("X"))
            .mtch()
            .decons()
            .this()
            .dot(Lit::int(2)))
    );
    assert_eq!(
        copat().easy_parse("2.X").map(|(v, _s)| v),
        Ok(Lit::int(2)
            .mtch()
            .decons()
            .this()
            .dot(Lit::Sym(Name::id("X"))))
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
        Ok(Lit::Int(10).mtch().decons().this().dot(Name::id("X").sym()))
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

fn expr_head<I>() -> impl Parser<I, Output = ExprHead>
where
    I: Stream<Token = char>,
{
    let modul_: fn(&mut I) -> StdParseResult<Modul, I> = |input| modul().parse_stream(input).into();

    (variable().map(|n: Name| ExprHead::Var(n)))
        .or(lit().map(|l: Lit| ExprHead::Const(l)))
        .or(between(char('{').skip(spaces()), char('}'), modul_).map(|f| ExprHead::Lambda(f)))
        .skip(spaces())
}

fn expr_op<I>() -> impl Parser<I, Output = ExprOp>
where
    I: Stream<Token = char>,
{
    let expr_: fn(&mut I) -> StdParseResult<Expr, I> = |input| expr().parse_stream(input).into();

    between(
        char('(').skip(spaces()),
        char(')'),
        expr_.map(|e| ExprOp::App(e)),
    )
    .or(spaces().with(char('.').skip(spaces()).with(lit().map(|l| ExprOp::Dot(l)))))
    .skip(spaces())
}

fn expr_tail<I>() -> impl Parser<I, Output = ExprTail>
where
    I: Stream<Token = char>,
{
    many(expr_op())
}

fn expr<I>() -> impl Parser<I, Output = Expr>
where
    I: Stream<Token = char>,
{
    expr_head()
        .and(expr_tail())
        .map(|(h, t): (ExprHead, ExprTail)| h.head().extend(t))
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

fn decl<I>() -> impl Parser<I, Output = Decl>
where
    I: Stream<Token = char>,
{
    attempt(
        string("include")
            .skip(spaces())
            .with(expr())
            .map(|expr| Decl::Include(expr)),
    )
    .or(copat()
        .skip(char('='))
        .skip(spaces())
        .and(expr())
        .map(|(copat, expr)| Decl::Method(copat, expr)))
}

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
        decl().easy_parse("five = 5").map(|(v, _s)| v),
        Ok(Decl::Method(
            Name::id("five").bind().this(),
            Lit::Int(5).cnst()
        ))
    );
}

fn decl_list<I>() -> impl Parser<I, Output = Vec<Decl>>
where
    I: Stream<Token = char>,
{
    many(decl().skip(char(';')).skip(spaces()))
}

pub fn modul<I>() -> impl Parser<I, Output = Modul>
where
    I: Stream<Token = char>,
{
    decl_list().map(|defns| Modul { defns })
}

#[test]
fn modul_test() {
    let input = "
        include Symbol;
        x = 1;
        y = 2;
    ";

    let expected_defns = vec![
        Decl::Include(Name::id("Symbol").sym().cnst()),
        Decl::Method(Name::id("x").bind().this(), Lit::Int(1).cnst()),
        Decl::Method(Name::id("y").bind().this(), Lit::Int(2).cnst()),
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

pub fn whole_input<I, P>(p: P) -> impl Parser<I, Output = P::Output>
where
    I: Stream<Token = char>,
    P: Parser<I>,
{
    spaces().with(p).skip(spaces()).skip(eof())
}
