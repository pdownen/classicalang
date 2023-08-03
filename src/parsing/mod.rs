mod examples;

use crate::syntax::sequential::{Lit, Modul, Name, Pat, PatHead, PatOp};

extern crate combine;

use combine::{
    chainl1, eof, many1,
    parser::{char::{char, digit, spaces, lower}, repeat::take_until, self, sequence, range::take_while},
    value, EasyParser, Parser, StdParseResult, Stream, attempt, between, many, parser::char::upper, satisfy
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
                let mut s = s1;
                s.push('.');
                s.push_str(s2.as_str());
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
    // between(char('"'), many(combine::any()), char('"'));
    char('"').with(take_until(char('"')))
}

fn symbol<I>() -> impl Parser<I, Output = Name> 
where
    I: Stream<Token = char>
{
    upper().and(take_until(spaces())).map(
        |(c, s): (char, String)| {
            let mut s = s;
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
        lit().easy_parse("\"string test!2@\"").map(|(v, _s)| v),
        Ok(Lit::Str("string test!2@".to_owned()))
    );

    assert_eq!(
        lit().easy_parse("T").map(|(v, _s)| v),
        Ok(Lit::Sym( Name{ id: "T".to_owned() }) )
    );
/* 
    assert_eq!(
        lit().easy_parse("Name").map(|(v, _s)| v),
        Ok(Lit::Sym( Name{ id: "Name".to_owned() }) )
    );
*/
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
    lower().and(take_until(spaces())).map(
        |(c, s): (char, String)| {
            let mut s = s;
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
}

fn pat_op<I>() -> impl Parser<I, Output = PatOp>
where
    I: Stream<Token = char> 
{
    let pat_: fn(&mut I) -> StdParseResult<Pat, I> 
        = |input| pat().parse_stream(input).into();

    between(char('('), char(')'), between(spaces(), spaces(), pat_))
        .map(|p| PatOp::App(p))
}

fn pat<I>() -> impl Parser<I, Output = Pat> 
where
    I: Stream<Token = char>
{
    pat_head().and(many(spaces().with(pat_op()))).map(
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
        Ok(PatHead::Const(Lit::Sym(Name::id("X"))))
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
    /* 
    assert_eq!(
        pat().easy_parse("f(\"argrz\")").map(|(v, _s)| v),
        Ok(PatHead::Var(
            Name::id("f")).head()
            .app(PatHead::Const(Lit::Str("argrz".to_owned())).head()))
    );
    */
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

    between(char('('), char(')'), between(spaces(), spaces(), copat_))
        .map(|p| CopatOp::App(p))
        .or(lit().map(|v| CopatOp::Dot(v)))
}

fn copat<I>() -> impl Parser<I, Output = Copat>
where
    I: Stream<Token = char>,
{
    pat().and(many(spaces().with(copat_op()))).map(
        |(pat, tail)| {
		    pat.this().extend(tail)
        }
    )
}
