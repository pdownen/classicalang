mod examples;

use crate::syntax::sequential::{Lit, Modul, Name, Pat};

extern crate combine;

use combine::{
    chainl1, eof, many1,
    parser::{char::{char, digit, spaces}, repeat::take_until, self, sequence},
    value, EasyParser, Parser, StdParseResult, Stream, attempt, between, many,
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
    parser::char::upper().and(take_until(spaces())).map(
        |(c, s): (char, String)| {
            let mut s = s;
            s.insert(0, c);
            Name { id: s }
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
        lit().easy_parse("1.666").map(|(v, _s)| v),
        Ok(Lit::Flt(1.666))
    );
    assert_eq!(
        lit().easy_parse("\"string test!2@\"").map(|(v, _s)| v),
        Ok(Lit::Str("string test!2@".to_owned()))
    );
    assert_eq!(
        lit().easy_parse("T").map(|(v, _s)| v),
        Ok(Lit::Sym( Name{id: "T".to_owned()}) )
    );
    assert_eq!(
        lit().easy_parse("Name").map(|(v, _s)| v),
        Ok(Lit::Sym( Name{id: "Name".to_owned()}) )
    );
}
