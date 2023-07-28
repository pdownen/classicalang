type ParseResult<'a, Output> = Result<(&'a str, Output), &'a str>;

trait Parser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output>;
}

impl<'a, F, Output> Parser<'a, Output> for F
where
    F: Fn(&'a str) -> ParseResult<Output>,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self(input)
    }
}

struct DynParser<'a, Output> {
    parser: &'a dyn Parser<'a, Output>,
}

impl<'a, Output> DynParser<'a, Output> {
    fn new(p: &'a impl Parser<'a, Output>) -> Self {
        DynParser { parser: p }
    }
}

impl<'a, Output> Parser<'a, Output> for DynParser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self.parser.parse(input)
    }
}

struct BoxedParser<'a, Output> {
    parser: Box<dyn Parser<'a, Output> + 'a>,
}

impl<'a, Output> BoxedParser<'a, Output> {
    fn new<P>(p: impl Parser<'a, Output> + 'a) -> Self {
        BoxedParser {
            parser: Box::new(p),
        }
    }
}

impl<'a, Output> Parser<'a, Output> for BoxedParser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self.parser.parse(input)
    }
}

fn map<'a, P, F, A, B>(parser: P, map_fn: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    F: (Fn(A) -> B) + Copy,
{
    move |input| {
        parser
            .parse(input)
            .map(|(next_input, result)| (next_input, map_fn(result)))
    }
}

fn pair<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, (R1, R2)>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    move |input| {
        parser1.parse(input).and_then(|(next_input, result1)| {
            parser2
                .parse(next_input)
                .map(|(last_input, result2)| (last_input, (result1, result2)))
        })
    }
}

fn left<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R1>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(left, _right)| left)
}

fn right<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R2>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(_left, right)| right)
}

fn eos<'a>() -> impl Parser<'a, ()> {
    |input: &'a str| {
        if input.is_empty() {
            Ok((input, ()))
        } else {
            Err(input)
        }
    }
}

fn match_literal<'a>(expected: &'static str) -> impl Parser<'a, ()> {
    move |input: &'a str| match input.get(0..expected.len()) {
        Some(next) if next == expected => Ok((&input[expected.len()..], ())),
        _ => Err(input),
    }
}

fn one_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    move |mut input| {
        let mut result = Vec::new();

        if let Ok((next_input, first_item)) = parser.parse(input) {
            input = next_input;
            result.push(first_item);
        } else {
            return Err(input);
        }

        while let Ok((next_input, next_item)) = parser.parse(input) {
            input = next_input;
            result.push(next_item);
        }

        Ok((input, result))
    }
}

fn zero_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    move |mut input| {
        let mut result = Vec::new();

        while let Ok((next_input, next_item)) = parser.parse(input) {
            input = next_input;
            result.push(next_item);
        }

        Ok((input, result))
    }
}

#[test]
fn one_or_more_combinator() {
    let parser = one_or_more(match_literal("ha"));
    assert_eq!(Ok(("", vec![(), (), ()])), parser.parse("hahaha"));
    assert_eq!(Err("ahah"), parser.parse("ahah"));
    assert_eq!(Err(""), parser.parse(""));
    assert_eq!(Ok(("x", vec![()])), parser.parse("hax"));
}

#[test]
fn zero_or_more_combinator() {
    let parser = zero_or_more(match_literal("ha"));
    assert_eq!(Ok(("", vec![(), (), ()])), parser.parse("hahaha"));
    assert_eq!(Ok(("ahah", vec![])), parser.parse("ahah"));
    assert_eq!(Ok(("", vec![])), parser.parse(""));
}

fn any_char(input: &str) -> ParseResult<char> {
    match input.chars().next() {
        Some(next) => Ok((&input[next.len_utf8()..], next)),
        _ => Err(input),
    }
}

fn pred<'a, P, A, F>(parser: P, predicate: F) -> impl Parser<'a, A>
where
    P: Parser<'a, A>,
    F: Fn(&A) -> bool + Copy,
{
    move |input| {
        if let Ok((next_input, value)) = parser.parse(input) {
            if predicate(&value) {
                return Ok((next_input, value));
            }
        }
        Err(input)
    }
}

#[test]
fn predicate_combinator() {
    let parser = pred(any_char, |c| *c == 'o');
    assert_eq!(Ok(("mg", 'o')), parser.parse("omg"));
    assert_eq!(Err("lol"), parser.parse("lol"));
    assert_eq!(Ok(("lol", 'o')), parser.parse("olol"));
}

fn whitespace_char<'a>() -> impl Parser<'a, char> {
    pred(any_char, |c| c.is_whitespace())
}

fn space1<'a>() -> impl Parser<'a, Vec<char>> {
    one_or_more(whitespace_char())
}

fn space0<'a>() -> impl Parser<'a, Vec<char>> {
    zero_or_more(whitespace_char())
}

#[derive(Debug, PartialEq)]
enum Expr {
    Num(i32),
    Plus(Box<Expr>, Box<Expr>),
    Mult(Box<Expr>, Box<Expr>),
}

// use std::rc::Rc;

use Expr::{Mult, Num, Plus};

fn quoted_string<'a>() -> impl Parser<'a, String> {
    map(
        right(
            match_literal("\""),
            left(
                zero_or_more(pred(any_char, |c| *c != '"')),
                match_literal("\""),
            ),
        ),
        |chars| chars.into_iter().collect(),
    )
}

fn main() {
    print!("{:?}", num_expr().parse("(-25)"));
}

fn integer(input: &str) -> ParseResult<i32> {
    let mut matched = String::new();
    let mut chars = input.chars();

    match chars.next() {
        Some(next) if next.is_numeric() || next == '-' => matched.push(next),
        _ => return Err(input),
    }

    while let Some(next) = chars.next() {
        if next.is_numeric() {
            matched.push(next);
        } else {
            break;
        }
    }

    let next_index = matched.len();
    Ok((&input[next_index..], matched.parse::<i32>().unwrap())) // ok
}

// triple parser application, returns middle reuslt

fn bracket<'a, P1, P2, P3, O1, O2, O3>(p1: P1, p2: P2, p3: P3) -> impl Parser<'a, O2>
where
    P1: Parser<'a, O1>,
    P2: Parser<'a, O2>,
    P3: Parser<'a, O3>,
{
    right(p1, left(p2, p3))
}

fn binary_op<'a, P1, P2, P3, O1, O2, O3>(p1: P1, p2: P2, p3: P3) -> impl Parser<'a, (O1, O3)>
where
    P1: Parser<'a, O1>,
    P2: Parser<'a, O2>,
    P3: Parser<'a, O3>,
{
    pair(p1, right(p2, p3))
}

fn plus_expr<'a, P, Output>(parser: P) -> impl Parser<'a, (Output, Output)>
where
    P: Parser<'a, Output> + Copy,
{
    binary_op(
        parser,
        bracket(space0(), match_literal("+"), space0()),
        parser,
    )

    /*
    move |input: &'a str| {
        parser.parse(input)
            .and_then(|(input, result1)| match_literal("+").parse(input)
            .and_then(|(input, _)| parser.parse(input)
            .and_then(|(input, result2)|
                    Ok((input, (result1, result2)))
                )
            )
        )
    }
    */
}

#[test]
fn plus_expr_test() {
    assert_eq!(Ok(("", (1, 2))), plus_expr(integer).parse("1 + 2"));
    assert_eq!(Err("ze"), plus_expr(integer).parse("1+       ze"));
}

fn mult_expr<'a, P, Output>(parser: P) -> impl Parser<'a, (Output, Output)>
where
    P: Parser<'a, Output> + Copy,
{
    binary_op(
        parser,
        bracket(space0(), match_literal("*"), space0()),
        parser,
    )
}

// fn parser type
fn either<'a, P1, P2, Output>(p1: P1, p2: P2) -> impl Parser<'a, Output>
where
    P1: Parser<'a, Output>,
    P2: Parser<'a, Output>,
{
    move |input| match p1.parse(input) {
        ok @ Ok(_) => ok,
        Err(_) => p2.parse(input),
    }
}

// impl Parser<'a, Output>
// FnParser<'a, Output>
// concrete parser type?
fn fail<'a, Output>() -> impl Parser<'a, Output> {
    |input| Err(input)
}

fn choose<'a, P, Output>(parsers: &'a [P]) -> impl Parser<'a, Output>
where
    P: Parser<'a, Output>,
{
    move |input| {
        for p in parsers {
            match p.parse(input) {
                ok @ Ok(_) => return ok,
                Err(_) => continue,
            }
        }
        Err(input)
    }
}

// order matters, experiemnt with other orders
fn expr<'a>() -> impl Parser<'a, Expr> {
    let expr_ = |input| expr().parse(input);

    either(
        map(integer, Num),
        either(
            parenthesized(expr_),
            either(
                map(plus_expr(expr_), |(x, y)| Plus(Box::new(x), Box::new(y))),
                map(mult_expr(expr_), |(x, y)| Mult(Box::new(x), Box::new(y))),
            ),
        ),
    )
}

#[test]
fn integer_parser() {
    assert_eq!(Ok(("", 100)), integer("100"));
    assert_eq!(Ok(("", 21423)), integer("21423"));
    assert_eq!(Ok(("", -100)), integer("-100"));
    assert_eq!(Err("x2"), integer("x2"));
    assert_eq!(Ok((".0", 2)), integer("2.0"));
}

fn parenthesized<'a, P, Output>(parser: P) -> impl Parser<'a, Output>
where
    P: Parser<'a, Output>,
{
    bracket(
        match_literal("("),
        bracket(space0(), parser, space0()),
        match_literal(")"),
    )
}

fn num_expr<'a>() -> impl Parser<'a, i32> {
    left(right(match_literal("("), integer), match_literal(")"))
}
