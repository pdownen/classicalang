extern crate combine;

use combine::{
    chainl1, eof, many1,
    parser::char::{char, digit, spaces},
    value, EasyParser, Parser, StdParseResult, Stream,
};
use std::io::Write;

#[derive(Debug, PartialEq, Clone)]
enum Expr {
    Num(i64),
    Plus(Box<Expr>, Box<Expr>),
    Minus(Box<Expr>, Box<Expr>),
    Times(Box<Expr>, Box<Expr>),
}

impl Expr {
    fn eval(self: &Expr) -> i64 {
        match self {
            Expr::Num(n) => *n,
            Expr::Plus(l, r) => l.eval() + r.eval(),
            Expr::Minus(l, r) => l.eval() - r.eval(),
            Expr::Times(l, r) => l.eval() * r.eval(),
        }
    }

    fn display(self: &Expr) -> String {
        match self {
            Expr::Num(n) => n.to_string(),
            Expr::Plus(l, r) => format!("({} + {})", l.display(), r.display()),
            Expr::Minus(l, r) => format!("({} - {})", l.display(), r.display()),
            Expr::Times(l, r) => format!("({} * {})", l.display(), r.display()),
        }
    }

    fn plus(self: Expr, other: Expr) -> Expr {
        Expr::Plus(Box::new(self), Box::new(other))
    }

    fn minus(self: Expr, other: Expr) -> Expr {
        Expr::Minus(Box::new(self), Box::new(other))
    }

    fn times(self: Expr, other: Expr) -> Expr {
        Expr::Times(Box::new(self), Box::new(other))
    }
}

fn number<I>() -> impl Parser<I, Output = i64>
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
    sign().and(number()).map(|(s, n)| s * n)
}

fn parenthesize<I, P>(p: P) -> impl Parser<I, Output = P::Output>
where
    I: Stream<Token = char>,
    P: Parser<I>,
{
    char('(')
        .skip(spaces())
        .with(p)
        .skip(spaces())
        .skip(char(')'))
        .skip(spaces())
}

fn whole<I, P>(p: P) -> impl Parser<I, Output = P::Output>
where
    I: Stream<Token = char>,
    P: Parser<I>,
{
    spaces().with(p).skip(spaces()).skip(eof())
}

fn atomic<I>(p: impl Parser<I, Output = Expr>) -> impl Parser<I, Output = Expr>
where
    I: Stream<Token = char>,
{
    integer().skip(spaces()).map(Expr::Num).or(parenthesize(p))
}

fn plus<I>() -> impl Parser<I, Output = fn(Expr, Expr) -> Expr>
where
    I: Stream<Token = char>,
{
    char('+').with(value(Expr::plus as _))
}

fn minus<I>() -> impl Parser<I, Output = fn(Expr, Expr) -> Expr>
where
    I: Stream<Token = char>,
{
    char('-').with(value(Expr::minus as _))
}

fn times<I>() -> impl Parser<I, Output = fn(Expr, Expr) -> Expr>
where
    I: Stream<Token = char>,
{
    char('*').with(value(Expr::times as _))
}

fn term<I>() -> impl Parser<I, Output = Expr>
where
    I: Stream<Token = char>,
{
    let expr_: fn(&mut I) -> StdParseResult<Expr, I> = |i| expr().parse_stream(i).into();

    chainl1(atomic(expr_), times().skip(spaces()))
}

fn expr<I>() -> impl Parser<I, Output = Expr>
where
    I: Stream<Token = char>,
{
    chainl1(term(), plus().or(minus()).skip(spaces()))
}

pub fn calculator() {
    loop {
        print!("> ");
        match std::io::stdout().flush() {
            Ok(_) => {}
            Err(e) => {
                eprintln!("Error flushing stdout: {}", e);
                return;
            }
        }
        let mut input = String::new();
        if std::io::stdin().read_line(&mut input).is_err() {
            eprintln!("Error reading input.");
            return;
        }
        let parsed_expr = whole(expr()).easy_parse(input.as_str());
        match parsed_expr {
            Ok((expr, _)) => {
                println!("{}", expr.display());
                println!("= {}", expr.eval());
            }
            Err(err) => println!("Error: {}", err),
        }
    }
}
