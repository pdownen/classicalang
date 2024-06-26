use crate::syntax::sequential::{
    AtomicPat, Copat, CopatOp, Decl, Decons, DeconsOp, Expr, ExprHead, ExprOp, ExprTail, Lit,
    Modul, Name, Pat,
};

extern crate combine;

use combine::{
    attempt, between, eof, many, many1, none_of, one_of, optional,
    parser::char::upper,
    parser::{
        self,
        char::{char, digit, lower, spaces, string},
    },
    satisfy, value, EasyParser, Parser, StdParseResult, Stream,
};

pub fn natural<I>() -> impl Parser<I, Output = String>
where
    I: Stream<Token = char>,
{
    many1(digit())
}

pub fn sign<'a, I>() -> impl Parser<I, Output = &'a str>
where
    I: Stream<Token = char>,
{
    string("-").or(optional(string("+")).map(|_| ""))
}

pub fn integer<I>() -> impl Parser<I, Output = i64>
where
    I: Stream<Token = char>,
{
    sign()
        .and(natural())
        .map(|(s, n)| format!("{s}{n}").parse::<i64>().unwrap())
}

pub fn float_specials<I>() -> impl Parser<I, Output = String>
where
    I: Stream<Token = char>,
{
    (attempt(string("inf"))
        .or(attempt(string("Inf")))
        .or(attempt(string("INF")))
        .or(attempt(string("nan")))
        .or(attempt(string("NAN")))
        .or(string("NaN")))
    .map(|s| s.to_string())
}

pub fn float_digits<I>() -> impl Parser<I, Output = String>
where
    I: Stream<Token = char>,
{
    many1(digit())
        .skip(char('.'))
        .and(many1(digit()))
        .and(optional((char('e').or(char('E'))).and(natural())))
        .map(
            |((s1, s2), exp): ((String, String), Option<(char, String)>)| match exp {
                None => format!("{s1}.{s2}"),
                Some((e, expn)) => format!("{s1}.{s2}{e}{expn}"),
            },
        )
        // .or(float_specials())
}

pub fn float<I>() -> impl Parser<I, Output = f64>
where
    I: Stream<Token = char>,
{
    sign()
        .and(float_digits())
        .map(|(s, f)| format!("{s}{f}").parse::<f64>().unwrap())
}

pub fn string_quoted<I>() -> impl Parser<I, Output = String>
where
    I: Stream<Token = char>,
{
    let string_char = none_of("\"\\".chars());
    let escape_sequence = char('\\').with(
        char('"')
            .or(char('\\'))
            .or(char('n').map(|_| '\n'))
            .or(char('0').map(|_| '\0')),
    );
    // look for other escape sequences

    char('"')
        .with(many(string_char.or(escape_sequence)))
        .skip(char('"'))
}

pub fn symbol<I>() -> impl Parser<I, Output = Name>
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

pub fn lit<I>() -> impl Parser<I, Output = Lit>
where
    I: Stream<Token = char>,
{
    attempt(float().map(Lit::Flt))
        .or(integer().map(Lit::Int))
        .or(string_quoted().map(Lit::Str))
        .or(symbol().map(Lit::Sym))
        .skip(spaces())
}

pub fn wildcard_pat<I>() -> impl Parser<I, Output = char>
where
    I: Stream<Token = char>,
{
    char('_')
}

// check about keywords 
pub fn variable<I>() -> impl Parser<I, Output = Name>
where
    I: Stream<Token = char>,
{
    lower()
        .and(many(satisfy(|c: char| {
            c.is_alphanumeric() || c == '_' || c == '\''
        })))
        .skip(spaces())
        .map(|(c, mut s): (char, String)| {
            s.insert(0, c);
            Name::id(&s)
        })
}

pub fn atomic_pat<I>() -> impl Parser<I, Output = AtomicPat>
where
    I: Stream<Token = char>,
{
    wildcard_pat()
        .map(|_| AtomicPat::blank())
        .or(lit().map(Lit::switch))
        .or(variable().map(AtomicPat::Var))
        .skip(spaces())
}

pub fn decons<I>() -> impl Parser<I, Output = Decons>
where
    I: Stream<Token = char>,
{
    lit()
        .and(spaces().with(many1(decons_op())))
        .map(|(c, ops): (Lit, Vec<DeconsOp>)| c.mtch().extend(ops))
}

pub fn decons_op<I>() -> impl Parser<I, Output = DeconsOp>
where
    I: Stream<Token = char>,
{
    let pat_: fn(&mut I) -> StdParseResult<Pat, I> = |input| pat().parse_stream(input).into();

    between(char('(').skip(spaces()), char(')').skip(spaces()), pat_)
        .or(atomic_pat().map(Pat::Atom))
        .map(DeconsOp::App)
}

pub fn pat<I>() -> impl Parser<I, Output = Pat>
where
    I: Stream<Token = char>,
{
    attempt(decons().map(Pat::Struc)).or(atomic_pat().map(Pat::Atom))
}

pub fn copat_op<I>() -> impl Parser<I, Output = CopatOp>
where
    I: Stream<Token = char>,
{
    let parenthesized = |p| between(char('(').skip(spaces()), char(')').skip(spaces()), p);
    let pat_: fn(&mut I) -> StdParseResult<Pat, I> = |input| pat().parse_stream(input).into();

    parenthesized(pat_)
        .map(|p| CopatOp::App(p))
        .or(atomic_pat().map(|p| CopatOp::App(p.atom())))
        .or(char('.').with(lit().map(CopatOp::Dot)))
        .skip(spaces())
}

pub fn copat<I>() -> impl Parser<I, Output = Copat>
where
    I: Stream<Token = char>,
{
    atomic_pat()
        .and(many(attempt(copat_op())))
        .map(|(apat, tail)| apat.this().extend(tail))
}

pub fn expr_head<I>() -> impl Parser<I, Output = ExprHead>
where
    I: Stream<Token = char>,
{
    let modul_: fn(&mut I) -> StdParseResult<Modul, I> = |input| modul().parse_stream(input).into();

    (variable().map(|n: Name| ExprHead::Var(n)))
        .or(lit().map(|l: Lit| ExprHead::Const(l)))
        .or(between(char('{').skip(spaces()), char('}'), modul_).map(|f| ExprHead::Lambda(f)))
        .skip(spaces())
}

pub fn expr_op<I>() -> impl Parser<I, Output = ExprOp>
where
    I: Stream<Token = char>,
{
    atomic_expr()
        .map(ExprOp::App)
        .or(spaces().with(char('.').skip(spaces()).with(lit().map(|l| ExprOp::Dot(l)))))
        .skip(spaces())
}

pub fn expr_tail<I>() -> impl Parser<I, Output = ExprTail>
where
    I: Stream<Token = char>,
{
    many(expr_op())
}

pub fn expr<I>() -> impl Parser<I, Output = Expr>
where
    I: Stream<Token = char>,
{
    atomic_expr()
        .and(expr_tail())
        .map(|(h, t): (Expr, ExprTail)| h.extend(t))
}

pub fn atomic_expr<I>() -> impl Parser<I, Output = Expr>
where
    I: Stream<Token = char>,
{
    let expr_: fn(&mut I) -> StdParseResult<Expr, I> = |input| expr().parse_stream(input).into();

    expr_head().map(ExprHead::head).or(between(
        char('(').skip(spaces()),
        char(')').skip(spaces()),
        expr_,
    ))
}

pub fn decl<I>() -> impl Parser<I, Output = Decl>
where
    I: Stream<Token = char>,
{
    attempt(
        string("include")
            .skip(spaces())
            .with(expr())
            .map(|expr| Decl::Include(expr)),
    )
    .or(attempt(
        copat()
            .skip(string("->"))
            .skip(spaces())
            .and(expr())
            .map(|(copat, expr)| Decl::Method(copat, expr)),
    ))
    .or(pat()
        .skip(string("<-"))
        .skip(spaces())
        .and(expr())
        .map(|(pat, expr)| Decl::Bind(pat, expr)))
}

pub fn decl_list<I>() -> impl Parser<I, Output = Vec<Decl>>
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

pub fn whole_input<I, P>(p: P) -> impl Parser<I, Output = P::Output>
where
    I: Stream<Token = char>,
    P: Parser<I>,
{
    spaces().with(p).skip(spaces()).skip(eof())
}
