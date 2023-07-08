use std::fmt;

pub type Name<'a> = &'a str;

// Modul ::= Decl_1 \n Decl_2 \n ... \n Decl_n
#[derive(Debug)]
pub struct Modul<'a> {
    pub defns: &'a [Decl<'a>],
}

impl<'a> fmt::Display for Modul<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for d in self.defns {
            writeln!(f, "{}", d)?;
        }
        Ok(())
    }
}

// Decl ::= include Expr
//        | Copat = Expr
#[derive(Debug)]
pub enum Decl<'a> {
    Include(Expr<'a>),
    Method(Copat<'a>, Expr<'a>),
}

impl<'a> fmt::Display for Decl<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Decl::Include(e) => write!(f, "include {}", e),
            Decl::Method(q, e) => write!(f, "{} = {}", q, e),
        }
    }
}

// Var = any identifier name starting with lower case letter

// Expr ::= Var
//        | Lit
//        | Expr(Expr)
//        | Expr.Lit
#[derive(Debug)]
pub enum Expr<'a> {
    Var(Name<'a>),
    Const(Lit<'a>),
    App(&'a Expr<'a>, &'a Expr<'a>),
    Dot(&'a Expr<'a>, Lit<'a>),
}

impl<'a> fmt::Display for Expr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Var(x) => write!(f, "{}", x),
            Expr::Const(c) => write!(f, "{}", c),
            Expr::App(e1, e2) => write!(f, "{}({})", e1, e2),
            Expr::Dot(e, c) => write!(f, "{}.{}", e, c),
        }
    }
}

// Int = machine integeger
// Flt = machine floating-point number
// Str = built-in string
// Sym = any identifier name starting with an upper case letter

// Lit ::= Int | Flt | Str | Sym
#[derive(Debug)]
pub enum Lit<'a> {
    Int(i64),
    Flt(f64),
    Str(String),
    Sym(Name<'a>),
}

impl<'a> fmt::Display for Lit<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Lit::Int(i) => write!(f, "{}", i),
            Lit::Flt(n) => write!(f, "{}", n),
            Lit::Str(s) => write!(f, "{:?}", s),
            Lit::Sym(s) => write!(f, "{}", s),
        }
    }
}

// Copat ::= Pat
//         | Copat(Pat)
//         | Copat.Lit
#[derive(Debug)]
pub enum Copat<'a> {
    This(Pat<'a>),
    App(&'a Copat<'a>, Pat<'a>),
    Dot(&'a Copat<'a>, Lit<'a>),
}

impl<'a> fmt::Display for Copat<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Copat::This(p) => write!(f, "{}", p),
            Copat::App(q, p) => write!(f, "{}({})", q, p),
            Copat::Dot(q, c) => write!(f, "{}.{}", q, c),
        }
    }
}

// Pat ::= _
//       | Var
//       | Lit
//       | Pat(Pat)
#[derive(Debug)]
pub enum Pat<'a> {
    Unused,
    Var(Name<'a>),
    Const(Lit<'a>),
    App(&'a Pat<'a>, &'a Pat<'a>),
}

impl<'a> fmt::Display for Pat<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Pat::Unused => write!(f, "_"),
            Pat::Var(x) => write!(f, "{}", x),
            Pat::Const(c) => write!(f, "{}", c),
            Pat::App(p1, p2) => write!(f, "{}({})", p1, p2),
        }
    }
}
