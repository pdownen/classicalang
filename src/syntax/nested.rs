use std::fmt;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Name {
    pub id: String,
}

impl Name {
    pub fn id(s: &str) -> Name {
        Name { id: s.to_string() }
    }

    pub fn sym(self) -> Lit {
        Lit::Sym(self)
    }

    pub fn refer(self) -> Expr {
        Expr::Var(self)
    }

    pub fn bind(self) -> Pat {
        Pat::Var(self)
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.id)
    }
}

// Modul ::= Decl_1 ; Decl_2 ; ... Decl_n ;
#[derive(Debug)]
pub struct Modul {
    pub defns: Vec<Decl>,
}

impl Modul {
    pub fn top() -> Modul {
        Modul { defns: Vec::new() }
    }

    pub fn then(mut self, decl: Decl) -> Modul {
        self.defns.push(decl);
        self
    }

    pub fn followed_by(mut self, more: Modul) -> Modul {
        self.defns.extend(more.defns);
        self
    }
}

impl fmt::Display for Modul {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for d in &self.defns {
            writeln!(f, "{};", d)?;
        }
        Ok(())
    }
}

// Decl ::= include Expr
//        | Copat = Expr
#[derive(Debug)]
pub enum Decl {
    Include(Expr),
    Method(Copat, Expr),
}

impl fmt::Display for Decl {
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
pub enum Expr {
    Var(Name),
    Const(Lit),
    App(Box<Expr>, Box<Expr>),
    Dot(Box<Expr>, Lit),
}

impl Expr {
    pub fn app(self, arg: Expr) -> Expr {
        Expr::App(Box::new(self), Box::new(arg))
    }

    pub fn dot(self, field: Lit) -> Expr {
        Expr::Dot(Box::new(self), field)
    }

    pub fn included(self) -> Decl {
        Decl::Include(self)
    }
}

impl fmt::Display for Expr {
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
pub enum Lit {
    Int(i64),
    Flt(f64),
    Str(String),
    Sym(Name),
}

impl Lit {
    pub fn int(i: i64) -> Lit {
        Lit::Int(i)
    }

    pub fn flt(x: f64) -> Lit {
        Lit::Flt(x)
    }

    pub fn str(s: String) -> Lit {
        Lit::Str(s)
    }

    pub fn sym(n: Name) -> Lit {
        Lit::Sym(n)
    }

    pub fn cnst(self) -> Expr {
        Expr::Const(self)
    }

    pub fn mtch(self) -> Decons {
        Decons::Const(self)
    }
}

impl fmt::Display for Lit {
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
pub enum Copat {
    This(Pat),
    App(Box<Copat>, Pat),
    Dot(Box<Copat>, Lit),
}

impl Copat {
    pub fn goes_to(self, rhs: Expr) -> Decl {
        Decl::Method(self, rhs)
    }

    pub fn app(self, arg: Pat) -> Copat {
        Copat::App(Box::new(self), arg)
    }

    pub fn dot(self, field: Lit) -> Copat {
        Copat::Dot(Box::new(self), field)
    }
}

impl fmt::Display for Copat {
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
//       | Decons
//
// Decons ::= Lit
//          | Decons(Pat)
#[derive(Debug)]
pub enum Pat {
    Unused,
    Var(Name),
    Struc(Decons),
}

#[derive(Debug)]
pub enum Decons {
    Const(Lit),
    App(Box<Decons>, Box<Pat>),
}

impl Decons {
    pub fn decons(self) -> Pat {
        Pat::Struc(self)
    }

    pub fn app(self, arg: Pat) -> Decons {
        Decons::App(Box::new(self), Box::new(arg))
    }
}

impl Pat {
    pub fn blank() -> Pat {
        Pat::Unused
    }

    pub fn this(self) -> Copat {
        Copat::This(self)
    }
}

impl fmt::Display for Pat {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Pat::Unused => write!(f, "_"),
            Pat::Var(x) => write!(f, "{}", x),
            Pat::Struc(s) => write!(f, "{}", s),
        }
    }
}

impl fmt::Display for Decons {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Decons::Const(c) => write!(f, "{}", c),
            Decons::App(p1, p2) => write!(f, "{}({})", p1, p2),
        }
    }
}
