use pretty::{Arena, DocAllocator, DocBuilder, RcDoc};
use std::fmt;

pub const MAX_LINE_WIDTH: usize = 80;

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
        ExprHead::Var(self).head()
    }

    pub fn bind(self) -> Pat {
        Pat::Var(self)
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let allocator: Arena<'_, ()> = pretty::Arena::new();
        let doc = allocator.text(&self.id);
        write!(f, "{}", doc.pretty(MAX_LINE_WIDTH))
    }
}

// Modul ::= Decl_1 ; Decl_2 ; ... Decl_n ;
#[derive(Debug, PartialEq)]
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
//        | Copat -> Expr
//        | Pat <- Expr
#[derive(Debug, PartialEq)]
pub enum Decl {
    Include(Expr),
    Method(Copat, Expr),
    Bind(Pat, Expr)
}

impl fmt::Display for Decl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Decl::Include(e) => write!(f, "include {}", e),
            Decl::Method(q, e) => write!(f, "{} -> {}", q, e),
            Decl::Bind(p, e) => write!(f, "{} <- {}", p, e),
        }
    }
}

// Var = any identifier name starting with lower case letter

// ExprHead ::= Var | Lit
// | {Modul}

// ExprOp ::= (Expr) | .Lit

// ExprTail ::= ExprOp*

// Expr ::= ExprHead ExprTail
#[derive(Debug, PartialEq)]
pub enum ExprHead {
    Var(Name),
    Const(Lit),
    Lambda(Modul),
}

#[derive(Debug, PartialEq)]
pub enum ExprOp {
    App(Expr),
    Dot(Lit),
}

pub type ExprTail = Vec<ExprOp>;

#[derive(Debug, PartialEq)]
pub struct Expr {
    head: ExprHead,
    tail: ExprTail,
}

impl ExprHead {
    pub fn head(self) -> Expr {
        Expr {
            head: self,
            tail: Vec::new(),
        }
    }
}

impl Expr {
    pub fn push(mut self, op: ExprOp) -> Expr {
        self.tail.push(op);
        self
    }

    pub fn extend(mut self, more: ExprTail) -> Expr {
        self.tail.extend(more);
        self
    }

    pub fn app(self, arg: Expr) -> Expr {
        self.push(ExprOp::App(arg))
    }

    pub fn dot(self, field: Lit) -> Expr {
        self.push(ExprOp::Dot(field))
    }

    pub fn included(self) -> Decl {
        Decl::Include(self)
    }
}

impl fmt::Display for ExprHead {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ExprHead::Var(x) => write!(f, "{}", x),
            ExprHead::Const(c) => write!(f, "{}", c),
            ExprHead::Lambda(l) => write!(f, "{{{l}}}"),
        }
    }
}

impl fmt::Display for ExprOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ExprOp::App(a) => write!(f, "({})", a),
            ExprOp::Dot(m) => write!(f, ".{}", m),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.head)?;
        for op in &self.tail {
            write!(f, "{}", op)?
        }
        Ok(())
    }
}

// Int = machine integeger
// Flt = machine floating-point number
// Str = built-in string
// Sym = any identifier name starting with an upper case letter

// Lit ::= Int | Flt | Str | Sym
#[derive(Debug, PartialEq, Clone)]
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
        ExprHead::Const(self).head()
    }

    pub fn mtch(self) -> Decons {
        Decons {
            head: self,
            tail: Vec::new(),
        }
    }
}


impl fmt::Display for Lit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let doc = match self {
            Lit::Int(i) => RcDoc::<()>::text(i.to_string()),
            Lit::Flt(n) => RcDoc::<()>::text(n.to_string()),
            Lit::Str(s) => RcDoc::<()>::text(format!("{:?}", s)),
            Lit::Sym(s) => RcDoc::<()>::text(s.to_string()),
        };
        write!(f, "{}", doc.pretty(MAX_LINE_WIDTH))
    }
}

// CopatOp ::= (Pat) | .Lit

// Copat ::= Pat CopatOp*
#[derive(Debug, PartialEq)]
pub enum CopatOp {
    App(Pat),
    Dot(Lit),
}

type CopatTail = Vec<CopatOp>;

#[derive(Debug, PartialEq)]
pub struct Copat {
    head: Pat,
    tail: CopatTail,
}

impl Copat {
    pub fn goes_to(self, rhs: Expr) -> Decl {
        Decl::Method(self, rhs)
    }

    pub fn push(mut self, op: CopatOp) -> Copat {
        self.tail.push(op);
        self
    }

    pub fn extend(mut self, more: CopatTail) -> Copat {
        self.tail.extend(more);
        self
    }

    pub fn app(self, arg: Pat) -> Copat {
        self.push(CopatOp::App(arg))
    }

    pub fn dot(self, field: Lit) -> Copat {
        self.push(CopatOp::Dot(field))
    }
}

impl fmt::Display for CopatOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let doc = match self {
            CopatOp::App(p) => RcDoc::<()>::text(format!("({})", p)),
            CopatOp::Dot(c) => RcDoc::<()>::text(format!(".{}", c)),
        };
        write!(f, "{}", doc.pretty(MAX_LINE_WIDTH))
    }
}

impl fmt::Display for Copat {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut doc: RcDoc<'_, ()> = RcDoc::nil();
        doc = doc.append(RcDoc::text(self.head.to_string()));
        for op in &self.tail {
            doc = doc.append(RcDoc::text(op.to_string()));
        }
        write!(f, "{}", doc.pretty(MAX_LINE_WIDTH))
    }
}

// Pat ::= _ | Var | Decons

// Decons ::= Lit DeconsOp*

// DeconsOp ::= (Pat)

#[derive(Debug, PartialEq)]
pub enum Pat {
    Unused,
    Var(Name),
    Struc(Decons),
}

#[derive(Debug, PartialEq)]
pub enum DeconsOp {
    App(Pat),
}

type DeconsTail = Vec<DeconsOp>;

#[derive(Debug, PartialEq)]
pub struct Decons {
    head: Lit,
    tail: DeconsTail,
}

impl Decons {
    pub fn decons(self) -> Pat {
        Pat::Struc(self)
    }

    pub fn push(mut self, op: DeconsOp) -> Decons {
        self.tail.push(op);
        self
    }

    pub fn extend(mut self, more: DeconsTail) -> Decons {
        self.tail.extend(more);
        self
    }

    pub fn app(self, arg: Pat) -> Decons {
        self.push(DeconsOp::App(arg))
    }
}

impl Pat {
    pub fn blank() -> Pat {
        Pat::Unused
    }

    pub fn this(self) -> Copat {
        Copat {
            head: self,
            tail: Vec::new(),
        }
    }
}

impl fmt::Display for Pat {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let doc = match self {
            Pat::Unused => RcDoc::<()>::text("_"),
            Pat::Var(x) => RcDoc::<()>::text(x.to_string()),
            Pat::Struc(s) => RcDoc::<()>::text(s.to_string()),
        };
        write!(f, "{}", doc.pretty(MAX_LINE_WIDTH))
    }
}

impl fmt::Display for DeconsOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let doc = match self {
            DeconsOp::App(p) => RcDoc::<()>::text(format!("({})", p)),
        };
        write!(f, "{}", doc.pretty(MAX_LINE_WIDTH))
    }
}

impl fmt::Display for Decons {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut doc: RcDoc<'_, ()> = RcDoc::nil();
        doc = doc.append(RcDoc::text(self.head.to_string()));
        for op in &self.tail {
            doc = doc.append(RcDoc::text(op.to_string()));
        }
        write!(f, "{}", doc.pretty(MAX_LINE_WIDTH))
    }
}
