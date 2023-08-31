use pretty::{Arena, DocAllocator, DocBuilder, RcDoc};
use std::fmt;

pub const MAX_LINE_WIDTH: usize = 80;
pub const INDENTATION_WIDTH: isize = 2;

pub(crate) trait PrettyPrint {
    fn to_doc(&self) -> RcDoc<()>;
    fn to_pretty(&self, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc().render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
    fn pretty_fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_pretty(MAX_LINE_WIDTH))
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Name {
    pub id: String,
}

impl Name {
    pub fn id(s: &str) -> Name {
        if s.is_empty() || !s.starts_with(|c: char| c.is_alphabetic()) {
            panic!("Empty identfier name: {}", s);
        }
        Name { id: s.to_string() }
    }

    pub fn sym(self) -> Lit {
        match self.id.chars().nth(0) {
            Some(c) if c.is_alphabetic() && c.is_uppercase() => Lit::Sym(self),
            _ => panic!("Invalid symbol name: {}", self.id),
        }
    }

    pub fn refer(self) -> Expr {
        match self.id.chars().nth(0) {
            Some(c) if c.is_alphabetic() && c.is_lowercase() => ExprHead::Var(self).head(),
            _ => panic!("Invalid referenced variable name: {}", self.id),
        }
    }

    pub fn bind(self) -> AtomicPat {
        match self.id.chars().nth(0) {
            Some(c) if c.is_alphabetic() && c.is_lowercase() => AtomicPat::Var(self),
            _ => panic!("Invalid bound variable name: {}", self.id),
        }
    }
}

impl PrettyPrint for Name {
    fn to_doc(&self) -> RcDoc<()> {
        RcDoc::text(&self.id)
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.id)
    }
}

// Modul ::= Decl_1 ; Decl_2 ; ... Decl_n ;
#[derive(Debug, PartialEq, Clone)]
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

    pub fn lambda(self) -> Expr {
        ExprHead::Lambda(self).head()
    }
}

impl PrettyPrint for Modul {
    fn to_doc(&self) -> RcDoc<()> {
        RcDoc::concat(
            self.defns
                .iter()
                .map(|d| d.to_doc().append(RcDoc::text(";")).append(RcDoc::line())),
        )
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
#[derive(Debug, PartialEq, Clone)]
pub enum Decl {
    Include(Expr),
    Method(Copat, Expr),
    Bind(Pat, Expr),
}

impl PrettyPrint for Decl {
    fn to_doc(&self) -> RcDoc<'_> {
        match self {
            Decl::Include(e) => RcDoc::text("include ")
                .append(e.to_doc())
                .nest(INDENTATION_WIDTH)
                .group(),
            Decl::Method(q, e) => q
                .to_doc()
                .append(RcDoc::text(" ->"))
                .append(RcDoc::line())
                .append(e.to_doc())
                .nest(INDENTATION_WIDTH)
                .group(),
            Decl::Bind(p, e) => p
                .to_doc()
                .append(RcDoc::text(" <-"))
                .append(RcDoc::line())
                .append(e.to_doc())
                .nest(INDENTATION_WIDTH)
                .group(),
        }
    }
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
#[derive(Debug, PartialEq, Clone)]
pub enum ExprHead {
    Var(Name),
    Const(Lit),
    Lambda(Modul),
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExprOp {
    App(Expr),
    Dot(Lit),
}

pub type ExprTail = Vec<ExprOp>;

#[derive(Debug, PartialEq, Clone)]
pub struct Expr {
    pub head: ExprHead,
    pub tail: ExprTail,
}

impl ExprHead {
    pub fn head(self) -> Expr {
        Expr {
            head: self,
            tail: Vec::new(),
        }
    }
}

impl PrettyPrint for ExprHead {
    fn to_doc(&self) -> RcDoc<()> {
        let doc = match self {
            ExprHead::Var(x) => x.to_doc(),
            ExprHead::Const(c) => c.to_doc(),
            ExprHead::Lambda(l) => RcDoc::<()>::text("{")
                .append(
                    RcDoc::<()>::line()
                        .append(l.to_doc())
                        .nest(INDENTATION_WIDTH),
                )
                .append(RcDoc::<()>::text("}"))
                .group(),
        };
        doc
    }
}

impl Expr {
    pub fn is_atomic(&self) -> bool {
        self.tail.is_empty()
    }

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

impl PrettyPrint for Expr {
    fn to_doc(&self) -> RcDoc<()> {
        let head_doc = self.head.to_doc();
        let op_docs: Vec<RcDoc<'_>> = self.tail.iter().map(|o| o.to_doc()).collect();
        let doc = head_doc
            .append(RcDoc::concat(op_docs).nest(INDENTATION_WIDTH))
            .group();
        doc
    }
}

impl PrettyPrint for ExprOp {
    fn to_doc(&self) -> RcDoc<()> {
        let doc = match self {
            ExprOp::App(a) if a.is_atomic() => RcDoc::line().append(a.to_doc()),
            ExprOp::App(a) => RcDoc::line()
                .append(RcDoc::<()>::text("("))
                .append(a.to_doc())
                .append(RcDoc::<()>::text(")")),
            ExprOp::Dot(m) => RcDoc::line_()
                .append(RcDoc::<()>::text("."))
                .append(m.to_doc()),
        };
        doc
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
#[derive(Debug, Clone)]
pub enum Lit {
    Int(i64),
    Flt(f64),
    Str(String),
    Sym(Name),
}

impl PartialEq for Lit {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int(l0), Self::Int(r0)) => l0 == r0,
            (Self::Flt(l0), Self::Flt(r0)) => {
                l0 == r0 || (l0.is_nan() && r0.is_nan()) || (l0.is_infinite() && r0.is_infinite())
            }
            (Self::Str(l0), Self::Str(r0)) => l0 == r0,
            (Self::Sym(l0), Self::Sym(r0)) => l0 == r0,
            _ => false,
        }
    }
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

    pub fn switch(self) -> AtomicPat {
        AtomicPat::Const(self)
    }

    pub fn mtch(self) -> Decons {
        Decons {
            head: self,
            tail: Vec::new(),
        }
    }
}

impl PrettyPrint for Lit {
    fn to_doc(&self) -> RcDoc<()> {
        let doc = match self {
            Lit::Int(i) => RcDoc::<()>::text(i.to_string()),
            Lit::Flt(n) => RcDoc::<()>::text(format!("{:?}", n)),
            Lit::Str(s) => RcDoc::<()>::text(format!("{:?}", s)),
            Lit::Sym(s) => RcDoc::<()>::text(s.to_string()),
        };
        doc
    }
}

impl fmt::Display for Lit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Lit::Int(i) => write!(f, "{}", i),
            Lit::Flt(n) => write!(f, "{:?}", n),
            Lit::Str(s) => write!(f, "{:?}", s),
            Lit::Sym(s) => write!(f, "{}", s),
        }
    }
}

// Copat ::= AtomicPat CopatOp*

// CopatOp ::= (Pat) | .Lit
#[derive(Debug, PartialEq, Clone)]
pub struct Copat {
    pub head: AtomicPat,
    pub tail: CopatTail,
}

type CopatTail = Vec<CopatOp>;

#[derive(Debug, PartialEq, Clone)]
pub enum CopatOp {
    App(Pat),
    Dot(Lit),
}

impl Copat {
    pub fn is_atomic(&self) -> bool {
        self.tail.is_empty()
    }

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

impl PrettyPrint for Copat {
    fn to_doc(&self) -> RcDoc<()> {
        let mut doc: RcDoc<'_, ()> = RcDoc::nil();
        doc = doc.append(self.head.to_doc());
        for op in &self.tail {
            doc = doc.append(op.to_doc());
        }
        doc
    }
}

impl PrettyPrint for CopatOp {
    fn to_doc(&self) -> RcDoc<()> {
        let doc = match self {
            CopatOp::App(p) if p.is_atomic() => RcDoc::<()>::text(" ").append(p.to_doc()),
            CopatOp::App(p) => RcDoc::<()>::text("(")
                .append(p.to_doc())
                .append(RcDoc::<()>::text(")")),
            CopatOp::Dot(c) => RcDoc::<()>::text(".").append(c.to_doc()),
        };
        doc
    }
}

impl fmt::Display for CopatOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CopatOp::App(p) => write!(f, "({})", p),
            CopatOp::Dot(c) => write!(f, ".{}", c),
        }
    }
}

impl fmt::Display for Copat {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.head)?;

        for op in &self.tail {
            write!(f, "{}", op)?
        }

        Ok(())
    }
}

// Pat ::= AtomicPat | Decons

// AtomicPat ::= _ | Var | Lit

// Decons ::= Lit DeconsOp+

// DeconsOp ::= (Pat)

#[derive(Debug, PartialEq, Clone)]
pub enum Pat {
    Atom(AtomicPat),
    Struc(Decons),
}

#[derive(Debug, PartialEq, Clone)]
pub enum AtomicPat {
    Unused,
    Var(Name),
    Const(Lit),
}

#[derive(Debug, PartialEq, Clone)]
pub enum DeconsOp {
    App(Pat),
}

type DeconsTail = Vec<DeconsOp>;

#[derive(Debug, PartialEq, Clone)]
pub struct Decons {
    pub head: Lit,
    pub tail: DeconsTail,
}

impl Pat {
    pub fn is_atomic(&self) -> bool {
        match self {
            Pat::Struc(d) => d.tail.is_empty(),
            Pat::Atom(_) => true,
        }
    }

    pub fn blank() -> Pat {
        AtomicPat::blank().atom()
    }
}

impl PrettyPrint for Pat {
    fn to_doc(&self) -> RcDoc<()> {
        let doc = match self {
            Pat::Atom(a) => a.to_doc(),
            Pat::Struc(s) => s.to_doc(),
        };
        doc
    }
}

impl fmt::Display for Pat {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Pat::Atom(a) => write!(f, "{}", a),
            Pat::Struc(s) => write!(f, "{}", s),
        }
    }
}

impl AtomicPat {
    pub fn blank() -> AtomicPat {
        AtomicPat::Unused
    }

    pub fn atom(self) -> Pat {
        Pat::Atom(self)
    }

    pub fn this(self) -> Copat {
        Copat {
            head: self,
            tail: Vec::new(),
        }
    }
}

impl PrettyPrint for AtomicPat {
    fn to_doc(&self) -> RcDoc<()> {
        let doc = match self {
            AtomicPat::Unused => RcDoc::<()>::text("_"),
            AtomicPat::Var(x) => x.to_doc(),
            AtomicPat::Const(c) => c.to_doc(),
        };
        doc
    }
}

impl fmt::Display for AtomicPat {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AtomicPat::Unused => write!(f, "_"),
            AtomicPat::Var(x) => write!(f, "{}", x),
            AtomicPat::Const(c) => write!(f, "{}", c),
        }
    }
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

impl PrettyPrint for Decons {
    fn to_doc(&self) -> RcDoc<()> {
        let mut doc: RcDoc<'_, ()> = RcDoc::nil();
        doc = doc.append(self.head.to_doc());
        for op in &self.tail {
            doc = doc.append(op.to_doc());
        }
        doc
    }
}

impl PrettyPrint for DeconsOp {
    fn to_doc(&self) -> RcDoc<()> {
        let doc = match self {
            DeconsOp::App(p) if p.is_atomic() => RcDoc::<()>::text(" ").append(p.to_doc()),
            DeconsOp::App(p) => RcDoc::<()>::text("(")
                .append(p.to_doc())
                .append(RcDoc::<()>::text(")")),
        };
        doc
    }
}

impl fmt::Display for DeconsOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DeconsOp::App(p) => write!(f, "({})", p),
        }
    }
}

impl fmt::Display for Decons {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.head)?;

        for op in &self.tail {
            write!(f, "{}", op)?
        }

        Ok(())
    }
}
