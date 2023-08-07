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
        ExprHead::Var(self).head()
    }

    pub fn bind(self) -> Pat {
        PatHead::Var(self).head()
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
#[derive(Debug, PartialEq)]
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

// ExprHead ::= Var | Lit

// ExprOp ::= (Expr) | .Lit

// ExprTail ::= ExprOp*

// Expr ::= ExprHead ExprTail
#[derive(Debug, PartialEq)]
pub enum ExprHead {
    Var(Name),
    Const(Lit),
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
#[derive(Debug, PartialEq)]
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

    pub fn mtch(self) -> Pat {
        PatHead::Const(self).head()
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

// PatHead ::= _ | Var | Lit

// PatOp ::= (Pat)

// Pat ::= PatHead PatOp*

#[derive(Debug, PartialEq)]
pub enum PatHead {
    Unused,
    Var(Name),
    Const(Lit),
}

#[derive(Debug, PartialEq)]
pub enum PatOp {
    App(Pat),
}

type PatTail = Vec<PatOp>;

#[derive(Debug, PartialEq)]
pub struct Pat {
    head: PatHead,
    tail: Vec<PatOp>,
}

impl PatHead {
    pub fn head(self) -> Pat {
        Pat {
            head: self,
            tail: Vec::new(),
        }
    }
}

impl Pat {
    pub fn blank() -> Pat {
        PatHead::Unused.head()
    }

    pub fn this(self) -> Copat {
        Copat {
            head: self,
            tail: Vec::new(),
        }
    }

    pub fn push(mut self, op: PatOp) -> Pat {
        self.tail.push(op);
        self
    }

    pub fn extend(mut self, more: PatTail) -> Pat {
        self.tail.extend(more);
        self
    }

    pub fn app(self, arg: Pat) -> Pat {
        self.push(PatOp::App(arg))
    }
}

impl fmt::Display for PatHead {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PatHead::Unused => write!(f, "_"),
            PatHead::Var(x) => write!(f, "{}", x),
            PatHead::Const(c) => write!(f, "{}", c),
        }
    }
}

impl fmt::Display for PatOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PatOp::App(p) => write!(f, "({})", p),
        }
    }
}

impl fmt::Display for Pat {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.head)?;

        for op in &self.tail {
            write!(f, "{}", op)?
        }

        Ok(())
    }
}
