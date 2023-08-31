use std::fmt;

use crate::{
    syntax::sequential::*
};

trait BasicPrint {
    fn basic_fmt(&self, f: &mut fmt::Formatter) -> fmt::Result;
}

impl BasicPrint for Name {
    fn basic_fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.id)
    }
}

impl BasicPrint for Modul {
    fn basic_fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for d in &self.defns {
            writeln!(f, "{};", d)?;
        }
        Ok(())
    }
}

impl BasicPrint for Decl {
    fn basic_fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Decl::Include(e) => write!(f, "include {}", e),
            Decl::Method(q, e) => write!(f, "{} -> {}", q, e),
            Decl::Bind(p, e) => write!(f, "{} <- {}", p, e)
        }
    }
}

impl BasicPrint for ExprHead {
    fn basic_fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ExprHead::Var(x) => write!(f, "{}", x),
            ExprHead::Const(c) => write!(f, "{}", c),
            ExprHead::Lambda(l) => write!(f, "{{{l}}}"),
        }
    }
}

impl BasicPrint for ExprOp {
    fn basic_fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ExprOp::App(a) => write!(f, "({})", a),
            ExprOp::Dot(m) => write!(f, ".{}", m),
        }
    }
}

impl BasicPrint for Expr {
    fn basic_fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.head)?;

        for op in &self.tail {
            write!(f, "{}", op)?
        }

        Ok(())
    }
}

impl BasicPrint for Lit {
    fn basic_fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Lit::Int(i) => write!(f, "{}", i),
            Lit::Flt(n) => write!(f, "{:?}", n),
            Lit::Str(s) => write!(f, "{:?}", s),
            Lit::Sym(s) => write!(f, "{}", s),
        }
    }
}

impl BasicPrint for CopatOp {
    fn basic_fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CopatOp::App(p) => write!(f, "({})", p),
            CopatOp::Dot(c) => write!(f, ".{}", c),
        }
    }
}

impl BasicPrint for Copat {
    fn basic_fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.head)?;

        for op in &self.tail {
            write!(f, "{}", op)?
        }

        Ok(())
    }
}

impl BasicPrint for Pat {
    fn basic_fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Pat::Atom(a) => write!(f, "{}", a),
            Pat::Struc(s) => write!(f, "{}", s),
        }
    }
}

impl BasicPrint for AtomicPat {
    fn basic_fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AtomicPat::Unused => write!(f, "_"),
            AtomicPat::Var(x) => write!(f, "{}", x),
            AtomicPat::Const(c) => write!(f, "{}", c),
        }
    }
}

impl BasicPrint for DeconsOp {
    fn basic_fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DeconsOp::App(p) => write!(f, "({})", p),
        }
    }
}

impl BasicPrint for Decons {
    fn basic_fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.head)?;

        for op in &self.tail {
            write!(f, "{}", op)?
        }

        Ok(())
    }
}