mod syntax;

use std::f64::consts::PI;
use std::fmt::{Debug, Display};

use syntax::{Copat, Decl, Expr, Lit, Modul, Pat};

fn compare_output<T: Debug + Display>(x: &T) {
    println!("----------");
    println!("{:?}", x);
    println!("==========");
    println!("{}", x);
    println!("----------");
}

fn main() {
    let ex1 = Copat::This(Pat::Var("self"));
    compare_output(&ex1);

    let hello = Expr::Const(Lit::Str(String::from("Hello, world!")));
    let ex2 = Expr::App(&Expr::Var("print"), &hello);
    compare_output(&ex2);

    let ex3 = Decl::Method(
        Copat::App(&Copat::This(Pat::Var("inc")), Pat::Var("x")),
        Expr::App(
            &Expr::App(&Expr::Var("plus"), &Expr::Var("x")),
            &Expr::Const(Lit::Int(1)),
        ),
    );
    compare_output(&ex3);

    let ex4 = Modul {
        defns: &[
            Decl::Include(Expr::Const(Lit::Sym("Math"))),
            Decl::Method(Copat::This(Pat::Var("pi")), Expr::Const(Lit::Flt(PI))),
            Decl::Method(
                Copat::App(&Copat::This(Pat::Var("area")), Pat::Var("r")),
                Expr::App(
                    &Expr::App(&Expr::Var("times"), &Expr::Var("pi")),
                    &Expr::App(
                        &Expr::App(
                            &Expr::Dot(&Expr::Const(Lit::Sym("Math")), Lit::Sym("Exp")),
                            &Expr::Var("r"),
                        ),
                        &Expr::Const(Lit::Int(2)),
                    ),
                ),
            ),
        ],
    };
    compare_output(&ex4);

    let ex5 = Modul {
        defns: &[
            Decl::Method(
                Copat::App(&Copat::This(Pat::Var("fact")), Pat::Const(Lit::Int(0))),
                Expr::Const(Lit::Int(1)),
            ),
            Decl::Method(
                Copat::App(&Copat::This(Pat::Var("fact")), Pat::Var("n")),
                Expr::App(
                    &Expr::App(&Expr::Var("times"), &Expr::Var("n")),
                    &Expr::App(
                        &Expr::Var("fact"),
                        &Expr::App(
                            &Expr::App(&Expr::Var("minus"), &Expr::Var("n")),
                            &Expr::Const(Lit::Int(1)),
                        ),
                    ),
                ),
            ),
        ],
    };
    compare_output(&ex5);

    let ex6 = Modul {
        defns: &[
            Decl::Method(
                Copat::Dot(&Copat::This(Pat::Var("zeroes")), Lit::Sym("Head")),
                Expr::Const(Lit::Int(0)),
            ),
            Decl::Method(
                Copat::Dot(&Copat::This(Pat::Var("zeroes")), Lit::Sym("Tail")),
                Expr::Var("zeroes"),
            ),
        ],
    };
    compare_output(&ex6);

    let ex7 = Modul {
        defns: &[
            Decl::Method(
                Copat::Dot(
                    &Copat::App(&Copat::This(Pat::Var("stutter")), Pat::Var("n")),
                    Lit::Sym("Head"),
                ),
                Expr::Var("n"),
            ),
            Decl::Method(
                Copat::Dot(
                    &Copat::Dot(
                        &Copat::App(&Copat::This(Pat::Var("stutter")), Pat::Var("n")),
                        Lit::Sym("Tail"),
                    ),
                    Lit::Sym("Head"),
                ),
                Expr::Var("n"),
            ),
            Decl::Method(
                Copat::Dot(
                    &Copat::Dot(
                        &Copat::App(&Copat::This(Pat::Var("stutter")), Pat::Var("n")),
                        Lit::Sym("Tail"),
                    ),
                    Lit::Sym("Tail"),
                ),
                Expr::App(
                    &Expr::Var("stutter"),
                    &Expr::App(
                        &Expr::App(&Expr::Var("plus"), &Expr::Var("n")),
                        &Expr::Const(Lit::Int(1)),
                    ),
                ),
            ),
        ],
    };
    compare_output(&ex7);

    let ex8 = Modul {
        defns: &[
            Decl::Method(
                Copat::Dot(
                    &Copat::App(
                        &Copat::App(&Copat::This(Pat::Var("zigzag")), Pat::Var("e")),
                        Pat::Var("o"),
                    ),
                    Lit::Sym("Head"),
                ),
                Expr::Dot(&Expr::Var("e"), Lit::Sym("Head")),
            ),
            Decl::Method(
                Copat::Dot(
                    &Copat::Dot(
                        &Copat::App(
                            &Copat::App(&Copat::This(Pat::Var("zigzag")), Pat::Var("e")),
                            Pat::Var("o"),
                        ),
                        Lit::Sym("Tail"),
                    ),
                    Lit::Sym("Head"),
                ),
                Expr::Dot(&Expr::Var("o"), Lit::Sym("Head")),
            ),
            Decl::Method(
                Copat::Dot(
                    &Copat::Dot(
                        &Copat::App(
                            &Copat::App(&Copat::This(Pat::Var("zigzag")), Pat::Var("e")),
                            Pat::Var("o"),
                        ),
                        Lit::Sym("Tail"),
                    ),
                    Lit::Sym("Tail"),
                ),
                Expr::App(
                    &Expr::App(
                        &Expr::Var("zigzag"),
                        &Expr::Dot(&Expr::Var("e"), Lit::Sym("Tail")),
                    ),
                    &Expr::Dot(&Expr::Var("o"), Lit::Sym("Tail")),
                ),
            ),
        ],
    };
    compare_output(&ex8);

    let ex9 = Modul {
        defns: &[
            Decl::Method(
                Copat::App(
                    &Copat::App(&Copat::This(Pat::Var("and")), Pat::Const(Lit::Sym("True"))),
                    Pat::Const(Lit::Sym("True")),
                ),
                Expr::Const(Lit::Sym("True")),
            ),
            Decl::Method(
                Copat::App(
                    &Copat::App(&Copat::This(Pat::Var("and")), Pat::Unused),
                    Pat::Unused,
                ),
                Expr::Const(Lit::Sym("False")),
            ),
        ],
    };
    compare_output(&ex9);

    let ex10 = Modul {
        defns: &[
            Decl::Method(
                Copat::Dot(
                    &Copat::This(Pat::App(&Pat::Const(Lit::Sym("Num")), &Pat::Var("n"))),
                    Lit::Sym("Eval"),
                ),
                Expr::Var("n"),
            ),
            Decl::Method(
                Copat::Dot(
                    &Copat::This(Pat::App(
                        &Pat::App(&Pat::Const(Lit::Sym("Add")), &Pat::Var("l")),
                        &Pat::Var("r"),
                    )),
                    Lit::Sym("Eval"),
                ),
                Expr::App(
                    &Expr::App(
                        &Expr::Var("plus"),
                        &Expr::Dot(&Expr::Var("l"), Lit::Sym("Eval")),
                    ),
                    &Expr::Dot(&Expr::Var("r"), Lit::Sym("Eval")),
                ),
            ),
            Decl::Method(
                Copat::Dot(
                    &Copat::This(Pat::App(
                        &Pat::App(&Pat::Const(Lit::Sym("Mul")), &Pat::Var("l")),
                        &Pat::Var("r"),
                    )),
                    Lit::Sym("Eval"),
                ),
                Expr::App(
                    &Expr::App(
                        &Expr::Var("times"),
                        &Expr::Dot(&Expr::Var("l"), Lit::Sym("Eval")),
                    ),
                    &Expr::Dot(&Expr::Var("r"), Lit::Sym("Eval")),
                ),
            ),
        ],
    };
    compare_output(&ex10);
}
