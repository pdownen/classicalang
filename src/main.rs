#![allow(dead_code)]

mod syntax;
use syntax::{Lit, Modul, Name, Pat};

mod parsing;

use std::f64::consts::PI;
use std::fmt::{Debug, Display};

fn compare_output<T: Debug + Display>(x: &T) {
    println!("----------");
    println!("{:?}", x);
    println!("==========");
    println!("{}", x);
    println!("----------");
}

fn main() {
    let ex1 = Name::id("self").bind().this();
    compare_output(&ex1);

    let ex2 = Name::id("print")
        .refer()
        .app(Lit::Str("Hello, world!".to_string()).cnst());
    compare_output(&ex2);

    let ex3 = Name::id("inc")
        .bind()
        .this()
        .app(Name::id("plus").bind())
        .goes_to(
            Name::id("plus")
                .refer()
                .app(Name::id("x").refer())
                .app(Lit::Int(1).cnst()),
        );
    compare_output(&ex3);

    let ex4 = Modul::top()
        .then(Name::id("Math").refer().included())
        .then(Name::id("pi").bind().this().goes_to(Lit::Flt(PI).cnst()))
        .then(
            Name::id("area")
                .bind()
                .this()
                .app(Name::id("r").bind())
                .goes_to(
                    Name::id("times").refer().app(Name::id("pi").refer()).app(
                        Name::id("Math")
                            .sym()
                            .cnst()
                            .dot(Name::id("exp").sym())
                            .app(Name::id("r").refer())
                            .app(Lit::Int(2).cnst()),
                    ),
                ),
        );

    compare_output(&ex4);

    let ex5 = Modul::top()
        .then(
            Name::id("fact")
                .bind()
                .this()
                .app(Lit::Int(0).mtch())
                .goes_to(Lit::Int(1).cnst()),
        )
        .then(
            Name::id("fact")
                .bind()
                .this()
                .app(Name::id("n").bind())
                .goes_to(
                    Name::id("times").refer().app(Name::id("n").refer()).app(
                        Name::id("fact").refer().app(
                            Name::id("minus")
                                .refer()
                                .app(Name::id("n").refer())
                                .app(Lit::Int(1).cnst()),
                        ),
                    ),
                ),
        );

    compare_output(&ex5);

    let ex6 = Modul::top()
        .then(
            Name::id("zeroes")
                .bind()
                .this()
                .dot(Name::id("Head").sym())
                .goes_to(Lit::Int(0).cnst()),
        )
        .then(
            Name::id("zeroes")
                .bind()
                .this()
                .dot(Name::id("Tail").sym())
                .goes_to(Name::id("zeroes").refer()),
        );

    compare_output(&ex6);

    let ex7 = Modul::top()
        .then(
            Name::id("stutter")
                .bind()
                .this()
                .app(Name::id("n").bind())
                .dot(Name::id("Head").sym())
                .goes_to(Name::id("n").refer()),
        )
        .then(
            Name::id("stutter")
                .bind()
                .this()
                .app(Name::id("n").bind())
                .dot(Name::id("Tail").sym())
                .dot(Name::id("Head").sym())
                .goes_to(Name::id("n").refer()),
        )
        .then(
            Name::id("stutter")
                .bind()
                .this()
                .app(Name::id("n").bind())
                .dot(Name::id("Tail").sym())
                .dot(Name::id("Tail").sym())
                .goes_to(
                    Name::id("stutter").refer().app(
                        Name::id("plus")
                            .refer()
                            .app(Name::id("n").refer())
                            .app(Lit::Int(1).cnst()),
                    ),
                ),
        );

    compare_output(&ex7);

    let ex8 = Modul::top()
        .then(
            Name::id("zigzag")
                .bind()
                .this()
                .app(Name::id("e").bind())
                .app(Name::id("o").bind())
                .dot(Name::id("Head").sym())
                .goes_to(Name::id("e").refer().dot(Name::id("Head").sym())),
        )
        .then(
            Name::id("zigzag")
                .bind()
                .this()
                .app(Name::id("e").bind())
                .app(Name::id("o").bind())
                .dot(Name::id("Tail").sym())
                .dot(Name::id("Head").sym())
                .goes_to(Name::id("o").refer().dot(Name::id("Head").sym())),
        )
        .then(
            Name::id("zigzag")
                .bind()
                .this()
                .app(Name::id("e").bind())
                .app(Name::id("o").bind())
                .dot(Name::id("Tail").sym())
                .dot(Name::id("Tail").sym())
                .goes_to(
                    Name::id("zigzag")
                        .refer()
                        .app(Name::id("e").refer().dot(Name::id("Tail").sym()))
                        .app(Name::id("o").refer().dot(Name::id("Tail").sym())),
                ),
        );

    compare_output(&ex8);

    let ex9 = Modul::top()
        .then(
            Name::id("and")
                .bind()
                .this()
                .app(Name::id("True").sym().mtch())
                .app(Name::id("True").sym().mtch())
                .goes_to(Name::id("True").sym().cnst()),
        )
        .then(
            Name::id("and")
                .bind()
                .this()
                .app(Pat::Unused)
                .app(Pat::Unused)
                .goes_to(Name::id("False").sym().cnst()),
        );

    compare_output(&ex9);

    let ex10 = Modul::top()
        .then(
            Name::id("Num")
                .sym()
                .mtch()
                .app(Name::id("n").bind())
                .this()
                .dot(Name::id("Eval").sym())
                .goes_to(Name::id("n").refer()),
        )
        .then(
            Name::id("Add")
                .sym()
                .mtch()
                .app(Name::id("l").bind())
                .app(Name::id("r").bind())
                .this()
                .dot(Name::id("Eval").sym())
                .goes_to(
                    Name::id("plus")
                        .refer()
                        .app(Name::id("l").refer().dot(Name::id("Eval").sym()))
                        .app(Name::id("r").refer().dot(Name::id("Eval").sym())),
                ),
        )
        .then(
            Name::id("mul")
                .sym()
                .mtch()
                .app(Name::id("l").bind())
                .app(Name::id("r").bind())
                .this()
                .dot(Name::id("Eval").sym())
                .goes_to(
                    Name::id("times")
                        .refer()
                        .app(Name::id("l").refer().dot(Name::id("Eval").sym()))
                        .app(Name::id("r").refer().dot(Name::id("Eval").sym())),
                ),
        );

    compare_output(&ex10);
}
