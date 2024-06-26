#![allow(dead_code)]

mod syntax;
use combine::EasyParser;
use parsing::parse::{modul, whole_input};
use syntax::sequential::{Lit, Modul, Name, Pat, PrettyPrint};

mod parsing;

use std::f64::consts::PI;
use std::fmt::{Debug, Display};
use std::io::BufRead;

fn compare_output<T: Debug + Display>(x: &T) {
    println!("----------");
    println!("{:?}", x);
    println!("==========");
    println!("{}", x);
    println!("----------");
}

fn test_pretty_printer() {
    let indentation_width = 80;
    loop {
        let stdin = std::io::stdin();
        for line in stdin.lock().lines() {
            let line = line.unwrap();
            let line = line.as_str();
            let v1 = match whole_input(modul()).easy_parse(line) {
                Ok((v1, _s)) => v1,
                Err(e) => {
                    println!("{:?}", e);
                    continue;
                }
            };
            let pretty_str = v1.to_pretty(indentation_width);
            let pretty_str = pretty_str.as_str();
            let v2 = match whole_input(modul()).easy_parse(pretty_str) {
                Ok((v2, _s)) => v2,
                Err(e) => {
                    println!("{:?}", e);
                    continue;
                }
            };
            if v1 != v2 {
                println!("Pretty printing failed");
            }
        }
    }
}

fn main() {
//     let ex1 = Name::id("self").bind().this();
//     compare_output(&ex1);

//     let ex2 = Name::id("print")
//         .refer()
//         .app(Lit::str("Hello, world!".to_string()).cnst());
//     compare_output(&ex2);

//     let ex3 = Name::id("inc")
//         .bind()
//         .this()
//         .app(Name::id("x").bind().atom())
//         .goes_to(
//             Name::id("plus")
//                 .refer()
//                 .app(Name::id("x").refer())
//                 .app(Lit::int(1).cnst()),
//         );
//     compare_output(&ex3);

//     let ex4 = Modul::top()
//         .then(Name::id("Math").refer().included())
//         .then(Name::id("pi").bind().this().goes_to(Lit::flt(PI).cnst()))
//         .then(
//             Name::id("area")
//                 .bind()
//                 .this()
//                 .app(Name::id("r").bind().atom())
//                 .goes_to(
//                     Name::id("times").refer().app(Name::id("pi").refer()).app(
//                         Name::id("Math")
//                             .sym()
//                             .cnst()
//                             .dot(Name::id("exp").sym())
//                             .app(Name::id("r").refer())
//                             .app(Lit::int(2).cnst()),
//                     ),
//                 ),
//         );

//     compare_output(&ex4);

//     let ex5 = Modul::top()
//         .then(
//             Name::id("fact")
//                 .bind()
//                 .this()
//                 .app(Lit::int(0).mtch().decons())
//                 .goes_to(Lit::int(1).cnst()),
//         )
//         .then(
//             Name::id("fact")
//                 .bind()
//                 .this()
//                 .app(Name::id("n").bind().atom())
//                 .goes_to(
//                     Name::id("times").refer().app(Name::id("n").refer()).app(
//                         Name::id("fact").refer().app(
//                             Name::id("minus")
//                                 .refer()
//                                 .app(Name::id("n").refer())
//                                 .app(Lit::int(1).cnst()),
//                         ),
//                     ),
//                 ),
//         );

//     compare_output(&ex5);

//     let ex6 = Modul::top()
//         .then(
//             Name::id("zeroes")
//                 .bind()
//                 .this()
//                 .dot(Name::id("Head").sym())
//                 .goes_to(Lit::int(0).cnst()),
//         )
//         .then(
//             Name::id("zeroes")
//                 .bind()
//                 .this()
//                 .dot(Name::id("Tail").sym())
//                 .goes_to(Name::id("zeroes").refer()),
//         );

//     compare_output(&ex6);

//     let ex7 = Modul::top()
//         .then(
//             Name::id("stutter")
//                 .bind()
//                 .this()
//                 .app(Name::id("n").bind().atom())
//                 .dot(Name::id("Head").sym())
//                 .goes_to(Name::id("n").refer()),
//         )
//         .then(
//             Name::id("stutter")
//                 .bind()
//                 .this()
//                 .app(Name::id("n").bind().atom())
//                 .dot(Name::id("Tail").sym())
//                 .dot(Name::id("Head").sym())
//                 .goes_to(Name::id("n").refer()),
//         )
//         .then(
//             Name::id("stutter")
//                 .bind()
//                 .this()
//                 .app(Name::id("n").bind().atom())
//                 .dot(Name::id("Tail").sym())
//                 .dot(Name::id("Tail").sym())
//                 .goes_to(
//                     Name::id("stutter").refer().app(
//                         Name::id("plus")
//                             .refer()
//                             .app(Name::id("n").refer())
//                             .app(Lit::int(1).cnst()),
//                     ),
//                 ),
//         );

//     compare_output(&ex7);

//     let ex8 = Modul::top()
//         .then(
//             Name::id("zigzag")
//                 .bind()
//                 .this()
//                 .app(Name::id("e").bind().atom())
//                 .app(Name::id("o").bind().atom())
//                 .dot(Name::id("Head").sym())
//                 .goes_to(Name::id("e").refer().dot(Name::id("Head").sym())),
//         )
//         .then(
//             Name::id("zigzag")
//                 .bind()
//                 .this()
//                 .app(Name::id("e").bind().atom())
//                 .app(Name::id("o").bind().atom())
//                 .dot(Name::id("Tail").sym())
//                 .dot(Name::id("Head").sym())
//                 .goes_to(Name::id("o").refer().dot(Name::id("Head").sym())),
//         )
//         .then(
//             Name::id("zigzag")
//                 .bind()
//                 .this()
//                 .app(Name::id("e").bind().atom())
//                 .app(Name::id("o").bind().atom())
//                 .dot(Name::id("Tail").sym())
//                 .dot(Name::id("Tail").sym())
//                 .goes_to(
//                     Name::id("zigzag")
//                         .refer()
//                         .app(Name::id("e").refer().dot(Name::id("Tail").sym()))
//                         .app(Name::id("o").refer().dot(Name::id("Tail").sym())),
//                 ),
//         );

//     compare_output(&ex8);

//     let ex9 = Modul::top()
//         .then(
//             Name::id("and")
//                 .bind()
//                 .this()
//                 .app(Name::id("True").sym().mtch().decons())
//                 .app(Name::id("True").sym().mtch().decons())
//                 .goes_to(Name::id("True").sym().cnst()),
//         )
//         .then(
//             Name::id("and")
//                 .bind()
//                 .this()
//                 .app(Pat::blank())
//                 .app(Pat::blank())
//                 .goes_to(Name::id("False").sym().cnst()),
//         );

//     compare_output(&ex9);

//     let ex10 = Modul::top()
//         .then(
//             (Name::id("Num")
//                 .sym()
//                 .switch()
//                 .this()
//                 .app(Name::id("n").bind().atom()))
//             .dot(Name::id("Eval").sym())
//             .goes_to(Name::id("n").refer()),
//         )
//         .then(
//             (Name::id("Add")
//                 .sym()
//                 .switch()
//                 .this()
//                 .app(Name::id("l").bind().atom())
//                 .app(Name::id("r").bind().atom()))
//             .dot(Name::id("Eval").sym())
//             .goes_to(
//                 Name::id("plus")
//                     .refer()
//                     .app(Name::id("l").refer().dot(Name::id("Eval").sym()))
//                     .app(Name::id("r").refer().dot(Name::id("Eval").sym())),
//             ),
//         )
//         .then(
//             (Name::id("Mul")
//                 .sym()
//                 .switch()
//                 .this()
//                 .app(Name::id("l").bind().atom())
//                 .app(Name::id("r").bind().atom()))
//             .dot(Name::id("Eval").sym())
//             .goes_to(
//                 Name::id("times")
//                     .refer()
//                     .app(Name::id("l").refer().dot(Name::id("Eval").sym()))
//                     .app(Name::id("r").refer().dot(Name::id("Eval").sym())),
//             ),
//         );

//     compare_output(&ex10);

//     let math_ex = whole_input(modul())
//         .easy_parse(
//             "
//             fact(0) -> 1;
//             fact(n) -> times(n)(fact(minus(n)(1)));",
//         )
//         .map(|(v, _s)| v)
//         .unwrap();

    /*
    println!("{}", math_ex);
    assert_eq!(
        math_ex,
        ex5
    );
    */
    // test_pretty_printer();

    loop {
        let stdin = std::io::stdin();
        for line in stdin.lock().lines() {
            let line = line.unwrap();
            let line = line.as_str();
            match whole_input(modul()).easy_parse(line).map(|(v, _s)| v) {
                Ok(v) => println!("{v:?}\n{}", v.to_pretty(80)),
                Err(e) => println!("{e}"),
            };
        }

        // store input parse result

        // eval
    }
}
