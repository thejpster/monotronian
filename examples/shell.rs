//! # Test shell for Monotronian

extern crate monotronian;

use std::io::Write;

struct Output;

impl std::fmt::Write for Output {
    fn write_str(&mut self, s: &str) -> Result<(), std::fmt::Error> {
        print!("{}", s);
        Ok(())
    }
}

fn main() -> Result<(), std::io::Error> {
    let mut parser = monotronian::Parser::new();
    println!("Welcome to Monotronian.");
    println!("Copyright (c) Jonathan 'theJPster' Pallant 2018.");
    loop {
        print_prompt();
        let mut input = String::new();
        std::io::stdin().read_line(&mut input)?;
        println!("You wrote: {:?}", input);
        match parser.parse(&input, &mut Output) {
            Ok(value) => println!("{:?}", value),
            Err(err) => eprintln!("Error: {:?}", err),
        }
    }
}

fn print_prompt() {
    let stdout = ::std::io::stdout();
    let mut handle = stdout.lock();
    write!(handle, "> ").unwrap();
    handle.flush().unwrap();
}
