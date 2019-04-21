//! # Test shell for Monotronian

extern crate monotronian;
use std::io::Write;

fn main() -> Result<(), std::io::Error> {
    println!("Welcome to Monotronian.");
    println!("Copyright (c) Jonathan 'theJPster' Pallant 2019.");
    loop {
        print_prompt();
        let mut input = String::new();
        let mut parser = monotronian::Parser::new();
        std::io::stdin().read_line(&mut input)?;
        println!("You wrote: {:?}", input);
        match parser.parse(input.as_bytes()) {
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
