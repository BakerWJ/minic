use std::{fs::read_to_string, path::PathBuf};

use chumsky::{
    input::{Input, Stream},
    Parser,
};
use clap::Parser as Clap;
use logos::Logos;
use parser::parser;

mod ast;
mod lexer;
mod parser;

#[derive(Clap, Debug)]
#[command(version, about)]
struct Args {
    #[arg(short, long)]
    file: PathBuf,
}

fn main() {
    let a = Args::parse();
    let contents = read_to_string(a.file).unwrap();

    let lex = lexer::Token::lexer(&contents);

    let token_iter = lex.spanned().map(|(tok, span)| match tok {
        Ok(tok) => (tok, span.into()),
        Err(()) => (lexer::Token::Error, span.into()),
    });

    let token_stream =
        Stream::from_iter(token_iter).spanned((contents.len()..contents.len()).into());

    let _ = parser().parse(token_stream);
}
