use logos::Logos;

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\n\f]+")] // Ignore this regex pattern between tokens
#[logos(skip r"\/\/[^\r\n]*")] // Ignore comments
pub enum Token<'a> {
    #[token("#include \"minicio.h\"")]
    Include,

    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token(";")]
    Semicolon,
    #[token(",")]
    Comma,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("(")]
    LParens,
    #[token(")")]
    RParens,

    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("for")]
    For,
    #[token("continue")]
    Continue,
    #[token("break")]
    Break,
    #[token("return")]
    Return,

    #[token("int")]
    IntType,
    #[token("bool")]
    BoolType,
    #[token("char")]
    CharType,
    #[token("void")]
    VoidType,

    #[token("=")]
    Assign,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("/")]
    Div,
    #[token("*")]
    Mult,
    #[token("!")]
    Not,

    #[token("&&")]
    And,
    #[token("||")]
    Or,
    #[token("==")]
    Eq,
    #[token("!=")]
    Neq,
    #[token("<")]
    Lt,
    #[token("<=")]
    Lte,
    #[token(">")]
    Gt,
    #[token(">=")]
    Gte,

    #[regex("[a-zA-Z][a-zA-Z0-9_]*", |lex| lex.slice())]
    Identifier(&'a str),

    #[regex("[0]|([1-9][0-9]*)", |lex| lex.slice().parse::<i32>().unwrap())]
    Int(i32),

    #[regex("\'.\'", |lex| lex.slice().chars().nth(1).unwrap())]
    Char(char),

    #[token("true", |_| true)]
    #[token("false", |_| false)]
    Bool(bool),

    Error,
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::read_to_string;

    fn lex_file(path: &str) -> Result<(), ()> {
        let contents = read_to_string(path).unwrap();

        let lex = Token::lexer(&contents);

        lex.into_iter().for_each(|a| assert!(a.is_ok()));

        Ok(())
    }

    #[test]
    fn lex_conds() -> Result<(), ()> {
        lex_file("examples/conds.c")
    }

    #[test]
    fn lex_var() -> Result<(), ()> {
        lex_file("examples/var.c")
    }

    #[test]
    fn lex_loop() -> Result<(), ()> {
        lex_file("examples/loop.c")
    }
}
