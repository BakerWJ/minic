use crate::ast::*;
use chumsky::{input::ValueInput, prelude::*};

use crate::lexer::Token;

pub fn parser<'a, I>() -> impl Parser<'a, I, Program, extra::Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    let values = select! {
        Token::Int(x) => Expr::Int(x),
        Token::Bool(x) => Expr::Bool(x),
        Token::Char(x) => Expr::Char(x)
    };

    let integer = select! {
        Token::Int(x) => x,
    };

    let ident = select! {
        Token::Identifier(x) => x
    };

    let expr = recursive(|expr| {
        let parens = expr
            .clone()
            .delimited_by(just(Token::LParens), just(Token::RParens));

        let list_var = ident
            .then_ignore(just(Token::LBracket))
            .then(expr.clone())
            .then_ignore(just(Token::RBracket))
            .map(|(a, b)| Variable::List(a.to_string(), b));

        let var = list_var.or(ident.map(|a| Variable::Basic(a.to_string())));

        let expr_var = var.clone().map(|a| Expr::Variable(Box::new(a)));

        let items = expr
            .clone()
            .separated_by(just(Token::Comma))
            .collect::<Vec<_>>();

        let call = ident
            .then(items.delimited_by(just(Token::LParens), just(Token::RParens)))
            .map(|(a, b)| Expr::FunctionCall(a.to_string(), b));

        let assignment = var
            .then_ignore(just(Token::Assign))
            .then(expr.clone())
            .map(|(a, b)| Expr::Assignment(Box::new(a), Box::new(b)));

        let u = choice((
            just(Token::Minus).to(Expr::Neg as fn(_) -> _),
            just(Token::Not).to(Expr::Not as fn(_) -> _),
        ));

        let unary_ops = u.then(expr.clone()).map(|(f, b)| f(Box::new(b)));

        let b = just(Token::Mult)
            .to(Expr::Mul as fn(_, _) -> _)
            .or(just(Token::Div).to(Expr::Div as fn(_, _) -> _))
            .or(just(Token::Plus).to(Expr::Add as fn(_, _) -> _))
            .or(just(Token::Minus).to(Expr::Sub as fn(_, _) -> _))
            .or(just(Token::Eq).to(Expr::Eq as fn(_, _) -> _))
            .or(just(Token::Neq).to(Expr::Neq as fn(_, _) -> _))
            .or(just(Token::Lt).to(Expr::Lt as fn(_, _) -> _))
            .or(just(Token::Lte).to(Expr::Lte as fn(_, _) -> _))
            .or(just(Token::Gt).to(Expr::Gt as fn(_, _) -> _))
            .or(just(Token::Gte).to(Expr::Gte as fn(_, _) -> _))
            .or(just(Token::And).to(Expr::And as fn(_, _) -> _))
            .or(just(Token::Or).to(Expr::Or as fn(_, _) -> _));

        let binaryops = values
            .or(parens.clone())
            .or(call.clone())
            .or(unary_ops.clone())
            .or(expr_var.clone())
            .foldl(b.then(expr.clone()).repeated(), |a, (f, b)| {
                f(Box::new(a), Box::new(b))
            });

        assignment
            .or(binaryops)
            .or(unary_ops)
            .or(parens)
            .or(call)
            .or(expr_var)
            .or(values)
    });

    let return_type = select! {
        Token::VoidType => ReturnType::Void,
        Token::CharType => ReturnType::Char,
        Token::IntType => ReturnType::Int,
        Token::BoolType => ReturnType::Bool
    };

    let var_type = select! {
        Token::CharType => VarType::Char,
        Token::IntType => VarType::Int,
        Token::BoolType => VarType::Bool
    };

    let variable_list = ident
        .then_ignore(just(Token::LBracket))
        .then(integer)
        .then_ignore(just(Token::RBracket))
        .map(|(a, b)| VariableList::List(a.to_string(), b));

    let variable = variable_list
        .clone()
        .or(ident.map(|a| VariableList::Basic(a.to_string())));

    let vardecl = var_type
        .then(variable.clone().separated_by(just(Token::Comma)).collect())
        .then_ignore(just(Token::Semicolon))
        .map(|(a, b)| VarDecl { t: a, variables: b });

    let statement = recursive(|stmt| {
        let if_else = just(Token::If)
            .ignore_then(
                expr.clone()
                    .delimited_by(just(Token::LParens), just(Token::RParens)),
            )
            .then(stmt.clone().repeated().collect())
            .then_ignore(just(Token::Else))
            .then(stmt.clone().repeated().collect())
            .map(|((a, b), c)| Statement::IfElse(a, b, c));

        let if_then = just(Token::If)
            .ignore_then(
                expr.clone()
                    .delimited_by(just(Token::LParens), just(Token::RParens)),
            )
            .then(stmt.clone().repeated().collect())
            .map(|(a, b)| Statement::If(a, b));

        let for_loop = just(Token::For)
            .ignored()
            .then_ignore(just(Token::LParens))
            .ignore_then(expr.clone().or_not())
            .then_ignore(just(Token::Semicolon))
            .then(expr.clone().or_not())
            .then_ignore(just(Token::Semicolon))
            .then(expr.clone().or_not())
            .then_ignore(just(Token::RParens))
            .then(stmt.clone())
            .map(|(((a, b), c), d)| Statement::For(a, b, c, Box::new(d)));

        let continue_stmt = just(Token::Continue)
            .ignored()
            .then_ignore(just(Token::Semicolon))
            .map(|_| Statement::Continue);

        let break_stmt = just(Token::Break)
            .ignored()
            .then_ignore(just(Token::Semicolon))
            .map(|_| Statement::Break);

        let expr_stmt = expr
            .clone()
            .then_ignore(just(Token::Semicolon))
            .map(Statement::Expr);

        let return_stmt = just(Token::Return)
            .ignore_then(expr.clone().or_not())
            .then_ignore(just(Token::Semicolon))
            .map(Statement::Return);

        let scope_stmt = just(Token::LBrace)
            .ignore_then(vardecl.clone().repeated().collect())
            .then(stmt.clone().repeated().collect())
            .then_ignore(just(Token::RBrace))
            .map(|(a, b)| {
                Statement::Scope(Scope {
                    vardecl: a,
                    stmts: b,
                })
            });

        expr_stmt
            .or(if_else)
            .or(if_then)
            .or(for_loop)
            .or(continue_stmt)
            .or(break_stmt)
            .or(return_stmt)
            .or(scope_stmt)
    });

    let scope = just(Token::LBrace)
        .ignore_then(vardecl.clone().repeated().collect())
        .then(statement.clone().repeated().collect())
        .then_ignore(just(Token::RBrace))
        .map(|(a, b)| Scope {
            vardecl: a,
            stmts: b,
        });

    let parameter = var_type.then(ident).map(|(a, b)| Parameter {
        t: a,
        name: b.to_string(),
    });

    let function = return_type
        .then(ident)
        .then(
            parameter
                .separated_by(just(Token::Comma))
                .collect()
                .delimited_by(just(Token::LParens), just(Token::RParens)),
        )
        .then(scope.clone().or_not())
        .map(|(((a, b), c), d)| Declaration::Fn(a, b.to_string(), c, d));

    let var_declaration = vardecl.clone().map(Declaration::VarDecl);

    let declaration = var_declaration.or(function).repeated().collect();

    just(Token::Include)
        .or_not()
        .then(declaration)
        .map(|(a, b)| match a {
            Some(_) => Program {
                header: true,
                decl: b,
            },
            None => Program {
                header: false,
                decl: b,
            },
        })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Token;
    use chumsky::{
        input::{Input, Stream},
        Parser,
    };
    use logos::Logos;
    use std::fs::read_to_string;

    fn parse_file(path: &str) -> Result<(), ()> {
        let contents = read_to_string(path).unwrap();

        let lex = Token::lexer(&contents);

        let token_iter = lex.spanned().map(|(tok, span)| match tok {
            Ok(tok) => (tok, span.into()),
            Err(()) => (Token::Error, span.into()),
        });

        let token_stream =
            Stream::from_iter(token_iter).spanned((contents.len()..contents.len()).into());

        let parsed = parser().parse(token_stream);

        assert!(parsed.into_result().is_ok());

        Ok(())
    }

    #[test]
    fn parse_conds() -> Result<(), ()> {
        parse_file("examples/conds.c")
    }

    #[test]
    fn parse_var() -> Result<(), ()> {
        parse_file("examples/var.c")
    }

    #[test]
    fn parse_loop() -> Result<(), ()> {
        parse_file("examples/loop.c")
    }
}
