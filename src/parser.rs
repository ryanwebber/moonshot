use std::marker::PhantomData;
use std::slice::Iter;

use crate::ast::*;
use crate::sexpr;
use crate::tokenizer::*;

#[derive(Clone)]
pub struct TokenStream<'a> {
    iter: Iter<'a, Token<'a>>,
}

#[derive(Debug)]
pub struct SyntaxError<'a> {
    pub position: Option<&'a Token<'a>>,
    pub context: Vec<String>,
}

type ParseResult<'a, T> = Result<(TokenStream<'a>, T), SyntaxError<'a>>;

trait Parser<'a> {
    type Value: Sized;
    fn try_parse_from(&self, stream: TokenStream<'a>) -> ParseResult<'a, Self::Value>;

    fn map<U>(self, f: fn(Self::Value) -> U) -> MappedParser<'a, Self, U>
    where
        Self: Sized,
    {
        MappedParser { p: self, mapfn: f }
    }
}

struct TokenParser(TokenKind);

struct EmbeddedParser<'a, F, T>
where
    F: Fn(TokenStream<'a>) -> Result<(TokenStream<'a>, T), SyntaxError<'a>>,
{
    _phantom: PhantomData<&'a T>,
    f: F,
}

struct MappedParser<'a, P, U>
where
    P: Parser<'a>,
{
    p: P,
    mapfn: fn(P::Value) -> U,
}

#[rustfmt::skip]
macro_rules! parse_sequence {
    ($($parser:expr),*) => {
        {
            let f: fn(TokenStream<'a>) -> Result<(TokenStream<'a>, _), SyntaxError<'a>> = |stream| {
                let mut s = stream.clone();
                let value = (
                    $(
                        {
                            let (s_prime, x) = ($parser).try_parse_from(s.clone())?;
                            s = s_prime;
                            x
                        },
                    )*
                );

                Ok((s, value))
            };

            EmbeddedParser { f, _phantom: PhantomData }
        }
    }
}

#[rustfmt::skip]
macro_rules! parse_first {
    ($($parser:expr),*) => {
        {
            let f: fn(TokenStream<'a>) -> Result<(TokenStream<'a>, _), SyntaxError<'a>> = |stream| {
                let position = stream.position();
                $(
                    {
                        match ($parser).try_parse_from(stream.clone()) {
                            Ok((s, value)) => return Ok((s, value)),
                            _ => {}
                        };
                    }
                )*

                Err(SyntaxError::new(position))
            };

            EmbeddedParser { f, _phantom: PhantomData }
        }
    }
}

#[rustfmt::skip]
macro_rules! parse_left_recursive_sequence {
    (alpha: $aparser:expr, beta: $bparser:expr) => {
        {
            parse_sequence!(
                ($bparser),
                EmbeddedParser {
                    f: |mut stream| {
                        let mut accum: Vec<_> = Vec::new();
                        loop {
                            match ($aparser).try_parse_from(stream.clone()) {
                                Ok((s, value)) => {
                                    stream = s;
                                    accum.push(value);
                                },
                                _ => {
                                    break;
                                }
                            };
                        }
                        
                        Ok((stream, accum))
                    },
                    _phantom: PhantomData
                }
            )
        }
    }
}

impl<'a, F, T> Parser<'a> for EmbeddedParser<'a, F, T>
where
    F: Fn(TokenStream<'a>) -> Result<(TokenStream<'a>, T), SyntaxError<'a>>,
{
    type Value = T;

    fn try_parse_from(&self, stream: TokenStream<'a>) -> ParseResult<'a, Self::Value> {
        (self.f)(stream)
    }
}

impl<'a, P, U> Parser<'a> for MappedParser<'a, P, U>
where
    P: Parser<'a>,
{
    type Value = U;
    fn try_parse_from(&self, stream: TokenStream<'a>) -> ParseResult<'a, Self::Value> {
        match self.p.try_parse_from(stream) {
            Ok((s, t)) => Ok((s, (self.mapfn)(t))),
            Err(e) => Err(e),
        }
    }
}

impl<'a> Parser<'a> for TokenParser {
    type Value = ();
    fn try_parse_from(&self, mut stream: TokenStream<'a>) -> ParseResult<'a, Self::Value> {
        let position = stream.position();
        match stream.next() {
            Some(Token { kind, .. }) if *kind == self.0 => Ok((stream, ())),
            _ => Err(SyntaxError::new(position).with_context(format!("Expected: {}", self.0))),
        }
    }
}

struct IdentifierParser;
struct NumberLiteralParser;
struct PrimaryExpressionParser;
struct AddativeExpressionParser;
struct AddativeExpressionParser2;
struct ExpressionParser;

impl<'a> Parser<'a> for IdentifierParser {
    type Value = Identifier<'a>;
    fn try_parse_from(&self, mut stream: TokenStream<'a>) -> ParseResult<'a, Self::Value> {
        match stream.next() {
            Some(Token {
                kind: TokenKind::Identifier,
                range,
                ..
            }) => Ok((stream, Self::Value { name: range })),
            _ => Err(SyntaxError::new(stream.position()).with_context(format!("Expected identifier"))),
        }
    }
}

impl<'a> Parser<'a> for NumberLiteralParser {
    type Value = NumberLiteral<'a>;
    fn try_parse_from(&self, mut stream: TokenStream<'a>) -> ParseResult<'a, Self::Value> {
        match stream.next() {
            Some(Token {
                kind: TokenKind::Number,
                range,
                ..
            }) => Ok((stream, Self::Value { value: range })),
            _ => Err(SyntaxError::new(stream.position()).with_context(format!("Expected number literal"))),
        }
    }
}

impl<'a> Parser<'a> for PrimaryExpressionParser {
    type Value = Expression<'a>;
    fn try_parse_from(&self, stream: TokenStream<'a>) -> ParseResult<'a, Self::Value> {
        parse_first!(
            IdentifierParser.map(|identifier| Expression::Dereference { identifier }),
            NumberLiteralParser.map(|literal| Expression::Constant { literal }),
            parse_sequence!(
                TokenParser(TokenKind::ParenOpen),
                ExpressionParser,
                TokenParser(TokenKind::ParenClose)
            )
            .map(|v| v.1)
        )
        .try_parse_from(stream)
    }
}

impl<'a> Parser<'a> for AddativeExpressionParser {
    type Value = Expression<'a>;
    fn try_parse_from(&self, stream: TokenStream<'a>) -> ParseResult<'a, Self::Value> {
        parse_left_recursive_sequence!(
            alpha:
                parse_sequence!(
                    parse_first!(
                        TokenParser(TokenKind::OpAddition).map(|_| Operator::Addition),
                        TokenParser(TokenKind::OpSubtraction).map(|_| Operator::Addition)
                    ),
                    PrimaryExpressionParser
                ),
            beta: PrimaryExpressionParser
        )
        .try_parse_from(stream)
        .map(|(s, (mut expr, rest))| {
            for (op, rhs) in rest.into_iter() {
                expr = Expression::BinOp {
                    lhs: Box::new(expr),
                    op,
                    rhs: Box::new(rhs),
                };
            }

            (s, expr)
        })
    }
}

impl<'a> Parser<'a> for ExpressionParser {
    type Value = Expression<'a>;
    fn try_parse_from(&self, stream: TokenStream<'a>) -> ParseResult<'a, Self::Value> {
        AddativeExpressionParser.try_parse_from(stream)
    }
}

impl<'a> SyntaxError<'a> {
    fn new(position: Option<&'a Token<'a>>) -> Self {
        Self {
            position,
            context: vec![],
        }
    }

    fn with_context(mut self, ctx: String) -> SyntaxError<'a> {
        self.context.push(ctx);
        self
    }
}

impl<'a> std::fmt::Display for SyntaxError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.position {
            Some(token) => {
                writeln!(f, "At: {}", token.range)?;
            }
            None => {
                writeln!(f, "At: <end of source>")?;
            }
        }

        writeln!(f, "Syntax error")?;
        for (i, ctx) in self.context.iter().enumerate().take(30) {
            write!(f, "  {: <1$}", "", i)?;
            writeln!(f, "{}", ctx)?;
        }

        Ok(())
    }
}

impl<'a> TokenStream<'a> {
    pub fn new(tokens: &'a [Token<'a>]) -> TokenStream<'a> {
        TokenStream { iter: tokens.iter() }
    }

    fn next(&mut self) -> Option<&'a Token<'a>> {
        self.iter.next()
    }

    fn position(&self) -> Option<&'a Token<'a>> {
        self.iter.clone().next()
    }

    fn sample_source(&self) -> String {
        match self.iter.clone().next() {
            Some(token) => {
                let source: String = token.rest.chars().take(24).take_while(|c| *c != '\r' && *c != '\n').collect();
                format!("{}...", source)
            }
            None => String::from("<^Z>"),
        }
    }
}

pub fn delete_me<'a>(tokens: &'a [Token<'a>]) -> Result<Expression<'a>, SyntaxError<'a>> {
    let expr = ExpressionParser.try_parse_from(TokenStream::new(&tokens)).map(|(_, fs)| fs)?;
    println!("Got: {}", sexpr::Value::from(&expr));
    Ok(expr)
}

pub fn try_parse_from<'a>(_stream: &mut TokenStream<'a>) -> ParseResult<'a, Parse<'a>> {
    todo!()
}

pub fn try_parse_all<'a>(tokens: &'a [Token<'a>]) -> Result<Parse, SyntaxError<'a>> {
    try_parse_from(&mut TokenStream::new(&tokens)).map(|(_, parse)| parse)
}
