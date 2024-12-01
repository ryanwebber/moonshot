use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;

use crate::ast;
use crate::types::{self, Numeric};
use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub moonshot);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum SemanticError {
    InvalidNumberLiteral,
}

impl Display for SemanticError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use self::SemanticError::*;
        match *self {
            InvalidNumberLiteral => write!(f, "Invalid number literal"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OwnedToken {
    pub offset: usize,
    pub substring: String,
}

impl Display for OwnedToken {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.substring)
    }
}

pub fn parse<'input>(src: &'input str) -> anyhow::Result<ast::ProgramFragment> {
    let parser = moonshot::ProgramFragmentParser::new();
    let fragment = parser.parse(src).map_err(|e| {
        e.map_token(|token| OwnedToken {
            offset: token.0,
            substring: token.1.to_string(),
        })
    })?;

    Ok(fragment)
}

fn to_string(s: &str) -> String {
    s.to_string()
}

fn to_numeric(s: &str) -> Result<Numeric, SemanticError> {
    let float_value: f32 = s.parse().map_err(|_| SemanticError::InvalidNumberLiteral)?;
    Ok(Numeric::Real(float_value.into()))
}

#[cfg(test)]
mod tests {
    use moonshot::{DirectiveParser, StringParser};

    use super::*;

    #[test]
    fn test_parse_string() {
        StringParser::new().parse("\"hello\"").unwrap();
        assert!(StringParser::new().parse("hello").is_err());
        assert!(StringParser::new().parse("\"hello").is_err());
        assert!(StringParser::new().parse("hello\"").is_err());
    }

    #[test]
    fn test_include_directive() {
        DirectiveParser::new().parse("inc \"hello\" as world").unwrap();
        assert!(DirectiveParser::new().parse("inc \"hello\" as \"world\"").is_err());
        assert!(DirectiveParser::new().parse("inc \"hello\" world").is_err());
        assert!(DirectiveParser::new().parse("inc \"hello\"").is_err());
    }

    #[test]
    fn test_program_directive() {
        let src = indoc::indoc! {"
            prog {
                .verb = 00;
                .noun = 01;
                .entry = my_entry;
            }
        "};

        DirectiveParser::new().parse(src).unwrap();
    }

    #[test]
    fn test_state_directive() {
        let src = indoc::indoc! {"
            state my_state (x: i15, y: BLAH) [
                x: i15 = x;
                y: BLAH = y;
            ] {
                32;
                thirty_two;
                let z: s = 32;
            }
        "};

        DirectiveParser::new().parse(src).unwrap();
    }
}
