#[derive(Debug)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub range: &'a str,
    pub rest: &'a str,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum TokenKind {
    ArrowDouble,
    ArrowSingle,
    Assignment,
    BraceClose,
    BraceOpen,
    Colon,
    Comma,
    Dot,
    KwdFrom,
    KwdGuard,
    KwdImport,
    KwdLet,
    KwdModule,
    KwdProc,
    KwdYield,
    Identifier,
    Number,
    OpAddition,
    OpEquality,
    OpSubtraction,
    ParenClose,
    ParenOpen,
    SemiColon,
    StringLiteral,
}

#[derive(Clone, Copy, Debug)]
pub enum TokenizingError {
    UnexpectedEOF,
    UnexpectedToken,
}

#[derive(Clone)]
pub struct TokenIter<'a> {
    remaining: core::str::Chars<'a>,
}

enum TokenizerState {
    Initial,
    MatchingIdentifier {
        length: usize,
    },
    MatchingNumber {
        length: usize,
    },
    MatchingPartial {
        fallback: TokenKind,
        terminals: &'static [(char, TokenKind)],
    },
    MatchingString {
        length: usize,
    },
}

fn simple_token_type(c: char) -> Option<TokenKind> {
    match c {
        '}' => Some(TokenKind::BraceClose),
        '{' => Some(TokenKind::BraceOpen),
        ':' => Some(TokenKind::Colon),
        ',' => Some(TokenKind::Comma),
        '.' => Some(TokenKind::Dot),
        ')' => Some(TokenKind::ParenClose),
        '(' => Some(TokenKind::ParenOpen),
        '+' => Some(TokenKind::OpAddition),
        ';' => Some(TokenKind::SemiColon),
        _ => None,
    }
}

fn normalize_token_identifier(id: &str) -> TokenKind {
    match id {
        "from" => TokenKind::KwdFrom,
        "guard" => TokenKind::KwdGuard,
        "import" => TokenKind::KwdImport,
        "let" => TokenKind::KwdLet,
        "module" => TokenKind::KwdModule,
        "proc" => TokenKind::KwdProc,
        "yield" => TokenKind::KwdYield,
        _ => TokenKind::Identifier,
    }
}

impl<'a> std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ArrowDouble => write!(f, "=>"),
            Self::ArrowSingle => write!(f, "->"),
            Self::Assignment => write!(f, "="),
            Self::BraceClose => write!(f, "}}"),
            Self::BraceOpen => write!(f, "{{"),
            Self::Colon => write!(f, ":"),
            Self::Comma => write!(f, ","),
            Self::Dot => write!(f, "."),
            Self::KwdFrom => write!(f, "from"),
            Self::KwdGuard => write!(f, "guard"),
            Self::KwdImport => write!(f, "import"),
            Self::KwdLet => write!(f, "let"),
            Self::KwdModule => write!(f, "module"),
            Self::KwdProc => write!(f, "proc"),
            Self::KwdYield => write!(f, "yield"),
            Self::Identifier => write!(f, "identifier"),
            Self::Number => write!(f, "number"),
            Self::OpAddition => write!(f, "+"),
            Self::OpEquality => write!(f, "=="),
            Self::OpSubtraction => write!(f, "-"),
            Self::ParenClose => write!(f, ")"),
            Self::ParenOpen => write!(f, "("),
            Self::SemiColon => write!(f, ";"),
            Self::StringLiteral => write!(f, "string literal"),
        }
    }
}

impl<'a> Iterator for TokenIter<'a> {
    type Item = Result<Token<'a>, TokenizingError>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut state = TokenizerState::Initial;
        let mut iter = self.remaining.clone().peekable();
        let mut rest = self.remaining.clone();

        // Consume tokens
        while let Some(c) = iter.peek() {
            match state {
                TokenizerState::Initial => match c {
                    ' ' | '\t' | '\n' | '\r' => {
                        // Ignore, but move the range ptr forward
                        _ = rest.next();
                    }
                    'a'..='z' | 'A'..='Z' | '_' | '$' => state = TokenizerState::MatchingIdentifier { length: 1 },
                    '0'..='9' => state = TokenizerState::MatchingNumber { length: 1 },
                    '=' => {
                        state = TokenizerState::MatchingPartial {
                            fallback: TokenKind::Assignment,
                            terminals: &[('=', TokenKind::OpEquality), ('>', TokenKind::ArrowDouble)],
                        }
                    }
                    '-' => {
                        state = TokenizerState::MatchingPartial {
                            fallback: TokenKind::OpSubtraction,
                            terminals: &[('>', TokenKind::ArrowSingle)],
                        }
                    }
                    '"' => state = TokenizerState::MatchingString { length: 1 },
                    _ => {
                        if let Some(kind) = simple_token_type(*c) {
                            state = TokenizerState::MatchingPartial {
                                fallback: kind,
                                terminals: &[],
                            };
                        } else {
                            return Some(Err(TokenizingError::UnexpectedToken));
                        }
                    }
                },
                TokenizerState::MatchingIdentifier { length } => match c {
                    'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => {
                        // Keep matching
                        state = TokenizerState::MatchingIdentifier { length: length + 1 }
                    }
                    _ => {
                        break;
                    }
                },
                TokenizerState::MatchingNumber { length } => match c {
                    '0'..='9' | '.' => {
                        // Keep matching
                        // TODO: properly tokenize numbers (ex: can't have multiple periods)
                        state = TokenizerState::MatchingNumber { length: length + 1 }
                    }
                    _ => {
                        break;
                    }
                },
                TokenizerState::MatchingPartial { .. } => {
                    break;
                }
                TokenizerState::MatchingString { length } => match c {
                    '"' => {
                        // Eat the token and end the string
                        _ = iter.next();
                        _ = self.remaining.next();
                        state = TokenizerState::MatchingString { length: length + 1 };
                        break;
                    }
                    _ => state = TokenizerState::MatchingString { length: length + 1 },
                },
            }

            _ = iter.next();
            _ = self.remaining.next();
        }

        // Consolidate
        match state {
            TokenizerState::Initial => {
                // EOF
                None
            }
            TokenizerState::MatchingIdentifier { length } => {
                let range = &rest.as_str()[0..length];
                Some(Ok(Token {
                    kind: normalize_token_identifier(range),
                    rest: &rest.as_str(),
                    range,
                }))
            }
            TokenizerState::MatchingNumber { length } => Some(Ok(Token {
                kind: TokenKind::Number,
                rest: &rest.as_str(),
                range: &rest.as_str()[0..length],
            })),
            TokenizerState::MatchingPartial { fallback, terminals } => {
                for (expected_c, kind) in terminals {
                    match iter.peek() {
                        Some(next_c) if next_c == expected_c => {
                            // Remember to increment our internal iterator for the additionally
                            // consumed character
                            _ = self.remaining.next();

                            return Some(Ok(Token {
                                kind: *kind,
                                rest: &rest.as_str(),
                                range: &rest.as_str()[0..2],
                            }));
                        }
                        _ => {}
                    }
                }

                Some(Ok(Token {
                    kind: fallback,
                    rest: &rest.as_str(),
                    range: &rest.as_str()[0..1],
                }))
            }
            TokenizerState::MatchingString { length } => Some(Ok(Token {
                kind: TokenKind::StringLiteral,
                rest: &rest.as_str(),
                range: &rest.as_str()[0..length],
            })),
        }
    }
}

pub struct StringNormalizer;

#[derive(Debug)]
pub enum StringNormalizationError {
    UnexpectedStringFormat(String),
}

pub struct Scanner<'a> {
    source: &'a str,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self { source }
    }

    pub fn iter(&self) -> TokenIter<'a> {
        TokenIter {
            remaining: self.source.chars(),
        }
    }
}

impl StringNormalizer {
    pub fn extract_and_unescape(s: &str) -> Result<String, StringNormalizationError> {
        // TODO: Actually implement this
        if s.len() < 2 {
            Err(StringNormalizationError::UnexpectedStringFormat(String::from(
                "Expected string to be at least two (2) characters long",
            )))
        } else {
            Ok(s[1..s.len() - 1].to_string())
        }
    }
}

impl ToString for StringNormalizationError {
    fn to_string(&self) -> String {
        match self {
            StringNormalizationError::UnexpectedStringFormat(s) => s.clone(),
        }
    }
}
