
pub struct Token<'a> {
    pub kind: TokenKind,
    pub range: &'a str,
    pub rest: &'a str,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum TokenKind {
    Arrow,
    Assignment,
    BraceClose,
    BraceOpen,
    Colon,
    ColonColon,
    Comma,
    KwdLet,
    KwdGuard,
    KwdModule,
    KwdYield,
    Identifier,
    Number,
    OpEquality,
    ParenClose,
    ParenOpen,
    Plus,
    SemiColon,
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
}

fn simple_token_type(c: char) -> Option<TokenKind> {
    match c {
        '}' => Some(TokenKind::BraceClose),
        '{' => Some(TokenKind::BraceOpen),
        ',' => Some(TokenKind::Comma),
        ')' => Some(TokenKind::ParenClose),
        '(' => Some(TokenKind::ParenOpen),
        '+' => Some(TokenKind::Plus),
        ';' => Some(TokenKind::SemiColon),
        _ => None
    }
}

fn normalize_token_identifier(id: &str) -> TokenKind {
    match id {
        "guard" => TokenKind::KwdGuard,
        "let" => TokenKind::KwdLet,
        "module" => TokenKind::KwdModule,
        "yield" => TokenKind::KwdYield,
        _ => TokenKind::Identifier,
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
                    'a'..='z' | 'A'..='Z' | '_' | '$' => {
                        state = TokenizerState::MatchingIdentifier {
                            length: 1,
                        }
                    }
                    '0'..='9' => {
                        state = TokenizerState::MatchingNumber {
                            length: 1,
                        }
                    }
                    '=' => {
                        state = TokenizerState::MatchingPartial {
                            fallback: TokenKind::Assignment,
                            terminals: &[
                                ('=', TokenKind::OpEquality),
                                ('>', TokenKind::Arrow),
                            ],
                        }
                    }
                    ':' => {
                        state = TokenizerState::MatchingPartial {
                            fallback: TokenKind::Colon,
                            terminals: &[(':', TokenKind::ColonColon)],
                        }
                    }
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
                        state = TokenizerState::MatchingIdentifier {
                            length: length + 1,
                        }
                    }
                    _ => {
                        break;
                    }
                },
                TokenizerState::MatchingNumber { length } => match c {
                    '0'..='9' | '.' => {
                        state = TokenizerState::MatchingNumber {
                            length: length + 1,
                        }
                    }
                    _ => {
                        break;
                    }
                },
                TokenizerState::MatchingPartial { .. } => {
                    break;
                }
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
            },
            TokenizerState::MatchingNumber { length } => {
                Some(Ok(Token {
                    kind: TokenKind::Number,
                    rest: &rest.as_str(),
                    range: &rest.as_str()[0..length],
                }))
            },
            TokenizerState::MatchingPartial {
                fallback,
                terminals,
            } => {
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
        }
    }
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
