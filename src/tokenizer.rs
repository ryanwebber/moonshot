pub struct Token<'a> {
    pub range: &'a str,
}

#[derive(Clone)]
pub struct TokenIter<'a> {
    remaining: core::str::Chars<'a>
}

enum TokenizerState {
    Initial,
    MatchingIdentifier {
        offset: usize,
        length: usize,
    },
}

impl<'a> Iterator for TokenIter<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut state = TokenizerState::Initial;
        let mut iter = self.remaining.clone().enumerate().peekable();
        
        let initial_iter = self.remaining.clone();

        // Consume tokens
        while let Some((i, c)) = iter.peek() {
            match state {
                TokenizerState::Initial => match c {
                    ' ' | '\t' | '\n' | '\r' => {
                        // Ignore
                    }
                    'a'..='z' | 'A'..='Z' | '_' => {
                        state = TokenizerState::MatchingIdentifier {
                            offset: *i,
                            length: 1,
                        }           
                    }
                    _ => {
                        // Error: unexpected token
                        return None;
                    }
                }
                TokenizerState::MatchingIdentifier { offset, length }=> match c {
                    'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => {
                        state = TokenizerState::MatchingIdentifier {
                            offset: offset,
                            length: length + 1,
                        }
                    },
                    _ => {
                        break;
                    }
                }
            }

            _ = iter.next();
            _ = self.remaining.next();
        };

        // Consolidate
        match state {
            TokenizerState::Initial => {
                None
            },
            TokenizerState::MatchingIdentifier { offset, length } => {
                Some(Token {
                    range: &initial_iter.as_str()[offset..offset+length],
                })
            }
        }
    }
}

pub struct Scanner<'a> {
    source: &'a str,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source
        }
    }

    pub fn iter(&self) -> TokenIter<'a> {
        TokenIter {
            remaining: self.source.chars()
        }
    }
}
