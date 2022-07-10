use std::fmt;
use std::collections::HashSet;
use chumsky::error::Error;

use crate::parser::prelude::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ParserErrorReason {
    Unexpected,
    Unclosed { span: Span, delimiter: Token },
    PatternListSameType(Vec<Box<S<SinglePattern>>>),
    ItemVariantDuplicatedModifier { dupe_list:  Vec<S<Token>>, modifier:   Token  },
    ItemVariantDisallowedModifier {   variant: S<ItemVariant>, modifier: S<Token> },
    AbsolutePathDisallowedRoot { path: S<Path> },
}

impl ParserErrorReason {
    pub fn simple_response(&self) -> String {
        use ParserErrorReason::*;

        match &self {
            Unexpected => "unexpected token".to_string(),
            Unclosed { delimiter, .. } => format!("unclosed {}", delimiter),
            PatternListSameType(_) => "pattern lists must be fully consisted of the same type of pattern".to_string(),
            ItemVariantDuplicatedModifier { modifier, .. } => format!("{} cannot be defined more than once", modifier),
            ItemVariantDisallowedModifier { variant: _, modifier } => format!("{} cannot be defined on {}", modifier.unspan_ref(), todo!()), // TODO: add a function for variants to get their name
            AbsolutePathDisallowedRoot { path } => format!("cannot have {} as the root of an absolute path", path.root.unspan_ref()),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ParserError {
    pub span: Span,
    pub reason: ParserErrorReason,
    pub expected: HashSet<Option<Token>>,
    pub found: Option<Token>,
    pub label: Option<&'static str>,
}

impl ParserError {
    /// Creates a [`ParserError`] from a span and reason (with no expected or found tokens)
    pub fn from_reason(span: Span, reason: ParserErrorReason) -> Self {
        Self { span, reason, expected: HashSet::default(), found: None, label: None }
    }
}

impl Error<Token> for ParserError {
    type Span = std::ops::Range<usize>;
    type Label = &'static str;

    fn expected_input_found<Iter: IntoIterator<Item = Option<Token>>>(
        span: Self::Span,
        expected: Iter,
        found: Option<Token>,
    ) -> Self {
        Self { label: None,
            span,
            reason: ParserErrorReason::Unexpected,
            expected: expected.into_iter().collect(),
            found,
        }
    }

    fn unclosed_delimiter(
        unclosed_span: Self::Span,
        delimiter: Token,
        span: Self::Span,
        expected: Token,
        found: Option<Token>,
    ) -> Self {
        Self { label: None,
            span,
            reason: ParserErrorReason::Unclosed {
                span: unclosed_span,
                delimiter,
            },
            expected: std::iter::once(Some(expected)).collect(),
            found,
        }
    }

    fn with_label(mut self, label: Self::Label) -> Self {
        self.label.get_or_insert(label);
        self
    }

    fn merge(mut self, other: Self) -> Self {
        // prioritize unclosed delimiter errors, and then own reason
        self.reason = match (&self.reason, &other.reason) {
            (ParserErrorReason::Unclosed { .. }, _) => self.reason,
            (_, ParserErrorReason::Unclosed { .. }) => other.reason,
            _ => self.reason,
        };

        // merge expected
        for expected in other.expected {
            self.expected.insert(expected);
        }

        self
    }
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.reason {
            // if it's a "normal" reason then display the found and expected stuff
            ParserErrorReason::Unexpected | ParserErrorReason::Unclosed { .. } => {
                if let Some(found) = &self.found {
                    write!(f, "found '{}'", found)?;
                } else {
                    write!(f, "found end of input")?;
                }

                match self.expected.len() {
                    0 => {}
                    1 => write!(f,
                        " but {} was expected",
                        match self.expected.iter().next().unwrap() {
                            Some(x) => format!("{}", x),
                            None => format!("end of input"),
                        },
                    )?,
                    _ => write!(f,
                        " but one of {} was expected",
                        self.expected.iter()
                            .map(|expected| match expected {
                                Some(x) => format!("{}", x),
                                None => format!("end of input"),
                            }).collect::<Vec<_>>()
                            .join(", ")
                    )?,
                }
            },
            // otherwise use the reason's response
            _ => write!(f, "{}", self.reason.simple_response())?,
        }

        fmt::Result::Ok(())
    }
}
