use crate::prelude::*;

pub use attribute::inner_attribute;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ItemVariant {
    InnerAttribute(Opt<attribute::Attribute>),
    Error,
}

impl ItemVariant {
    /// Checks if an item variant accepts a token
    /// 
    /// # Panics
    /// 
    /// - If the token is not a keyword
    /// - If the keword is not one of KW_EXT, KW_STAT, or KW_ABST
    fn accepts(&self, tok: &Token) -> bool {
        match tok {
            KEYWORD(keyword) =>
                match keyword {
                    Keyword::KW_EXT => false,
                    Keyword::KW_STAT => false,
                    Keyword::KW_ABST => false,
                    _ => panic!("Tried to check if an item variant accepts a non-item-modifier keyword")
                },
            _ => panic!("Tried to check if an item variant accepts a non-keyword modifier")
        }
    }

    /// Gets name of variant for error reporting
    fn name(&self) -> &str {
        match &self {
            ItemVariant::InnerAttribute(_) => "Inner Attribute",
            ItemVariant::Error => "Error"
        }
    }
}


#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Item {
    variant: ItemVariant,
    attributes: Vec<Opt<attribute::Attribute>>,
    visibility: Visibility,

    ext: bool, // external
    stat: bool, // static
    abst: bool, // abstract
}

impl Item {
    pub fn new(
        variant: ItemVariant, attributes: Vec<Opt<attribute::Attribute>>, visibility: Visibility, 
        ext: bool, stat: bool, abst: bool
    ) -> Self {
        Item { variant, attributes, visibility, ext, stat, abst }
    }

    /// Creates a new private [`Item`] with no special flags or attributes
    pub fn simple(variant: ItemVariant) -> Self {
        Self::new(variant, vec![], Visibility::Prv, false, false, false)
    }

    /// Creates a new [`Item`] with a given [`Visibility`], but no other flags or attributes
    pub fn with_visibility(variant: ItemVariant, visibility: Visibility) -> Self {
        Self::new(variant, vec![], visibility, false, false, false)
    }
}

impl From<ItemVariant> for Item {
    fn from(variant: ItemVariant) -> Self {
        Item::simple(variant)
    }
}


pub fn item_variant() -> impl Parser<Token, ItemVariant, Error = Simple<Token>> {
    choice((
        inner_attribute().map(ItemVariant::InnerAttribute),
    ))
}

pub fn item() -> impl Parser<Token, Item, Error = Simple<Token>> {
    inner_attribute()
        .map(|attribute| Item::simple(ItemVariant::InnerAttribute(attribute)))
    .or(attribute::outer_attribute().repeated()
        .then(visibility())
        .then(
            one_of([KW_EXT, KW_STAT, KW_ABST])
                .repeated()
                .validate(|list, span: Span, emit| {
                    for keyword in [KW_EXT, KW_STAT, KW_ABST] {
                        if list.clone().into_iter().filter(|tok| *tok == keyword).count() > 1 {
                            emit(Simple::custom(span.clone(), format!("Cannot declare \"{}\" more than once", keyword.to_string())))
                        }
                    } 
                    list
                })
        )
        .then(item_variant())
        .validate(|(((attributes, visibility), list), variant), span, emit| {
            for keyword in list.clone() {
                if !variant.accepts(&keyword) { 
                    emit(Simple::custom(span.clone(), format!("Cannot declare {} on item {}", keyword.to_string(), variant.name()))) 
                }
            }
            // might as well put it into a single tuple on the way out
            (attributes, visibility, list, variant)
        })
        .map(|(attributes, visibility, modifiers, variant)| {
            Item {
                attributes,
                visibility,
                variant,

                ext: modifiers.contains(&KW_EXT),
                stat: modifiers.contains(&KW_STAT),
                abst: modifiers.contains(&KW_ABST),
            }
        })
    )
}