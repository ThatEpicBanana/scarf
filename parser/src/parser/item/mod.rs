use crate::parser::prelude::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ItemVariant {
    InnerAttribute(S<Opt<attribute::Attribute>>),
    Error,
}

#[parser_util]
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


    pub fn parser() -> S<ItemVariant> {
        choice((
            Attribute::inner_attribute().map(ItemVariant::InnerAttribute),
        )).map_with_span(map_span)
    }
}


#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Item {
    variant: S<ItemVariant>,
    attributes: Vec<S<Opt<Attribute>>>,
    visibility: S<Visibility>,

    ext: bool, // external
    stat: bool, // static
    abst: bool, // abstract
}

#[parser_util(derive_parsable)]
impl Item {
    pub fn new(
        variant: S<ItemVariant>, attributes: Vec<S<Opt<Attribute>>>, visibility: S<Visibility>,
        ext: bool, stat: bool, abst: bool
    ) -> Self {
        Item { variant, attributes, visibility, ext, stat, abst }
    }

    /// Creates a new private [`Item`] with no special flags or attributes
    pub fn simple(variant: S<ItemVariant>) -> Self {
        Self::new(variant, vec![], no_span(Visibility::Prv), false, false, false)
    }

    /// Creates a new [`Item`] with a given [`Visibility`], but no other flags or attributes
    pub fn with_visibility(variant: S<ItemVariant>, visibility: S<Visibility>) -> Self {
        Self::new(variant, vec![], visibility, false, false, false)
    }



    pub fn parser() -> Item {
        Attribute::inner_attribute()
            .map_with_span(|attribute, spn| Item::simple(map_span(ItemVariant::InnerAttribute(attribute), spn)))
        .or(Attribute::outer_attribute().repeated()
            .then(parse!(Visibility))
            .then(
                one_of([KW_EXT, KW_STAT, KW_ABST])
                    .map_with_span(map_span)
                    .repeated()
            )
            .then(parse!(ItemVariant))
                    .validate(|(((attributes, visibility), list), variant), span, emit| {
                        // check each modifier
                        let modifiers = [kw!("ext"), kw!("stat"), kw!("abst")]
                            .map(|kw| {
                                let kw_list: Vec<_> = list.iter().filter(|tok| *tok.unspan_ref() == kw).collect();

                                match kw_list.len() {
                                    0 => false,
                                    x => {
                                        // check if there's a dupe
                                        if x > 1 {
                                            let dupe_list = kw_list.iter().map(|tok| tok.to_owned().to_owned()).collect();
                                            emit(ParserError::from_reason(span.clone(), ParserErrorReason::ItemVariantDuplicatedModifier { dupe_list, modifier: kw.clone() }));
                                        }

                                        // check if the variant accepts the keyword
                                        if variant.accepts(&kw) { true }
                                        else {
                                            emit(ParserError::from_reason(span.clone(), ParserErrorReason::ItemVariantDisallowedModifier { variant: variant.clone(), modifier: kw_list[0].clone() }));
                                            false
                                        }
                                    }
                                }
                            });

                        Item { attributes, visibility, variant, ext: modifiers[0], stat: modifiers[1], abst: modifiers[2] }
                    })
        )
    }
}

impl From<S<ItemVariant>> for Item {
    fn from(variant: S<ItemVariant>) -> Self {
        Item::simple(variant)
    }
}





