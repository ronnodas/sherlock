use anyhow::{Context as _, Ok, Result, bail};
use itertools::Itertools as _;
use select::node::Node;
use select::predicate::Predicate as _;

use crate::models::{Card, CardBack, Judgment, MaybeHint};
use crate::solver::grid::html::{Class, ClassName, Div, H3, NodeExt as _, Paragraph};

pub(crate) fn parse_card(node: &Node<'_>) -> Result<(Card, bool)> {
    let node = node
        .expect(Div)?
        .unique_child(Div.and(Class(ClassName::Card)))?;
    let status = if node.is(Class(ClassName::Flipped)) {
        if node.is(Class(ClassName::Innocent)) {
            Some(Judgment::Innocent)
        } else if node.is(Class(ClassName::Criminal)) {
            Some(Judgment::Criminal)
        } else {
            bail!("expecting either `.innocent` or `.criminal`")
        }
    } else {
        None
    };
    // TODO validate coord
    let [card] = if status.is_some() {
        node.expect_children(Div.and(Class(ClassName::CardBack)))
    } else {
        node.expect_children(Div.and(Class(ClassName::CardFront)))
    }
    .context("inside a `.card`")?;
    let name = parse_name(card)?;
    let profession = parse_profession(card)?;
    let back = status
        .map(|judgment| parse_back(card, judgment))
        .transpose()?;
    let has_hint = node.is(Class(ClassName::HasHint));
    let card = Card::new(name, profession, back);
    Ok((card, has_hint))
}

fn parse_back(card: Node<'_>, judgment: Judgment) -> Result<CardBack> {
    let card = card
        .expect(Class(judgment.into()))
        .context("`.card-back` should be consistent with `.card`")?;

    let hint = MaybeHint::Logical(parse_hint(card)?);
    Ok(CardBack::new(judgment, hint))
}

fn parse_hint(card: Node<'_>) -> Result<String> {
    let hint = card
        .unique_child(Paragraph.and(Class(ClassName::Hint)))
        .with_context(|| format!("`.card-back` should have a unique `p .hint`: {card:?}"))?;
    Ok(hint.text().trim().to_owned())
}

fn parse_profession(card: Node<'_>) -> Result<String> {
    let profession = card
        .unique_child(Paragraph.and(Class(ClassName::Profession)))
        .with_context(|| {
            format!("`.card-{{back,front}}` should have a unique `p .profession`: {card:?}")
        })?;
    Ok(profession.text().trim().to_owned())
}

fn parse_name(card: Node<'_>) -> Result<String> {
    let [name] = card
        .expect_children(Div.and(Class(ClassName::Name)))
        .context("`.card-{back,front}` should have a unique `div .name`")?;
    let name = name
        .unique_child(H3.and(Class(ClassName::Name)))
        .context("`div .name` should have a unique `h3 .name`")?
        .text();
    // emulating `text-transform: capitalize`
    Ok(name
        .trim()
        .chars()
        .with_position()
        .map(|(position, c)| {
            if position.is_first {
                c.to_ascii_uppercase()
            } else {
                c
            }
        })
        .collect())
}
