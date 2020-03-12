// https://bodil.lol/parser-combinators/
// Parsing is a process of deriving structure from a stream of data.
// A parser is something which teases out that structure.

#[derive(Clone, Debug, PartialEq, Eq)]
struct Element {
    name: String,
    attributes: Vec<(String, String)>,
    children: Vec<Element>,
}

fn the_letter_a(input: &str) -> Result<(&str, ()), &str> {
    match input.chars().next() {
        Some('a') => Ok((&input['a'.len_utf8()..], ())),
        _ => Err(input),
    }
}

fn match_literal(expected: &'static str)
    -> impl Fn(&str) -> Result<(&str, ()), &str> {
    move |input| match input.get(0..expected.len()) {
        Some(next) if next == expected => {
            Ok((&input[expected.len()..], ()))
        },
        _ => Err(input),
    }
}

fn identifier(input: &str) -> Result<(&str, String), &str> {
    let mut matched = String::new();
    let mut chars = input.chars();
    match chars.next() {
        Some(next) if next.is_alphabetic() => matched.push(next),
        _ => return Err(input),
    }

    while let Some(next) = chars.next() {
        if next.is_alphanumeric() || next =='-' {
            matched.push(next)
        } else {
            break
        }
    }

    Ok((&input[matched.len()..], matched))
}

fn pair<P1, P2, R1, R2>(parser1: P1, parser2: P2)
    -> impl Fn(&str) -> Result<(&str, (R1, R2)), &str>
    where
        P1: Fn(&str) -> Result<(&str, R1), &str>,
        P2: Fn(&str) -> Result<((&str, R2)), &str>,
{
    move |input| match parser1(input) {
        Ok((input1, R1)) => match parser2(input1) {
            Ok((input2, R2)) => Ok((input2, (R1, R2))),
            Err(err) => Err(err),
        },
        Err(err) => Err(input),
    }
}

#[cfg(test)]
mod tests {
    use crate::{match_literal, identifier, pair};

    #[test]
    fn literal_parser() {
        let parse_joe = match_literal("Hello Joe!");
        assert_eq!(
            parse_joe("Hello Joe!"),
            Ok(("", ())),
        );
        assert_eq!(
            parse_joe("Hello Joe!Hello Robert!"),
            Ok(("Hello Robert!", ())),
        );
        assert_eq!(
            parse_joe("Hello Mike!"),
            Err("Hello Mike!"),
        );
    }

    #[test]
    fn identifier_parser() {
        assert_eq!(
            identifier("a10 b20"),
            Ok((" b20", "a10".to_string())),
        );
        assert_eq!(
            identifier("i-am-an-identifier"),
            Ok(("", "i-am-an-identifier".to_string())),
        );
        assert_eq!(
            identifier("not entirely an identifier"),
            Ok((" entirely an identifier", "not".to_string())),
        );
        assert_eq!(
            identifier("!not at all an identifier"),
            Err("!not at all an identifier"),
        );
    }

    #[test]
    fn pair_combinator() {
        let tag_opener = pair(match_literal("<"), identifier);
        assert_eq!(
            tag_opener("<my-first-element/>"),
            Ok(("/>", ((), "my-first-element".to_string()))),
        );
        assert_eq!(tag_opener("oops"), Err("oops"));
        assert_eq!(tag_opener("<!oops"), Err("!oops"));
    }
}
