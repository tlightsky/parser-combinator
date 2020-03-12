// https://bodil.lol/parser-combinators/
// Parsing is a process of deriving structure from a stream of data.
// A parser is something which teases out that structure.

#[derive(Clone, Debug, PartialEq, Eq)]
struct Element {
    name: String,
    attributes: Vec<(String, String)>,
    children: Vec<Element>,
}

type ParseResult<'a, Output> = Result<(&'a str, Output), &'a str>;

trait Parser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output>;
}

impl<'a, F, Output> Parser<'a, Output> for F
where
    F: Fn(&'a str) -> ParseResult<Output>,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self(input)
    }
}

fn the_letter_a(input: &str) -> Result<(&str, ()), &str> {
    match input.chars().next() {
        Some('a') => Ok((&input['a'.len_utf8()..], ())),
        _ => Err(input),
    }
}

fn match_literal<'a>(expected: &'static str) -> impl Parser<'a, ()> {
    move |input: &'a str| match input.get(0..expected.len()) {
        Some(next) if next == expected => Ok((&input[expected.len()..], ())),
        _ => Err(input),
    }
}

fn identifier(input: &str) -> ParseResult<String>{
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

fn pair<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2)
   -> impl Parser<'a, (R1, R2)>
   where
       P1: Parser<'a, R1>,
       P2: Parser<'a, R2>,
{
   move |input|
       parser1.parse(input).and_then(|(input1, result1)| {
           parser2.parse(input1)
               .map(|(input2, result2)| (input2, (result1, result2))
           )
       })
}

fn left<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R1>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(left, _)| left)
}

fn right<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R2>
    where
        P1: Parser<'a, R1>,
        P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(_, right)| right)
}

fn map<'a, P, F, A, B>(parser: P, map_fn: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    F: Fn(A) -> B,
{
    move |input|
        parser.parse(input)
            .map(|(next_input, result)| (next_input, map_fn(result)))
}

fn one_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    move |mut input: &'a str| {
        let mut result = Vec::new();
        match parser.parse(input) {
            Ok((input1, r1)) => {
                input = input1;
                result.push(r1);
            },
            Err(err) => return Err(err),
        }
        while let Ok((input1, r1)) = parser.parse(input) {
            input = input1;
            result.push(r1);
        }
        Ok((input, result))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn literal_parser() {
        let parse_joe = match_literal("Hello Joe!");
        assert_eq!(
            parse_joe.parse("Hello Joe!"),
            Ok(("", ())),
        );
        assert_eq!(
            parse_joe.parse("Hello Joe!Hello Robert!"),
            Ok(("Hello Robert!", ())),
        );
        assert_eq!(
            parse_joe.parse("Hello Mike!"),
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
            tag_opener.parse("<my-first-element/>"),
            Ok(("/>", ((), "my-first-element".to_string()))),
        );
        assert_eq!(tag_opener.parse("oops"), Err("oops"));
        assert_eq!(tag_opener.parse("<!oops"), Err("!oops"));
    }
    #[test]
    fn right_combinator() {
        let tag_opener = right(match_literal("<"), identifier);
        assert_eq!(
            Ok(("/>", "my-first-element".to_string())),
            tag_opener.parse("<my-first-element/>"),
        );
        assert_eq!(Err("oops"), tag_opener.parse("oops"));
        assert_eq!(Err("!oops"), tag_opener.parse("<!oops"));
    }
}
