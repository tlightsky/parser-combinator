// https://bodil.lol/parser-combinators/
// Parsing is a process of deriving structure from a stream of data.
// A parser is something which teases out that structure.
#![type_length_limit="181933244"]

#[derive(Clone, Debug, PartialEq, Eq)]
struct Element {
    name: String,
    attributes: Vec<(String, String)>,
    children: Vec<Element>,
}

type ParseResult<'a, Output> = Result<(&'a str, Output), &'a str>;

trait Parser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output>;

    fn map<F, NewOutput>(self, map_fn: F) -> BoxedParser<'a, NewOutput>
    where
        Self: Sized + 'a,
        Output: 'a,
        NewOutput: 'a,
        F: Fn(Output) -> NewOutput + 'a,
    {
        BoxedParser::new(map(self, map_fn))
    }

    fn pred<F>(self, predicator: F) -> BoxedParser<'a, Output>
    where
        Self: Sized + 'a,
        Output: 'a,
        F: Fn(&Output) -> bool + 'a,
    {
        BoxedParser::new(pred(self, predicator))
    }
}

impl<'a, F, Output> Parser<'a, Output> for F
where
    F: Fn(&'a str) -> ParseResult<Output>,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self(input)
    }
}

// fn the_letter_a(input: &str) -> Result<(&str, ()), &str> {
//     match input.chars().next() {
//         Some('a') => Ok((&input['a'.len_utf8()..], ())),
//         _ => Err(input),
//     }
// }

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
    move |mut input| {
        let mut result = Vec::new();

        if let Ok((next_input, first_item)) = parser.parse(input) {
            input = next_input;
            result.push(first_item);
        } else {
            return Err(input);
        }

        while let Ok((next_input, next_item)) = parser.parse(input) {
            input = next_input;
            result.push(next_item);
        }

        Ok((input, result))
    }
}

// fn one_or_more<'a, P, A>(parser: Rc<P>) -> impl Parser<'a, Vec<A>>
// where
//     P: Parser<'a, A>,
// {
//     map(pair(parser.borrow(),
//                 zero_or_more(parser.borrow())),
//         |(head, mut tail)| {
//         tail.insert(0, head);
//         tail
//     })
// }

fn zero_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>
{
    move |mut input| {
        let mut result = Vec::new();
        while let Ok((input1, r1)) = parser.parse(input) {
            input = input1;
            result.push(r1);
        }
        Ok((input, result))
    }
}

fn any_char(input: &str) -> ParseResult<char> {
    match input.chars().next() {
        Some(c) => Ok((&input[c.len_utf8()..], c)),
        _ => Err(input),
    }
}

fn pred<'a, R, P, F>(parser: P, predicator: F) -> impl Parser<'a, R>
where
    P: Parser<'a, R>,
    F: Fn(&R) -> bool,
{
    move |input| {
        match parser.parse(input) {
            Ok((input1, next)) if(predicator(&next)) => Ok((input1, next)),
            _ => Err(input),
        }
    }
}

fn whitespace_char<'a>() -> impl Parser<'a, char> {
    pred(any_char, |c| c.is_whitespace())
}

fn space1<'a>() -> impl Parser<'a, Vec<char>> {
    one_or_more(whitespace_char())
}

// fn space0<'a>() -> impl Parser<'a, Vec<char>> {
//     zero_or_more(whitespace_char())
// }

fn quoted_string<'a>() -> impl Parser<'a, String> {
    right(
        match_literal("\""),
        left(
            zero_or_more(any_char.pred(|c| *c!='\"')),
            match_literal("\""),
        )
    ).map(|chars| chars.into_iter().collect())
}

fn attribute_pair<'a>() -> impl Parser<'a, (String, String)> {
    pair(identifier, right(match_literal("="),quoted_string()))
}

fn attributes<'a>() -> impl Parser<'a, Vec<(String, String)>> {
    zero_or_more(right(space1(), attribute_pair()))
}

fn element_start<'a>() -> impl Parser<'a, (String, Vec<(String, String)>)> {
    right(match_literal("<"), pair(identifier, attributes()))
}

fn single_element<'a>() -> impl Parser<'a, Element> {
        left(element_start(), match_literal("/>")).map(
    |(name, attributes)| Element {
                name,
                attributes,
                children: vec![]
            }
        )
}

struct BoxedParser<'a, Output> {
    parser: Box<dyn Parser<'a, Output> + 'a>
}

impl<'a, Output> BoxedParser<'a, Output> {
    fn new<P>(parser: P) -> Self
    where
        P: Parser<'a, Output> + 'a
    {
        BoxedParser {
            parser: Box::new(parser),
        }
    }
}

impl<'a, Output> Parser<'a, Output> for BoxedParser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self.parser.parse(input)
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
    #[test]
    fn one_or_more_combinator() {
        let parser = one_or_more(match_literal("ha"));
        assert_eq!(parser.parse("hahaha"), Ok(("", vec![(), (), ()])));
        assert_eq!(parser.parse("ahah"), Err("ahah"));
        assert_eq!(parser.parse(""), Err(""));
    }
    #[test]
    fn zero_or_more_combinator() {
        let parser = zero_or_more(match_literal("ha"));
        assert_eq!(parser.parse("hahaha"), Ok(("", vec![(), (), ()])));
        assert_eq!(parser.parse("ahah"), Ok(("ahah", vec![])));
        assert_eq!(parser.parse(""), Ok(("", vec![])));
    }
    #[test]
    fn predicate_combinator() {
        let parser = pred(any_char, |c| *c == 'o' );
        assert_eq!(parser.parse("omg"), Ok(("mg", 'o')));
        assert_eq!(parser.parse("lol"), Err("lol"));
    }
    #[test]
    fn quoted_string_parser() {
        assert_eq!(
            quoted_string().parse("\"Hello Joe\""),
            Ok(("", "Hello Joe".to_string())),
        );
    }

    #[test]
    fn attribute_parser0() {
        assert_eq!(
            attributes().parse(" one=\"1\""),
            Ok(("", vec![("one".to_string(), "1".to_string())]))
        );
    }

    #[test]
    fn attribute_parser1() {
        assert_eq!(
            attributes().parse(" one=\"1\" two=\"2\" three=\"3\""),
            Ok(("",
                vec![("one".to_string(), "1".to_string()),
                    ("two".to_string(), "2".to_string()),
                    ("three".to_string(), "3".to_string())])),
        );
    }

    #[test]
    fn single_element_parser() {
        assert_eq!(
            single_element().parse("<div class=\"float\"/>"),
            Ok(("",
                Element {
                    name: "div".to_string(),
                    attributes: vec![("class".to_string(), "float".to_string())],
                    children: vec![],
                }
            )),
        );
    }
}
