module HtmlParsing

type HtmlParser<'Result> = ParserImpl of (char list -> ('Result option * char list))

let makeParser parserImpl = ParserImpl(parserImpl)

let unwrapParser parser = 
    match parser with
    | ParserImpl parserImpl -> parserImpl

let runParser (maybeParser, html) =
    match maybeParser with
    | Some parser -> (unwrapParser parser) html
    | None -> (None, html)

(* 
    sequencing of parsers: 
        1) try parse given html with given parser;
        2) whenever successful, 
            2.1) apply 'f' to the result in order to get next instance of HtmlParser and
            2.2) return a pair (just created parser, remained html) so that exactly the same operator could be reapplied
*)
let (++) (maybeParser, html) (f:('a -> HtmlParser<'b>)) = 
    match maybeParser with
    | Some parser -> (unwrapParser parser html) |> (fun (parseResult, html') -> ((parseResult |> Option.map f), html'))
    | None -> (None, html)

(* recursivelt repeats parser invokation until if fails *)
let repeat accumulator empty parser = 
    let rec repeat' result html  =
        match unwrapParser parser html with
        | (None, html) -> (Some(result), html)
        | (Some(result'), html') -> repeat' (accumulator result result') html'
    (repeat' empty) 
    |> makeParser
    

(* accumulates a sequence of similar chars *)
let accumulate (parser:HtmlParser<char list>) = repeat (@) [] parser

(* 
    represents a parser that tries to parse just next char based on the predicate;
    chars are converted to string for further convience 
*)


let nextCharParser predicate = 
    makeParser (fun html -> 
        match html with
        | [] -> (None, [])
        | h::t -> if predicate h then (Some([h]), t) else (None, html))

let nextCharConstantParser constant = nextCharParser (fun c -> c = constant)

        

