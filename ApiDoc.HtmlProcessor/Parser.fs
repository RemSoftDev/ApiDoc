module Parser

type Parser<'Result> = ParserImpl of (char list -> ('Result option * char list))

let make_parser parserImpl = ParserImpl(parserImpl)

let unwrap_parser parser = 
    match parser with
    | ParserImpl parserImpl -> parserImpl

let run_parser (maybeParser, html) =
    match maybeParser with
    | Some parser -> (unwrap_parser parser) html
    | None -> (None, html)

(* 
    sequencing of parsers: 
        1) try parse given html with given parser;
        2) whenever successful, 
            2.1) apply 'f' to the result in order to get next instance of Parser and
            2.2) return a pair (just created parser, remained html) so that exactly the same operator could be reapplied
*)
let (++) (maybeParser, html) (f:('a -> Parser<'b>)) = 
    match maybeParser with
    | Some parser -> (unwrap_parser parser html) |> (fun (parseResult, html') -> ((parseResult |> Option.map f), html'))
    | None -> (None, html)

(* recursivelt repeats parser invokation until if fails *)
let accumulate_while_success accumulator empty parser = 
    let rec accumulate_while_success' result html  =
        match unwrap_parser parser html with
        | (None, html) -> (Some(result), html)
        | (Some(result'), html') -> accumulate_while_success' (accumulator result result') html'
    (accumulate_while_success' empty) 
    |> make_parser

(* 
    expects given parser to succeed exact number of times,
    in any other case the result considered to be failure
*)   
let accumulate_exactly accumulator empty times parser = 
    let rec accumulate_exactly' times result html  =
        match times with
        | 0 -> 
            match unwrap_parser parser html with
            | (None, html) -> (Some(result), html)
            | (Some(result'), html') -> (None, html)
        | times' -> 
            match unwrap_parser parser html with
            | (None, html) -> (None, html)
            | (Some(result'), html') -> accumulate_exactly' (times' - 1) (accumulator result result') html'
            
    (accumulate_exactly' times empty) 
    |> make_parser

        

