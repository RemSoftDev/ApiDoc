module Parser
open OkOrThrow

type Parser<'Result> = ParserImpl of (char list -> (OkOrThrow<'Result, string> * char list))

let return_parser initial = 
    (fun symbols -> (Ok(initial), symbols)) 
    |> ParserImpl

let private _get_parser_impl parser = 
    match parser with
    | ParserImpl parserImpl -> parserImpl
let (-->) symbols parser = (_get_parser_impl parser) symbols

let private _chain f parser =
    _get_parser_impl parser
    |> (fun parserImpl -> parserImpl >> 
        (fun (okOrThrow, symbols') -> 
            okOrThrow 
            |> OkOrThrow.apply ((f symbols')) (fun throw -> (Throw(throw), symbols'))))
    |> ParserImpl

let private _bind (f:('a -> Parser<'b>)) = _chain (fun symbols ok -> symbols --> f ok)
let (>>=) parser f = _bind f parser
let (>>==) lparser rparser = lparser >>= (fun _ -> rparser)

let private _transform f = _chain (fun symbols ok -> symbols --> (return_parser (f ok)))
let (|>|) parser f = _transform f parser

let next_char_when f =
    (fun symbols -> 
        match symbols with
        | [] -> (Throw("No char to consume."), [])
        | h::t -> 
            if f h 
            then (Ok(h), t) 
            else (Throw(sprintf "Consumed char '%c' does not meet the predicate's requirement. Predicate: %A." h f), symbols))
    |> ParserImpl

open Shared 
let private _merge_parse_results f = OkOrThrow.merge f (+)
let accumulator f parser = 
    let rec accumulator' result symbols =
        match symbols --> parser with
        | (Ok result', symbols') -> accumulator' (Ok (result') :: result) symbols'
        | (Throw error, symbols') -> 
            if result = []
            then (Throw (error), symbols')
            else 
                result 
                |> List.rev
                |> List.reduce (_merge_parse_results f)
                |> pair symbols'
                |> swap
    (accumulator' []) 
    |> ParserImpl

let any parsers = 
    let rec any' parsers result symbols =
        match parsers with
        | [] -> result
        | h::t -> 
            let (okOrThrow, symbols') = symbols --> h
            if okOrThrow |> OkOrThrow.isOk 
            then (okOrThrow, symbols') 
            else (any' t (okOrThrow, symbols') symbols')
    (any' parsers (Throw("You need at least one parser to run 'any'."), []))
    |> ParserImpl