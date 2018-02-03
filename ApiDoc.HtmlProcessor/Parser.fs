module Parser
open OkOrThrow

type Parser<'Result> = ParserImpl of (char list -> (OkOrThrow<'Result, string> * char list))

let return_parser initial = 
    (fun symbols -> (Ok(initial), symbols)) 
    |> ParserImpl

let broken_parser<'a> = 
    (fun symbols -> (OkOrThrow<'a, string>.Throw("This parser never ever succees..."), symbols))
    |> ParserImpl

let private get_parser_impl parser = 
    match parser with
    | ParserImpl parserImpl -> parserImpl
let (-->) symbols parser = (get_parser_impl parser) symbols

let bind (f:('a -> Parser<'b>)) parser = 
    get_parser_impl parser
    |> (fun parserImpl -> parserImpl >> 
        (fun (okOrThrow, symbols') -> 
            okOrThrow 
            |> OkOrThrow.bind (fun ok -> symbols' --> f ok) (fun throw -> (Throw(throw), symbols'))))
    |> ParserImpl
let (>>=) parser f = bind f parser
let (>>==) lparser rparser = lparser >>= (fun _ -> rparser)

let transform f parser =
    get_parser_impl parser
    |> (fun parserImpl -> parserImpl >> 
        (fun (okOrThrow, symbols') -> 
            okOrThrow 
            |> OkOrThrow.bind (fun ok -> symbols' --> (return_parser (f ok))) (fun throw -> (Throw(throw), symbols'))))
    |> ParserImpl
let (|>|) parser f = transform f parser

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
let merge_parse_results f = OkOrThrow.merge f (+)
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
                |> List.reduce (merge_parse_results f)
                |> pair symbols'
                |> swap
    (accumulator' []) 
    |> ParserImpl


