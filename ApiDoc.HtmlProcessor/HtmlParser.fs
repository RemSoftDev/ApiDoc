module HtmlParser

open System
open Parser

let accumulate_while_success (parser:Parser<char list>) = accumulate_while_success (@) [] parser

let accumulate_exactly times (parser:Parser<char list>) = accumulate_exactly (@) [] times parser

(* 
    represents a parser that tries to parse just next char based on the predicate;
    chars are converted to string for further convience 
*)
let next_char_parser predicate = 
    (fun html -> 
        match html with
        | [] -> 
            (None, [])
        | h::t -> 
            if predicate h then (Some([h]), t) else (None, html))
    |> make_parser 

(* 
    represents a parser that tries to parse just next char, provided as an argument
*)
let next_char_constant_parser constant = next_char_parser (fun c -> c = constant)

let whitespace_accumulator = next_char_parser Char.IsWhiteSpace |> accumulate_while_success
let letters_accumulator = next_char_parser Char.IsLetter |> accumulate_while_success
let forward_slash_parser = next_char_constant_parser '/' |> accumulate_exactly 1
let left_bracket_parser = next_char_constant_parser '<' |> accumulate_exactly 1
let right_bracket_parser = next_char_constant_parser '>' |> accumulate_exactly 1
let open_tag_parser html = 
    (Some(left_bracket_parser), html)
    ++ (fun _ -> letters_accumulator)
    ++ (fun _ -> whitespace_accumulator <||> forward_slash_parser)
    ++ (fun _ -> forward_slash_parser <||> (unit_parser ['/']))
    ++ (fun _ -> right_bracket_parser)
