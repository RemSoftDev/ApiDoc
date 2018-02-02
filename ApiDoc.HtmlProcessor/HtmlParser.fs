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
    make_parser (fun html -> 
        match html with
        | [] -> (None, [])
        | h::t -> if predicate h then (Some([h]), t) else (None, html))

(* 
    represents a parser that tries to parse just next char, provided as an argument
*)
let next_char_constant_parser constant = next_char_parser (fun c -> c = constant)


let letters_accumulator = (next_char_parser Char.IsLetter) |> accumulate_while_success
let open_tag_parser = next_char_constant_parser '<'

