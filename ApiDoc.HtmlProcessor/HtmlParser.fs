module HtmlParser

open System
open Shared
open Parser

type HtmlParserResult<'a> = ParseResult of ('a * char list)

let private _open_bracket_parser = next_char_when (fun c -> c = '<')
let private _close_bracket_parser = next_char_when (fun c -> c = '>')
let private _forward_slash_parser = next_char_when (fun c -> c = '/')
let private _equals_sign_parser = next_char_when (fun c -> c = '=')
let private _quote_sing_parser =  next_char_when (fun c -> c = '\'')
let private _double_quote_sign_parser = next_char_when (fun c -> c = '"')

let private _either_quote_sign_parser = [_quote_sing_parser; _double_quote_sign_parser;] |> any
let private _self_standing_close_tag_parser = _forward_slash_parser >>== _close_bracket_parser
let private _close_tag_parser = _open_bracket_parser >>== _forward_slash_parser

let private _letters_accumulator = next_char_when Char.IsLetter |>| stringify |> accumulator (+)
let private _whitespaces_accumulator = next_char_when Char.IsWhiteSpace |> accumulator (fun _ __ -> ' ')

let private _tag_opening_parser = 
    _open_bracket_parser
    >>== _letters_accumulator

// accumulates attributes into a single string
let private _attributes_parser = 
    (_whitespaces_accumulator
    >>== _letters_accumulator 
    >>== _equals_sign_parser
    >>== _either_quote_sign_parser
    >>== _letters_accumulator
    >>== _either_quote_sign_parser)
    |>| stringify
    |> accumulator (+)

// parsers all kinds of self-standing attribute
let self_standing_tag_parser = 
    _tag_opening_parser
    >>== ([_attributes_parser; (return_parser " ");] |> any)
    >>== _self_standing_close_tag_parser

let opening_tag_parser = 
    _tag_opening_parser
    >>== ([_attributes_parser; (return_parser " ");] |> any)
    >>== _close_bracket_parser

let closing_tag_parser = 
    _close_tag_parser
    >>== ([_whitespaces_accumulator; (return_parser ' ');] |> any)
    >>== _letters_accumulator
    >>== _close_bracket_parser


