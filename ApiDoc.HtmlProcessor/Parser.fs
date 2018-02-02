module Parser

type Parser<'Result> = ParserImpl of (char list -> ('Result option * char list))

let return_parser initial = 
    (fun symbols -> (Some(initial), symbols)) 
    |> ParserImpl

let private get_parser_impl parser = 
    match parser with
    | ParserImpl parserImpl -> parserImpl
let (-->) symbols parser = (get_parser_impl parser) symbols

let bind (f:('a -> Parser<'b>)) parser = 
    get_parser_impl parser
    |> (fun parserImpl -> parserImpl >> (fun (result, symbols') -> if result |> Option.isSome 
                                                                   then symbols' --> f (Option.get result)
                                                                   else (None, symbols')))
    |> ParserImpl
let (>>=) parser f = bind f parser
let (>>==) lparser rparser = lparser >>= (fun _ -> rparser)

let next_char_parser = 
    (fun symbols -> 
        match symbols with
        | [] -> (None, [])
        | h::t -> (Some(h), t))
    |> ParserImpl



