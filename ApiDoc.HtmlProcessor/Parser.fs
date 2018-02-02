module Parser

type Parser<'Result> = ParserImpl of (char list -> ('Result option * char list))

let return_parser initial = (fun symbols -> (Some(initial), symbols)) |> ParserImpl
let broken_parser<'a> = (fun symbols -> (Option<'a>.None, symbols)) |> ParserImpl

let run_parser parser = 
    match parser with
    | ParserImpl parserImpl -> parserImpl
let (-->) parser symbols = (run_parser parser) symbols

let bind (f:('a -> Parser<'b>)) parser = 
    run_parser parser
    >> (fun (result, symbols') -> if result |> Option.isSome 
                                  then f (Option.get result) --> symbols'
                                  else (None, symbols'))
    |> ParserImpl
let (>>=) parser f  = bind f parser



