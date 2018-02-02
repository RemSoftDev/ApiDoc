// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
open Global
open Parser


[<EntryPoint>]
let main argv =

    let any_combiner_test = 
        [HtmlParser.open_tag_parser; HtmlParser.letters_accumulator;] 
        |> any
        |> Parser.accumulate_while_success (@) []

    (Some(any_combiner_test), ['<';'h'; 'r';'/'; '>';])
    |> Parser.run_parser
    |> (printfn "%A")


    Console.ReadLine() |> ignore

    0 // return an integer exit code
