// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
open Global
open Parser


[<EntryPoint>]
let main argv =

    (* 
        usage example:
            sequencing of "primitive parsers" is applied recursively
            in order to read content of the '<p>' tag;
            as long as content is parsed, parsing considered completed.
    *)        
    
    (['<';'p';'>';] @ ['t';'e';'x';'t';] @ ['<';'/';'p';'>';] 
    |> (pair (Some(HtmlParser.next_char_constant_parser '<'))))
    ++ (fun openBracket -> (HtmlParser.next_char_parser Char.IsLetter) |> HtmlParser.accumulate_while_success)
    ++ (fun tagName -> HtmlParser.next_char_constant_parser '>')
    ++ (fun closeBracket -> (HtmlParser.next_char_parser Char.IsLetter) |> HtmlParser.accumulate_while_success)
    |> Parser.run_parser
    |> (printfn "%A") 


    Console.ReadLine() |> ignore

    0 // return an integer exit code
