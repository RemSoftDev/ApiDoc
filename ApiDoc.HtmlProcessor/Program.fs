// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
open Parser

let default_report = Parser.report (fun result -> printfn "%s" "Parsed successfully.") (printfn "Was unable to parse: %A")

[<EntryPoint>]
let main argv =        
  
    ['<';'h'; 'r';'/'; '>';] // no whitespaces
    |> HtmlParser.close_tag_parser 
    |> Parser.run_parser
    |> default_report
    |> ignore
    
    ['<';'h'; 'r';' ';'/'; '>';] // single whitespace
    |> HtmlParser.close_tag_parser 
    |> Parser.run_parser
    |> default_report
    |> ignore
    
    ['<';'h'; 'r';' ';' ';'/'; '>';] // multiple whitespaces whitespace
    |> HtmlParser.close_tag_parser 
    |> Parser.run_parser
    |> default_report
    |> ignore

    Console.ReadLine() |> ignore

    0 // return an integer exit code
