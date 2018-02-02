// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
open Global
open Parser

let default_report = Parser.report (fun result -> printfn "%s" "Parsed successfully.") (printfn "Was unable to parse: %A")

[<EntryPoint>]
let main argv =        
  
    ['<';'h'; 'r';'/'; '>';]
    |> HtmlParser.open_tag_parser 
    |> Parser.run_parser
    |> default_report
    |> ignore

    Console.ReadLine() |> ignore

    0 // return an integer exit code
