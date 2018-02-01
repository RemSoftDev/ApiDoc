// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
open HtmlParsing


[<EntryPoint>]
let main argv =
    (* 
        usage example:
            sequencing of "primitive parsers" is applied recursively
            in order to read content of the '<p>' tag;
            as long as content is parsed, parsing considered completed.
    *)
    let fakeHtml = ['<';'p';'>';] @ ['t';'e';'x';'t';] @ ['<';'/';'p';'>';]
    (Some(nextCharConstantParser '<'), fakeHtml)
    ++ (fun openBracket -> (nextCharParser Char.IsLetter) |> accumulate)
    ++ (fun tagName -> nextCharConstantParser '>')
    ++ (fun closeBracket -> (nextCharParser Char.IsLetter) |> accumulate)
    |> runParser
    |> (printfn "%A")
    
    Console.ReadLine() |> ignore

    0 // return an integer exit code
