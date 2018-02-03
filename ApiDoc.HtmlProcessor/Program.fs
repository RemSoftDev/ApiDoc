// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
open Shared
open Parser



[<EntryPoint>]
let main argv =        
    
    ("<div class=\"handsome\" align=\"center\" />".ToCharArray() |> List.ofArray) -->   

    // parse opening
    (next_char_when (fun c -> c = '<')
    >>== (next_char_when Char.IsLetter |>| stringify |> accumulator (+))
    >>== (next_char_when Char.IsWhiteSpace |> accumulator (fun _ __ -> ' '))

    // parse one or more attributes
    >>== (((next_char_when Char.IsLetter |>| stringify |> accumulator (+))
    >>== next_char_when (fun c -> c = '=')
    >>== ([next_char_when (fun c -> c = '"'); next_char_when (fun c -> c = '\'');] |> any)
    >>== (next_char_when Char.IsLetter |>| stringify |> accumulator (+))
    >>== ([next_char_when (fun c -> c = '"'); next_char_when (fun c -> c = '\'');] |> any)
    >>== (next_char_when Char.IsWhiteSpace |> accumulator (fun _ __ -> ' '))) |>| stringify |> accumulator (+))

    // parse closing
    >>== next_char_when (fun c -> c = '/')
    >>== next_char_when (fun c -> c = '>'))
    |> (printfn "%A")

    Console.ReadLine() |> ignore

    0 // return an integer exit code
