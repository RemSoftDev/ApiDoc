// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
open Parser



[<EntryPoint>]
let main argv =        
    
    ['a'..'z'] -->
    (next_char_parser
    >>== next_char_parser
    >>== next_char_parser
    >>== next_char_parser)
    |> (printfn "%A")

    Console.ReadLine() |> ignore

    0 // return an integer exit code
