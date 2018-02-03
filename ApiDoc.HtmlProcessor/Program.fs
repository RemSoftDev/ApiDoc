// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
open Shared
open Parser



[<EntryPoint>]
let main argv =        
    
    ['a'..'z'] -->
    (   
        // firstly the parser is constructed with help of proper combinators;
        // only after that parsing itself takes place

        next_char_when Char.IsLetter // defining a primitive single-char-per-step parser
        |>| stringify // Parser<char> -> (char -> string) -> Parser<string>
        |> (accumulator (+))) // giving a primitive parser an ability to accumulate characters into a single string
    |> (printfn "%A")

    Console.ReadLine() |> ignore

    0 // return an integer exit code
