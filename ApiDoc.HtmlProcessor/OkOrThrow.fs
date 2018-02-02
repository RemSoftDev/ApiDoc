module OkOrThrow

type OkOrThrow<'a, 'b> =
    | Ok of 'a
    | Throw of 'b

let map o t oot =
    match oot with 
    | Ok value -> o value
    | Throw error -> t error

let rethrow o oot = oot |> map o (fun throw -> Throw(throw))

let isOk oot = oot |> map (fun _ -> true) (fun _ -> false)
