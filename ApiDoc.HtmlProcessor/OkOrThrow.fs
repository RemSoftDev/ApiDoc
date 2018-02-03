module OkOrThrow

type OkOrThrow<'a, 'b> =
    | Ok of 'a
    | Throw of 'b

let apply o t oot =
    match oot with 
    | Ok value -> o value
    | Throw error -> t error

let map o oot = oot |> apply (fun ok -> Ok(o ok)) (fun throw -> Throw(throw))
   
let rethrow o oot = oot |> apply o (fun throw -> Throw(throw))

let isOk oot = oot |> apply (fun _ -> true) (fun _ -> false)

let merge f g loot root =   
    match (loot, root) with 
    | (Ok lvalue, Ok rvalue) -> Ok(f lvalue rvalue)
    | (Throw lerror, Throw rerror) -> Throw(g lerror rerror)
    | (_, Throw error) -> Throw(error)
    | (Throw error, _) -> Throw(error)
