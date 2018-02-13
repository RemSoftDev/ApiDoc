module OkOrThrow

type OkOrThrow<'a, 'b> =
    | Ok of 'a
    | Throw of 'b

let apply o t oot =
    match oot with 
    | Ok value -> o value
    | Throw error -> t error

let ok p1 p2 =  Ok(p1 p2)
let throw p =  Throw(p)

let map o oot = oot |> apply (ok o) throw
   
let rethrow o oot = oot |> apply o throw

let isOk oot = oot |> apply (fun _ -> true) (fun _ -> false)

let merge f g loot root =   
    match (loot, root) with 
    | (Ok lvalue, Ok rvalue) -> Ok(f lvalue rvalue)
    | (Throw lerror, Throw rerror) -> Throw(g lerror rerror)
    | (_, Throw error) -> Throw(error)
    | (Throw error, _) -> Throw(error)
