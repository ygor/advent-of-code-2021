module day12.Extensions

module String =
    let split (sep: string) (input: string) =
            input.Split(sep)
            |> List.ofArray
     
module List =
    let foldn n folder state =
        [0 .. n - 1]
        |> List.fold folder state