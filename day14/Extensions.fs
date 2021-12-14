module day12.Extensions

module String =
    let split (sep: string) (input: string) =
            input.Split(sep)
            |> List.ofArray

module Map =
    let update key f (map: Map<'a, 'b>) =
        Map.add key (f map.[key]) map