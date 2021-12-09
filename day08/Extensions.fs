module day08.Extensions

module String =
    let split (sep: string) (input: string) = input.Split(sep) |> List.ofArray

module Map =
    let any predicate map =
        map
        |> Map.filter predicate
        |> Map.count
        |> (<) 0
        