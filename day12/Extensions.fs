module day12.Extensions

module String =
    let split (sep: string) (input: string) = input.Split(sep) |> List.ofArray

module List =
    let lengthBy predicate list =
        list |> List.filter predicate |> List.length