module day06.Extensions

module String =
    let split (sep: string) (input: string) = input.Split(sep) |> List.ofArray
