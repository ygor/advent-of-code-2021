module day07.Extensions

module String =
    let split (sep: string) (input: string) = input.Split(sep) |> List.ofArray
