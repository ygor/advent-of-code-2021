module day05.Extensions

module String =
    let split (sep: string) (input: string) = input.Split(sep) |> List.ofArray

module Map =
    let itemOrDefault key defaultValue map = 
        if Map.containsKey key map then map.[key] else defaultValue