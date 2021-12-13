module day12.Extensions

module String =    
    let split (sep: string) (input: string) =
        input.Split(sep)
        |> List.ofArray

    let substring start (string: string) = string.Substring(start)