module day12.Extensions

module String =
    let split (sep: string) (input: string) =
            input.Split(sep)
            |> List.ofArray
    
module List =    
    let flatMap mapping list =
        list |> List.map mapping |> List.concat    