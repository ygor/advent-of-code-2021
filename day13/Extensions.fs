module day12.Extensions

module String =    
    let split (sep: string) (input: string) =
        input.Split(sep)
        |> List.ofArray

    let substring start (string: string) = string.Substring(start)
        
module List =
    let flatMap mapping list =
        list
        |> List.map mapping
        |> List.concat
        
    let groupMap projection mapping list =
        list
        |> List.groupBy projection
        |> List.map (fun (key, values) -> key, List.map mapping values)
