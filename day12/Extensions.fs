module day12.Extensions

module String =
    let split (sep: string) (input: string) = input.Split(sep) |> List.ofArray
    
    let split2 (sep: string) (input: string) =
        input.Split(sep)
        |> List.ofArray
        |> (fun list -> (list.[0], list.[1]))

module List =
    let lengthBy predicate list =
        list |> List.filter predicate |> List.length
        
    let flatMap mapping list =
        list
        |> List.map mapping
        |> List.concat
        
    let groupMap projection mapping list =
        list
        |> List.groupBy projection
        |> List.map (fun (key, values) -> key, List.map mapping values)
