module day11.Extensions

module List =
    let foldn n folder state =
        [0 .. (n - 1)]
        |> List.fold folder state        
