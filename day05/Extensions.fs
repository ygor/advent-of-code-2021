module day05.Extensions

open System.Text.RegularExpressions

let (..) a b =
    if a < b then seq { a .. b }
    else seq { a .. -1 .. b }

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None
