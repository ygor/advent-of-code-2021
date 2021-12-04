module day04.Extensions

open System.Text.RegularExpressions

module String =
    let split (sep: string) (input: string) = input.Split(sep) |> List.ofArray
    
    let trim (input: string) =
        input.Trim()
        
module Regex =
    let split (pattern: string) (input: string) =
        Regex.Split(input, pattern) |> List.ofArray

module Seq =
    let any predicate seq =
        seq |> Seq.filter predicate |> Seq.length |> (<) 0
