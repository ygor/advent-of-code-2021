module day04.Extensions

module String =
    let split (sep: string) (value: string) = value.Split(sep) |> List.ofArray

module Seq =
    let any predicate seq =
        seq
        |> Seq.filter predicate
        |> Seq.length
        |> (<) 0