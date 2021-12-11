module day10.Extensions

open Microsoft.FSharp.Core

module Seq =
    let partitionMap f (input: Result<'a, 'b> seq) =
        input
        |> Seq.fold (fun ((oks: 'a seq), (errors: 'b seq)) value ->
            match f(value) with
            | Error a -> oks, (Seq.append errors [a])
            | Ok b -> (Seq.append oks [b]), errors) ([], [])