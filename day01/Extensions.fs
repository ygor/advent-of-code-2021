module Extensions

module Seq =
    let lengthBy filter list =
        list
        |> Seq.filter filter
        |> Seq.length
