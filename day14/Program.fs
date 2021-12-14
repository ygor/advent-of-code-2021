open System.IO
open day12.Extensions

let input = File.ReadAllText("input.txt") |> String.split "\n\n" |> List.ofSeq

let template = input.[0] |> Seq.map string |> List.ofSeq
let rules =
    input.[1]
    |> String.split "\n"
    |> List.map (String.split " -> " >> (fun l -> l.[0], l.[1]))
    |> Map.ofList

let step template =
    template
    |> List.pairwise
    |> List.indexed
    |> List.fold (fun (polymer: string list) (index, pair) ->
        let value = rules.[$"{fst pair}{snd pair}"]
        let index' = polymer.Length - template.Length + index + 1
        List.insertAt index' value polymer) template

let part1 =
    [0 .. 39 ]
    |> List.fold (fun polymer _ -> step polymer) template
    |> List.countBy id
    |> List.sortByDescending snd
    |> (fun list -> snd list.Head - snd (List.last list))

[<EntryPoint>]
let main _ =
    printfn $"Part 1: %A{part1}"
    0
    