open System.IO
open day12.Extensions

let input = File.ReadAllText("input.txt") |> String.split "\n\n" |> List.ofSeq

let template =
    input.[0]
    |> Seq.map string
    |> Seq.pairwise
    |> Seq.map (fun (a, b) -> a + b)
    |> Seq.countBy id
    |> Seq.map (fun (x, i) -> x, bigint i)
    |> Map.ofSeq
    
let rules =
    input.[1]
    |> String.split "\n"
    |> List.map (String.split " -> " >> (fun l ->
        l.[0], [$"{l.[0].[0]}{l.[1]}"; $"{l.[1]}{l.[0].[1]}"]))
    |> Map.ofList

let step (template: Map<string, bigint>) =
    template
    |> Map.fold (fun polymer pair count ->
        rules.[pair]
        |> List.fold (fun polymer' pair' ->
            let count' = if Map.containsKey pair' polymer' then polymer'.[pair'] + count else count
            Map.add pair' count' polymer') polymer) Map.empty

let score polymer =
    polymer
    |> Map.toList
    |> List.sortByDescending snd
    |> (fun list -> snd list.Head - snd (List.last list))

let result steps =
    [0 .. steps - 1 ]
    |> List.fold (fun polymer _ -> step polymer) template
    |> Map.fold (fun map' pair count ->
        let value = if Map.containsKey pair.[1] map' then map'.[pair.[1]] + count else count
        Map.add pair.[1] value map') Map.empty
    |> Map.update (Seq.head input.[0]) (fun value -> value + bigint 1)
    |> score
    
[<EntryPoint>]
let main _ =
    printfn $"Part 1: %A{result 10}"
    printfn $"Part 2: %A{result 40}"
    0
    