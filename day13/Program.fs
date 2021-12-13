open System.IO
open day12.Extensions

let input =
    File.ReadAllText("input.txt")
    |> String.split "\n\n"
    |> List.ofSeq

let map =
    input.[0]
    |> String.split "\n"
    |> List.map (String.split "," >> List.map int >> (fun l -> l.[0], l.[1]))

let folds =
    input.[1]
    |> String.split "\n"
    |> List.map (String.substring 11 >> String.split "=" >> (fun l -> l.[0], int l.[1]))

let fold (axis, value) map =
    map
    |> List.map (fun (x, y) ->
        if axis = "y" then x, (if y > value then y - 2 * (y - value) else y)
        else (if x > value then x - 2 * (x - value) else x), y)
    |> Seq.distinct
    |> List.ofSeq

let part1 =
    map
    |> fold (List.head folds)
    |> List.length

let render map =
    let range = List.allPairs [List.minBy; List.maxBy] [fst; snd] |> List.map (fun (l, r) -> l r map)
    
    [snd range.[1] .. snd range.[3]]
    |> List.iter (fun y ->
        [fst range.[0] .. fst range.[2]]
        |> List.map (fun x -> if List.contains (x, y) map then "#" else ".")
        |> List.reduce (+)
        |> printfn "%s")

let part2 =
    folds |> List.fold (fun map' (axis, value) -> fold (axis, value) map') map

[<EntryPoint>]
let main _ =
    printfn $"Part 1: %i{part1}"
    printfn $"Part 2: %A{render part2}"
    0