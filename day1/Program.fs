open System.IO

let measurements =
    File.ReadAllLines("input.txt")
    |> Seq.toList
    |> List.map int

let rec part1 = function
    | x :: y :: ys -> part1 (y :: ys) + (if y > x then 1 else 0)
    | _ -> 0

let part2 =
    measurements
    |> List.windowed 3
    |> List.map List.sum
    |> part1

[<EntryPoint>]
let main _ =
    printfn $"Part 1: {part1 measurements}"
    printfn $"Part 2: {part2}"
    0