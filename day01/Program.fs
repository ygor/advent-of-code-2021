open System.IO

let measurements =
    File.ReadAllLines("input.txt")
    |> Seq.toList
    |> List.map int

let part1 =
    List.pairwise
    >> List.filter (fun (x, y) -> y > x)
    >> List.length

let part2 =
    List.windowed 3 >> List.map List.sum >> part1

[<EntryPoint>]
let main _ =
    printfn $"Part 1: {part1 measurements}"
    printfn $"Part 2: {part2 measurements}"
    0
