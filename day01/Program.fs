open System.IO
open Extensions

let measurements =
    File.ReadAllLines("input.txt") |> Seq.map int

let part1 =
    Seq.pairwise >> Seq.lengthBy (fun (x, y) -> y > x)

let part2 =
    Seq.windowed 3 >> Seq.map Seq.sum >> part1

[<EntryPoint>]
let main _ =
    printfn $"Part 1: {part1 measurements}"
    printfn $"Part 2: {part2 measurements}"
    0
