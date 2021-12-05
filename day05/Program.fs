open System.IO
open day05.Extensions

let lines =
    File.ReadAllLines("input.txt")
    |> Seq.map (fun line ->
        match line with
        | Regex "(\d+),(\d+) -> (\d+),(\d+)" [ x1; y1; x2; y2 ] -> (int x1, int y1), (int x2, int y2)
        | _ -> failwithf $"Invalid input %s{line}")
    |> List.ofSeq

let horizontals =
    lines
    |> List.filter (fun ((x1, y1), (x2, y2)) -> x1 = x2 || y1 = y2)
    |> List.map (fun ((x1, y1), (x2, y2)) ->
        List.allPairs [x1 .. Math.sign (x2 - x1) .. x2] [y1 .. Math.sign (y2 - y1) .. y2])

let verticals =
    lines
    |> List.filter (fun ((x1, y1), (x2, y2)) -> abs (x2 - x1) = abs (y2 - y1))
    |> List.map (fun ((x1, y1), (x2, y2)) ->
        List.zip [x1 .. Math.sign (x2 - x1) .. x2] [y1 .. Math.sign (y2 - y1) .. y2])
    
let overlaps lines =
    lines
    |> List.concat
    |> List.groupBy id
    |> List.filter (snd  >> List.length >> (<) 1)
    |> List.length
    
[<EntryPoint>]
let main _ =
    printfn $"Part 1: {overlaps horizontals}"
    printfn $"Part 2: {overlaps (horizontals @ verticals)}"
    0