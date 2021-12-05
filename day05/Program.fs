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
        List.allPairs [x1 .. (if x2 > x1 then 1 else -1) .. x2] [y1 .. (if y2 > y1 then 1 else -1) .. y2])

let verticals =
    lines
    |> List.filter (fun ((x1, y1), (x2, y2)) -> abs (x2 - x1) = abs (y2 - y1))
    |> List.map (fun ((x1, y1), (x2, y2)) ->
        List.zip [int x1 .. (if x2 > x1 then 1 else -1) .. int x2] [int y1 .. (if y2 > y1 then 1 else -1) .. int y2])
    
let overlap lines =
    lines
    |> List.concat
    |> List.groupBy id
    |> List.filter (snd  >> List.length >> (<) 1)
    |> List.length
    
[<EntryPoint>]
let main _ =
    printfn $"Part 1: {overlap horizontals}"
    printfn $"Part 2: {overlap (horizontals @ verticals)}"
    0