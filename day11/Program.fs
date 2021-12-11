open System.IO
open day11.Extensions

let input = File.ReadAllLines("input.txt")

let map =
    input
    |> Seq.mapi (fun y -> Seq.mapi (fun x value -> (x, y), ((string >> int) value, false)))
    |> Seq.concat
    |> Map.ofSeq

let adjacents (x, y) =
    List.allPairs [-1;0;1] [-1;0;1]
    |> List.map (fun (dx, dy) -> (x + dx, y + dy))
    |> List.filter (fun (x, y) -> x >= 0 && x < (Seq.length input.[0]) && y >= 0 && y < (Seq.length input))

let rec flash map =
    let map' =
        Map.keys map
        |> Seq.fold (fun (map: Map<(int * int),(int * bool)>) (x, y) ->
            if fst map.[x, y] > 9 then
                let map' = Map.add (x, y) (0, true) map
                adjacents (x, y)
                |> List.fold (fun map' (a, b) ->
                    let (energy, flashed) = map'.[a, b]
                    if not flashed then Map.add (a, b) (energy + 1, flashed) map' else map') map'
            else map) map
    if Map.exists (fun _ (energy, flashed) -> energy > 9 && not flashed) map' then flash map' else map'
    
let step map =
    map
    |> Map.map (fun _ (energy, _) -> energy + 1, false)
    |> flash
    
let part1 steps =
    [0 .. (steps - 1)]
    |> List.scan (fun map' _ -> step map') map
    |> List.sumBy (fun map ->
        map
        |> Map.filter (fun _ -> snd)
        |> Map.count)
    
[<EntryPoint>]
let main _ =
    printfn $"Part 1: %i{part1 100}"
    0