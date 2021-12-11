open System.IO
open day11.Extensions

let input = File.ReadAllLines("input.txt") 
let map =
    input
    |> Seq.mapi (fun y line -> line |> Seq.mapi (fun x value -> (x, y), ((string >> int) value, false)))
    |> Seq.concat
    |> Map.ofSeq

let height, width = Seq.length input, Seq.length input.[0]

let adjacents (x,y) =
    List.allPairs [-1;0;1] [-1;0;1]
    |> List.map (fun (dx, dy) -> (x + dx, y + dy))
    |> List.filter (fun (x, y) -> x >= 0 && x < width && y >= 0 && y < height)
    |> List.except [(x, y)]

let rec flash (map: Map<int * int, int * bool>) =
    let map' =
        List.foldn width (fun map x ->
            List.foldn height (fun (map: Map<int * int, int * bool>) y ->
                if fst map.[x, y] > 9 then
                    let map' = Map.add (x, y) (0, true) map
                    adjacents (x, y)
                    |> List.fold (fun map'' (a, b) ->
                        if not (snd map''.[a, b])
                        then Map.add (a, b) (fst map''.[a, b] + 1, snd map''.[a, b]) map''
                        else map'') map'
                else map) map) map
    if Map.exists (fun _ (energy, flashed) -> energy > 9 && not flashed) map' then flash map' else map'
    
let step map =
    map
    |> Map.map (fun _ value -> fst value + 1, false)
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