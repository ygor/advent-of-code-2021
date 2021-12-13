open System.IO

let input = File.ReadAllLines("input.txt")

let grid: Map<(int * int), (int * bool)> =
    input
    |> Seq.mapi (fun y -> Seq.mapi (fun x value -> (x, y), ((string >> int) value, false)))
    |> (Seq.concat >> Map.ofSeq)

let adjacents (x, y) =
    List.allPairs [-1; 0; 1] [-1; 0; 1]
    |> List.map (fun (dx, dy) -> (x + dx, y + dy))
    |> List.filter (fun (x, y) -> x >= 0 && x < input.[0].Length && y >= 0 && y < input.Length)

let cascade (x, y) (grid: Map<(int * int), (int * bool)>) =
    adjacents (x, y)
    |> List.fold (fun (grid': Map<(int * int), (int * bool)>) (a, b) ->
        let energy, flashed = grid'.[a, b]
        if not flashed then grid'.Add ((a, b), (energy + 1, flashed)) else grid') grid
    
let rec flash (grid: Map<(int * int), (int * bool)>) =
    let grid' =
        grid.Keys
        |> Seq.fold (fun (grid: Map<(int * int), (int * bool)>) (x, y) ->
            if fst grid.[x, y] > 9 then cascade (x, y) (grid.Add ((x, y), (0, true))) else grid) grid
    if Map.exists (fun _ (energy, flashed) -> energy > 9 && not flashed) grid' then flash grid' else grid'
    
let step grid = Map.map (fun _ (energy, _) -> energy + 1, false) grid |> flash
    
let part1 steps =
    [0 .. (steps - 1)]
    |> List.scan (fun grid' _ -> step grid') grid
    |> List.sumBy (Map.filter (fun _ -> snd) >> Map.count)

let rec part2 grid =
    if Map.forall (fun _ (energy, _) -> energy = 0) grid then 0 else 1 + part2 (step grid)
    
[<EntryPoint>]
let main _ =
    printfn $"Part 1: %i{part1 100}"
    printfn $"Part 2: %i{part2 grid}"
    0