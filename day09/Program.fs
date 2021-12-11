open System.IO
open day09.Extensions

let map =
    File.ReadAllLines("input.txt")
    |> Seq.map (fun line -> line |> Seq.map (string >> int) |> List.ofSeq)
    |> List.ofSeq

let height, width = List.length map, List.length map.[0]

let adjacents (x,y) =
    Set.ofList [(-1, 0); (1, 0); (0, -1); (0, 1)]
    |> Set.map (fun (dx, dy) -> (x + dx, y + dy))
    |> Set.filter (fun (x, y) -> x >= 0 && x < width && y >= 0 && y < height)

let lows =
    List.foldn width (fun lows x ->
        List.foldn height (fun lows' y ->
            if Set.all (fun (a, b) -> map.[b].[a] > map.[y].[x]) (adjacents (x, y))
            then Set.add (x, y) lows' else lows') lows) Set.empty<int * int>
    
let rec basin area =
    let area' =
        area
        |> Set.map adjacents
        |> Set.unionMany
        |> Set.filter (fun (x, y) -> map.[y].[x] < 9)
        |> Set.union area
    
    if Set.difference area' area = Set.empty then area' else basin area'      

let part1 =
    lows |> Set.toSeq |> Seq.sumBy (fun (x, y) -> map.[y].[x] + 1)

let part2 =
    lows
    |> Set.map (fun point -> basin (Set.add point Set.empty) |> Set.count)
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.reduce (*)
    
[<EntryPoint>]
let main _ =
    printfn $"Part 1: %A{part1}"
    printfn $"Part 2: %A{part2}"
    0