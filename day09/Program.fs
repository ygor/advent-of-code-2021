open System.IO
open day09.Extensions

let map =
    File.ReadAllLines("input.txt")
    |> Seq.map (fun line -> line |> Seq.map (string >> int) |> List.ofSeq)
    |> List.ofSeq

let height, width = List.length map, List.length map.[0]

let adjacents (x,y) =
    [(-1, 0); (1, 0); (0, -1); (0, 1)]
    |> Seq.map (fun (dx, dy) -> (x + dx, y + dy))
    |> Seq.filter (fun (x, y) -> x >= 0 && x < width && y >= 0 && y < height)
    |> Set.ofSeq

let lows =
    [0 .. (width - 1)]
    |> List.fold (fun lows x ->
        [0 ..  (height - 1)]
        |> List.fold (fun lows' y ->
            let points = adjacents (x, y) |> Set.map (fun (a, b) -> map.[b].[a])
            if Set.all (fun h -> h > map.[y].[x]) points then (x, y) :: lows' else lows') lows) []
    
let rec basin area =
    let area' =
        area
        |> Set.map adjacents
        |> Set.unionMany
        |> Set.filter (fun (x, y) -> map.[y].[x] < 9)
        |> Set.union area
    
    if Set.difference area' area = Set.empty then area' else basin area'      

let part1 =
    lows |> List.sumBy (fun (x, y) -> map.[y].[x] + 1)

let part2 =
    lows
    |> Seq.map (fun point -> Set.add point Set.empty)
    |> Seq.map (basin >> Set.count)
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.reduce (*)
    
[<EntryPoint>]
let main _ =
    printfn $"Part 1: %A{part1}"
    printfn $"Part 2: %A{part2}"
    0