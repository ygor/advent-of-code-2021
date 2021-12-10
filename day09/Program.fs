open System.IO
open day09.Extensions

let heightmap =
    File.ReadAllLines("input.txt")
    |> Seq.mapi (fun y line ->
        line
        |> Seq.mapi (fun x value -> (x, y), (string >> int) value)
        |> List.ofSeq)
    |> List.ofSeq

let adjacents (x,y) =
    let height, width = List.length heightmap, List.length heightmap.[0]
    
    [(-1, 0); (1, 0); (0, -1); (0, 1)]
    |> Seq.map (fun (dx, dy) -> (x + dx, y + dy))
    |> Seq.filter (fun (x, y) -> x >= 0 && x < width && y >= 0 && y < height)
    |> Set.ofSeq

let lows =
    let height, width = List.length heightmap, List.length heightmap.[0]
    
    [0 .. (height - 1)]
    |> List.fold (fun lows y ->
        [0 ..  (width - 1)]
        |> List.fold (fun lows' x ->
            let points =
                adjacents (x, y)
                |> Set.map (fun (a, b) -> snd heightmap.[b].[a])
                
            if Set.all (fun height -> height > snd heightmap.[y].[x]) points
            then heightmap.[y].[x] :: lows'
            else lows') lows) []
    
let rec basin (area: Set<int * int>) =
    let border =
        area
        |> Set.map adjacents
        |> Set.unionMany
        |> (><) Set.difference area

    let area' =
        border
        |> Set.filter (fun (x, y) ->
            snd heightmap.[y].[x] < 9 &&
            adjacents (x, y)
            |> Set.intersect area
            |> Set.filter (fun (a, b) -> snd heightmap.[b].[a] <= snd heightmap.[y].[x])
            |> Set.count >= 1)
        |> Set.union area
    
    if Set.difference area' area = Set.empty then area' else basin area'      

let part1 =
    lows |> List.sumBy (fun (_, h) -> h + 1)

let part2 =
    lows
    |> Seq.map (fun (point, _) -> Set.add point Set.empty)
    |> Seq.map (basin >> Set.count)
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.reduce (*)
    
[<EntryPoint>]
let main _ =
    printfn $"%A{part1}"
    printfn $"%A{part2}"
    0