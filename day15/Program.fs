open System.IO
open day12.Extensions

let risks =
    File.ReadAllLines("input.txt")
    |> Seq.mapi (fun y -> Seq.mapi (fun x value -> (x, y), ((string >> int) value)))
    |> (Seq.concat >> Map.ofSeq)

let dimensions (map: Map<int * int, int>) =
    (map.Keys |> Seq.maxBy fst |> fst) + 1, (map.Keys |> Seq.maxBy snd |> snd) + 1    

let tiling (map: Map<int * int, int>) =
    let width, height = dimensions map
    List.foldn (5 * width) (fun outer x ->
        List.foldn (5 * height) (fun inner y ->
            let value = (map.[x % width, y % height] + x / width + y / height - 1) % 9 + 1 
            Map.add (x, y) value inner) outer) Map.empty

let neighbours (x, y) =
    [(0, 1); (1, 0); (-1, 0); (0, -1)] |> List.map (fun (dx, dy) -> x + dx, y + dy)

let updateNeighbours current neighbours (risks: Map<int * int, int>) map =
    neighbours
    |> List.fold (fun (map: Map<int * int, int>) a ->
        let value = map.[current] + risks.[a]
        map.Add (a, if map.ContainsKey a && map.[a] < value then map.[a] else value)) map

let remove current queue = List.filter (fst >> (<>) current) queue
    
let enqueue unvisited (map: Map<int * int, int>) queue =
    unvisited
    |> List.map (fun p -> p, map.[p])
    |> List.append queue
    
let findPath (risks: Map<int * int, int>) =
    let width, height = dimensions risks
    
    let rec loop (queue: ((int * int) * int) list) (visited: Set<int * int>) (map: Map<int * int, int>) =
        if queue.IsEmpty then map.[width - 1, height - 1]
        else
            let current = List.minBy snd queue |> fst
            let unvisited = neighbours current |> List.filter (fun p -> risks.Keys.Contains p && not(visited.Contains p))
            let map' = updateNeighbours current unvisited risks map
            let queue' = queue |> remove current |> enqueue unvisited map'
            
            loop queue' (visited.Add current) map'

    loop [(0, 0),0] (Set.add (0,0) Set.empty) (Map.add (0,0) 0 Map.empty)
    
[<EntryPoint>]
let main _ =
    printfn $"Part 1: %i{findPath risks}"
    printfn $"Part 2: %i{tiling risks |> findPath}"
    0