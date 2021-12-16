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

let adjacents (x, y) =
    [(0, 1); (1, 0); (-1, 0); (0, -1)] |> List.map (fun (dx, dy) -> x + dx, y + dy)

let findPath (risks: Map<int * int, int>) =
    let width, height = dimensions risks
    let finish = (width - 1, height - 1)

    let rec loop (queue: ((int * int) * int) list) (visited: Set<int * int>) (map: Map<int * int, int>) =
        if queue.IsEmpty then map.[finish]
        else
            let current = List.minBy snd queue |> fst
            let unvisited =
                adjacents current
                |> List.filter (fun p -> risks.Keys.Contains p && not(visited.Contains p))
 
            let map' =
                unvisited
                |> List.fold (fun (map: Map<int * int, int>) a ->
                    let value = map.[current] + risks.[a]
                    map.Add (a, if map.ContainsKey a && map.[a] < value then map.[a] else value)) map

            let queue' =
                unvisited
                |> List.map (fun p -> p, map'.[p])
                |> List.append (List.filter (fun (a, _) -> a <> current) queue)
            
            loop queue' (visited.Add current) map'

    loop [(0, 0),0] (Set.add (0,0) Set.empty) (Map.add (0,0) 0 Map.empty)
    
[<EntryPoint>]
let main _ =
    printfn $"Part 1: %i{findPath risks}"
    printfn $"Part 2: %i{tiling risks |> findPath}"
    0