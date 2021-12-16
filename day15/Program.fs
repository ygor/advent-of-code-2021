open System.IO
open day12.Extensions

let risks =
    File.ReadAllLines("input.txt")
    |> Seq.mapi (fun y -> Seq.mapi (fun x value -> (x, y), ((string >> int) value)))
    |> (Seq.concat >> Map.ofSeq)

let dimensions map =
    Map.keys map |> Seq.maxBy fst |> fst, Map.keys map |> Seq.maxBy snd |> snd    

let tiling (map: Map<int * int, int>) =
    let width, height = dimensions map
    List.foldn (5 * width) (fun outer x ->
        List.foldn (5 * height) (fun inner y ->
            let value = (map.[x % width, y % height] + x / width + y / height - 1) % 9 + 1 
            Map.add (x, y) value inner) outer) Map.empty

let adjacents (x, y) =
    [(0, 1); (1, 0); (-1, 0); (0, -1)] |> List.map (fun (dx, dy) -> x + dx, y + dy)

let rec findPath current (visited: Set<int * int>) (risks: Map<int * int, int>) (map: Map<int * int, int>) =
    let width, height = dimensions risks
    let unvisited = adjacents current |> List.filter (fun p -> risks.Keys.Contains p && not(visited.Contains p))
    let map'' =
        unvisited
        |> List.fold (fun (map: Map<int * int, int>) a ->
            let value = map.[current] + risks.[a]
            map.Add (a, if map.ContainsKey a && map.[a] < value then map.[a] else value)) map

    if current = (width - 1, height - 1)
    then map''.[width - 1, height - 1]
    else
        let next = unvisited |> Seq.min
        findPath next (visited.Add current) risks map''
    
let part1 = findPath (0, 0) Set.empty risks (Map.add (0,0) 0 Map.empty)
    
let part2 =
    let risks' = tiling risks
    findPath (0, 0) Set.empty risks' (Map.add (0,0) 0 Map.empty)
    
[<EntryPoint>]
let main _ =
    printfn $"Part 1: %i{part1}"
    printfn $"Part 2: %i{part2}"
    0