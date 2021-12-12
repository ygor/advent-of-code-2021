open System.IO
open day12.Extensions

let connect (a, b) (caves: Map<string, Set<string>>) =
    let value = Set.add b (if caves.ContainsKey a then caves.[a] else Set.empty)
    Map.add a value caves

let caves =
    File.ReadAllLines("input.txt")
    |> Seq.map (String.split "-")
    |> Seq.fold (fun caves line ->
        caves
        |> connect (line.[0], line.[1])
        |> connect (line.[1], line.[0])) Map.empty
    |> Map.map (fun _ -> Set.toList)

let canVisit1 (cave: string) (path: string list) = 
    not (List.contains cave path) || cave.ToUpper() = cave
    
let rec visit (paths: string list list) canVisit =
    paths
    |> List.map (fun path ->
        if (List.head path = "end") then [ [ path ] ]
        else
            caves.[List.head path]
            |> List.map (fun next ->
                if canVisit next path then visit [ next :: path ] canVisit else [path]))
    |> (List.concat >> List.concat)

let paths canVisit =
    visit [ [ "start" ] ] canVisit
    |> List.filter (fun path -> List.head path = "end")
    
let part1 = paths canVisit1 |> List.length

let canVisit2 (cave: string) (path: string list) =
    let map =
        path
        |> List.countBy id
        |> List.filter (fun (cave, count) -> cave.ToUpper() <> cave && count > 1)
        |> Map.ofList
        
    cave <> "start" && (cave.ToUpper() = cave || map.Count = 0 || not (List.contains cave path))

let part2 = paths canVisit2 |> List.length

[<EntryPoint>]
let main _ =
    printfn $"Part 1: %i{part1}"
    printfn $"Part 1: %i{part2}"
    0