open System.IO
open day12.Extensions

let caves =
    File.ReadAllLines("input.txt")
    |> List.ofArray
    |> List.flatMap (String.split2 "-" >> (fun (l, r) -> [(l, r); (r, l)]))
    |> List.groupMap fst snd 
    |> Map.ofList

let canVisit1 (cave: string) (path: string list) = 
    not (List.contains cave path) || cave.ToUpper() = cave

let rec visit (paths: string list list) canVisit =
    paths
    |> List.flatMap (fun path ->
        if (List.head path = "end") then [ [ path ] ]
        else
            caves.[List.head path]
            |> List.map (fun next ->
                if canVisit next path then visit [ next :: path ] canVisit else [path]))
    |> List.concat

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