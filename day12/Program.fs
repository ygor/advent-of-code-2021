open System.IO
open day12.Extensions

let connect (a, b) (caves: Map<string, Set<string>>) =
    let value = Set.add b (if caves.ContainsKey a then caves.[a] else Set.empty)
    Map.add a value caves

let caves =
    File.ReadAllLines("input.txt")
    |> Seq.map (String.split "-")
    |> Seq.fold (fun caves line ->
        caves |> connect (line.[0], line.[1]) |> connect (line.[1], line.[0])) Map.empty
    |> Map.map (fun _ -> Set.toList)
    
let isSmall (cave: string) =
    cave.ToUpper() <> cave
    
let rec visit (paths: string list list) =
    paths
    |> List.map (fun path ->
        if (List.head path = "end") then [[path]]
        else
            caves.[List.head path]
            |> List.map (fun next ->
                if (List.contains next path && isSmall next) then [path] else visit [next :: path]))
    |> (List.concat >> List.concat)

let part1 =
    visit [["start"]]
    |> List.filter (fun path -> List.head path = "end")
    |> List.length
    
[<EntryPoint>]
let main _ =
    printfn $"Part 1: %A{part1}"
    0