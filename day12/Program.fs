open System.IO
open day12.Extensions

let caves =
    File.ReadAllLines("input.txt")
    |> List.ofArray
    |> List.flatMap (String.split2 "-" >> (fun (l, r) -> [(l, r); (r, l)]))
    |> List.groupMap fst snd 
    |> Map.ofList

let isLarge (cave: string) = cave.ToUpper() = cave
    
let rec count (path: string list) canVisit =
    if List.head path = "end" then 1
    else
        caves.[List.head path]
        |> List.filter (fun next ->
            next <> "start" && (isLarge next || not (List.contains next path) || canVisit path))
        |> List.sumBy (fun next -> count (next :: path) canVisit)

let part1 =
    count [ "start" ] (fun _ -> false)

let part2 =
    count [ "start" ] (fun path ->
        path
        |> List.filter (isLarge >> not)
        |> (fun caves -> caves |> List.distinct |> List.length = caves.Length))        

[<EntryPoint>]
let main _ =
    printfn $"Part 1: %i{part1}"
    printfn $"Part 2: %i{part2}"
    0