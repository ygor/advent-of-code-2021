open System.IO
open day08.Extensions

let numbers =
    [ "abcefg"
      "cf"
      "acdeg"
      "acdfg"
      "bcdf"
      "abdfg"
      "abdefg"
      "acf"
      "abcdefg"
      "abcdfg" ]
    |> List.map Set.ofSeq
    
let notes =
    File.ReadAllLines("input.txt")
    |> Seq.map (fun line ->
        line
        |> String.split " | "
        |> List.map (String.split " ")
        |> (fun entry -> (entry.[0], entry.[1])))
let part1 =
    let uniques = numbers |> Seq.groupBy Set.count |> Seq.head |> Seq.map snd >> Set.count 
    
    notes
    |> Seq.sumBy (fun (_, digits) ->
        digits
        |> List.filter (fun digit -> Seq.contains (List.length digit) uniques)
        |> List.length)

let branches (signals: string list list) =
    let groups = List.groupBy Set.count numbers |> Map.ofSeq
    
    signals
    |> List.map (fun signal -> signal, groups.[Seq.length signal])
    |> List.fold (fun branches (signal, numbers) ->
        branches
        |> List.map (fun branch -> numbers |> List.map (fun number -> (signal, number) :: branch))
        |> List.concat) [[]]

let solve (map: Map<string, Set<string>>) =
    let solved = Map.filter (fun _ values -> Set.count values = 1) map
    
    map
    |> Map.map (fun key values ->
        if Map.containsKey key solved then values
        else Set.difference values (Map.values solved |> Set.unionMany))

let update (map: Map<string, Set<string>>) (signal: string list, segments: Set<string>) =
    signal
    |> List.fold (fun map' wire ->
        let segments' = if Map.containsKey wire map' then Set.intersect map'.[wire] segments else segments
        Map.add wire segments' map') map
    
let rec decode signals =    
    signals
    |> branches
    |> List.map (fun branch ->
        branch
        |> List.fold update Map.empty<string, Set<string>>
        |> solve)
    |> List.find (fun map ->
        let count =
            map
            |> Map.values
            |> Seq.filter (Set.count >> (=) 1)
            |> Seq.length
        count = Map.count map)

let decrypt (map: Map<string, Set<string>>) (digit: string list) =
    let segments =
        digit
        |> Seq.map (fun d -> map.[d])
        |> Set.unionMany
    
    Seq.findIndex (fun number -> number = segments)

let number (digits: string list list) (map: Map<string, Set<string>>) =
    digits
    |> Seq.map (decrypt map >> string)
    |> Seq.reduce (+)
    |> int
    
let part2 =
    notes
    |> Seq.map (fun (signals, digits) ->
        let map = decode signals
        number digits map)
    |> Seq.sum

[<EntryPoint>]
let main _ =
    printfn $"%A{part2}"
    0
