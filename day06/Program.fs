open System.IO
open day05.Extensions

let fish =
    File.ReadAllText("input.txt")
    |> String.split ","
    |> Seq.map int
    |> Seq.groupBy id
    |> Seq.map (fun (f, fs) -> f, bigint (Seq.length fs))
    |> Map.ofSeq

let simulate days (fish: Map<int, bigint>) =
    [1 .. days]
    |> Seq.fold (fun fish' _ ->
        fish'
        |> Map.fold (fun fish'' f count ->
            match f with
            | 0 ->
                fish''
                |> Map.add 6 ((Map.getOrDefault 6 (bigint 0) fish'') + count)
                |> Map.add 8 ((Map.getOrDefault 6 (bigint 0) fish'') + count)
            | n -> Map.add (n - 1) ((Map.getOrDefault (n - 1) (bigint 0) fish'') + count) fish'') Map.empty
        ) fish
    |> Map.toSeq
    |> Seq.sumBy snd  
    
[<EntryPoint>]
let main _ =
    printfn $"Part 1: {simulate 80 fish}"
    printfn $"Part 2: {simulate 256 fish}"
    0