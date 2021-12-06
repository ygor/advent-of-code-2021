open System.IO
open day06.Extensions

let fish =
    File.ReadAllText("input.txt")
    |> String.split ","
    |> List.map int
    |> List.fold (fun list value ->
        List.updateAt value (list.[value] + (bigint 1)) list) (List.init 9 (fun _ -> bigint 0)) 

let simulate days (initial: List<bigint>) =
    [1 .. days]
    |> Seq.fold (fun fish _ ->
        let fish' = List.tail fish @ [List.head fish]
        List.updateAt 6 (fish'.[6] + fish'.[8]) fish') initial
    |> List.sum
    
[<EntryPoint>]
let main _ =
    printfn $"Part 1: {simulate 80 fish}"
    printfn $"Part 2: {simulate 256 fish}"
    0