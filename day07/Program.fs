open System.IO
open day07.Extensions

let positions =
    File.ReadAllText("input.txt")
    |> String.split ","
    |> List.map int

let fuel positions rate =
    [Seq.min positions .. Seq.max positions]
    |> Seq.map (fun x -> Seq.sumBy (fun y -> rate (abs (y - x))) positions)
    |> Seq.min
    
[<EntryPoint>]
let main _ =
    printfn $"Part 1: %i{fuel positions id}"
    printfn $"Part 2: %i{fuel positions (fun x -> x * (x + 1) / 2)}"
    0