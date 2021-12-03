open System
open System.IO
open day03.Extensions

let lines =
    File.ReadAllLines("input.txt")
    |> Seq.map (Seq.map string >> List.ofSeq)
    |> List.ofSeq

let fromBinary (value: string seq) =
    Convert.ToInt32(Seq.reduce (+) value, 2)

let power =
    lines
    |> Seq.transpose
    |> Seq.map (fun column ->
        column
        |> Seq.groupBy id
        |> Seq.sortBy (fun (_, values) -> Seq.length values)
        |> Seq.map (fun (value, _) -> string value))
    |> Seq.transpose
    |> Seq.map fromBinary
    |> Seq.reduce (*)

let rec iterate index i (lines: string list list) =
    match Seq.length lines with
    | 1 -> List.head lines
    | _ ->
        lines
        |> List.groupBy (fun list -> list.[index])
        |> List.map (fun (c, lines') -> ((List.length lines', c), lines'))
        |> List.sortBy fst
        |> List.map snd
        |> List.at i
        |> iterate (index + 1) i

let life =
    [0..1]
    |> Seq.map (fun i -> iterate 0 i lines)
    |> Seq.map fromBinary
    |> Seq.reduce (*)
    
[<EntryPoint>]
let main _ =
    printfn $"Part 1: %i{power}"
    printfn $"Part 2: %i{life}"
    0