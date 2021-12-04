open System.IO
open day04.Extensions

let input = File.ReadAllText("input.txt") |> String.split "\n\n"

let numbers =
    List.head input |> String.split "," |> List.map int

let boards =
    List.tail input
    |> List.map (fun block ->
        block
        |> String.split "\n"
        |> List.map (String.trim >> Regex.split "\s+" >> List.map int)
        |> (fun rows -> List.concat [rows; List.transpose rows]))
    |> List.indexed
    |> Map.ofList
    
let isWinner drawn board =
    Seq.any (fun numbers -> List.except drawn numbers = []) board
    
let result boards numbers =
    numbers
    |> Seq.fold (fun (winners, boards', drawn) number ->
        let drawn' = number :: drawn
        let winners' =
            boards'
            |> Map.filter (fun _ -> isWinner drawn')
            |> Map.map (fun _ board -> board, drawn')
        
        Map.values winners' |> Seq.append winners,
        Map.removeMany (Map.keys winners') boards',
        drawn') (Seq.empty, boards, [])

let score (board, drawn) =
    let sum = List.except drawn (List.concat board) |> List.sum
    sum * Seq.head drawn
    
[<EntryPoint>]
let main _ =
    let winners, _, _ = result boards numbers
    let first = winners |> Seq.minBy (snd >> List.length)
    let last = winners |> Seq.maxBy (snd >> List.length)
    
    printfn $"Part 1: %i{score first}"
    printfn $"Part 2: %i{score last}"
    0