open System.IO
open day04.Extensions

type Board = int list list

let input = File.ReadAllText("input.txt") |> String.split "\n\n"

let numbers =
    Seq.head input
    |> String.split ","
    |> List.map int

let boards : Board list =
    List.tail input
    |> List.map (fun block ->
        block
        |> String.split "\n"
        |> List.map (String.trim >> Regex.split "\s+" >> List.map int)
        |> (fun board -> List.concat [board; List.transpose board]))

let isWinner drawn board =
    Seq.any (fun numbers -> List.except drawn numbers = []) board
    
let result (boards: Board list) (numbers: int list) =
    numbers
    |> Seq.fold (fun ((boards': Map<int, Board>), winners, drawn) number ->
        let drawn' = number :: drawn
        let winners' =
            boards'
            |> Map.filter (fun _ -> isWinner drawn')
            |> Map.map (fun _ board -> (board, drawn'))
        
        Map.removeMany (Map.keys winners') boards',
        winners' |> Map.values |> List.ofSeq |> List.append winners,
        drawn') (List.indexed boards |> Map.ofList, [], [])

let score (board: Board, drawn) =
    let sum = List.except drawn (List.concat board) |> List.fold (+) 0
    sum * Seq.head drawn
    
[<EntryPoint>]
let main _ =
    let (_, winners, _) = result boards numbers
    let first = winners |> List.minBy (fun (board, draw) -> List.length draw)
    let last = winners |> List.maxBy (fun (board, draw) -> List.length draw)
    
    printfn $"Part 1: %i{score first}"
    printfn $"Part 2: %i{score last}"
    0