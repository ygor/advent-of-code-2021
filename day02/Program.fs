open System.IO
open Extensions

let input = File.ReadAllLines("input.txt") 

let part1 =
    input
    |> Seq.fold (fun (horizontal, depth) line ->
        match line with
        | Regex "forward (\d+)" [ number ] -> (horizontal + int number, depth)
        | Regex "down (\d+)" [ number ] -> (horizontal, depth + int number)
        | Regex "up (\d+)" [ number ] -> (horizontal, depth - int number)
        | _ -> failwithf $"Invalid input %s{line}") (0, 0)
    |> (fun (a, b) -> a * b)

let part2 =
    input
    |> Seq.fold (fun (horizontal, depth, aim) line ->
        match line with
        | Regex "forward (\d+)" [ number ] -> (horizontal + int number, depth + (aim * int number), aim)
        | Regex "down (\d+)" [ number ] -> (horizontal, depth, aim + int number)
        | Regex "up (\d+)" [ number ] -> (horizontal, depth, aim - int number)
        | _ -> failwithf $"Invalid input %s{line}") (0, 0, 0)
    |> (fun (a, b, _) -> a * b)

[<EntryPoint>]
let main _ =
    printfn $"Part 1: {part1}"
    printfn $"Part 2: {part2}"
    0
