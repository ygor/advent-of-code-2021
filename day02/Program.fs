open System.IO
open Extensions

let input = File.ReadAllLines("input.txt")

let course =
    input
    |> Seq.fold
        (fun ((h1, d1), (h2, d2, aim)) line ->
            match line with
            | Regex "forward (\d+)" [ x ] -> ((h1 + int x, d1), (h2 + int x, d2 + (aim * int x), aim))
            | Regex "down (\d+)" [ x ] -> ((h1, d1 + int x), (h2, d2, aim + int x))
            | Regex "up (\d+)" [ x ] -> ((h1, d1 - int x), (h2, d2, aim - int x))
            | _ -> failwithf $"Invalid input %s{line}")
        ((0, 0), (0, 0, 0))

let part1 = course |> (fun ((h, d), _) -> h * d)

let part2 = course |> (fun (_, (h, d, _)) -> h * d)

[<EntryPoint>]
let main _ =
    printfn $"Part 1: {part1}"
    printfn $"Part 2: {part2}"
    0