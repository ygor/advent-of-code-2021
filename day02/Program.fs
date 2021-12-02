open System.IO
open Extensions

let course =
    File.ReadAllLines("input.txt")
    |> Seq.fold
        (fun ((h1, d1), (h2, d2, aim)) line ->
            match line with
            | Regex "forward (\d+)" [ x ] -> ((h1 + int x, d1), (h2 + int x, d2 + (aim * int x), aim))
            | Regex "down (\d+)" [ x ] -> ((h1, d1 + int x), (h2, d2, aim + int x))
            | Regex "up (\d+)" [ x ] -> ((h1, d1 - int x), (h2, d2, aim - int x))
            | _ -> failwithf $"Invalid input %s{line}")
        ((0, 0), (0, 0, 0))

let part1 = fst course |> (fun (h, d) -> h * d)

let part2 = snd course |> (fun (h, d, _) -> h * d)

[<EntryPoint>]
let main _ =
    printfn $"Part 1: {part1}"
    printfn $"Part 2: {part2}"
    0
