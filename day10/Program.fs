open day10.Extensions
    
let lines = System.IO.File.ReadAllLines("input.txt") |> Seq.map List.ofSeq
let map = [('(', ')'); ('<', '>'); ('{', '}'); ('[', ']')] |> Map.ofList    

let rec parse (opened: char list) (line: char list) =
    match line with
    | [] -> Result.Ok opened
    | char :: tail ->
        if Seq.contains char "([<{" then parse (char :: opened) tail
        elif List.length opened > 0 && map.[List.head opened] = char then parse (List.tail opened) tail
        else Result.Error char

let incomplete, corrupted = Seq.map (parse []) lines |> Seq.partitionMap id

let part1 =
    let points = [(')', 3); ('>', 25137); ('}', 1197); (']', 57)] |> Map.ofList
    corrupted |> Seq.sumBy (fun c -> points.[c])
    
let part2 =
    let points = [(')', bigint 1); ('>', bigint 4); ('}', bigint 3); (']', bigint 2)] |> Map.ofList
    incomplete
    |> Seq.map (List.fold (fun score c -> score * (bigint 5) + points.[map.[c]]) (bigint 0))
    |> Seq.sort
    |> (fun scores -> Seq.skip ((Seq.length scores - 1) / 2) scores |> Seq.head)
        
[<EntryPoint>]
let main _ =
    printfn $"Part 1: %i{part1}"
    printfn $"Part 2: {part2}"
    0