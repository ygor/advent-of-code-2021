open System.IO

type Chunk =
    | Leaf of char
    | Node of char * Chunk list

type Line =
    | Incomplete of char list
    | Corrupt of char
    | Chunks of Chunk list
    
let lines =
    File.ReadAllLines("input.txt") |> Seq.map (List.ofSeq)

let map = [('(', ')'); ('<', '>'); ('{', '}'); ('[', ']')] |> Map.ofList    

let rec parse (chunks: Chunk list) (opens: char list) (line: char list) =
    match line with
    | [] -> if List.length opens = 0 then Chunks chunks else Incomplete opens
    | char :: tail ->
        if Seq.contains char "([<{" then parse chunks (char :: opens) tail
        elif List.length opens > 0 && map.[List.head opens] = char then
            let chunk =
                match List.head chunks with
                | Leaf o -> Node (o, [Leaf (List.head opens)])
                | Node (o, xs) -> Node (o, (Leaf (List.head opens)) :: xs) 
            parse (List.updateAt 0 chunk chunks) (List.tail opens) tail
        else Corrupt char

let part1 =
    let points = [(')', 3); ('>', 25137); ('}', 1197); (']', 57)] |> Map.ofList

    lines
    |> Seq.map (parse [Leaf '.'] [])
    |> Seq.sumBy (fun line ->
        match line with
        | Corrupt c -> points.[c]
        | _ -> 0 )
    
let part2 =
    let points = [(')', bigint 1); ('>', bigint 4); ('}', bigint 3); (']', bigint 2)] |> Map.ofList

    lines
    |> Seq.map (fun line ->
        match parse [Leaf '.'] [] line with
        | Incomplete cs -> List.fold (fun score c -> score * (bigint 5) + points.[map.[c]]) (bigint 0) cs
        | _ -> bigint 0 )
    |> Seq.filter ((<) (bigint 0))
    |> Seq.sort
    |> (fun scores -> Seq.skip ((Seq.length scores - 1) / 2) scores)
    |> Seq.head
        
[<EntryPoint>]
let main _ =
    printfn $"Part 1: %i{part1}"
    printfn $"Part 2: {part2}"
    0