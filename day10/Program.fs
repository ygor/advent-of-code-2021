open System.IO

type Chunk =
    | Leaf of char
    | Node of char * Chunk list

type Line =
    | Incomplete of char list
    | Corrupt of char
    | Chunks of Chunk list
    
let lines =
    File.ReadAllLines("input.txt")

let map = [('(', ')'); ('<', '>'); ('{', '}'); ('[', ']')] |> Map.ofList    

let rec parse (chunks: Chunk list) (opens: char list) line =
    if Seq.length line = 0 then
        if List.length opens = 0 then Chunks chunks else Incomplete opens
    else
        let c = Seq.head line
        if Seq.contains c "([<{" then parse chunks (c :: opens) (Seq.tail line)
        elif List.length opens > 0 && map.[List.head opens] = c then
            let chunk =
                match List.head chunks with
                | Leaf o -> Node (o, [Leaf (List.head opens)])
                | Node (o, xs) -> Node (o, (Leaf (List.head opens)) :: xs) 
            parse (List.updateAt 0 chunk chunks) (List.tail opens) (Seq.tail line)
        else
            Corrupt c 

let part1 =
    let points = [(')', 3); ('>', 25137); ('}', 1197); (']', 57)] |> Map.ofList

    lines
    |> Seq.map (parse [Leaf '.'] [])
    |> Seq.sumBy (fun line ->
        match line with
        | Corrupt c -> points.[c]
        | _ -> 0 )
    
let part2 =
    let points = [(')', 1); ('>', 4); ('}', 3); (']', 2)] |> Map.ofList

    lines
    |> Seq.map (parse [Leaf '.'] [])
    |> Seq.map (fun line ->
        match line with
        | Incomplete cs -> List.fold (fun score c -> score * (bigint 5) + (bigint points.[map.[c]])) (bigint 0) cs
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