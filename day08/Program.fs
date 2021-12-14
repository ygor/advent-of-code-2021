open System.IO
open day08.Extensions

let numbers =
    [ "abcefg"
      "cf"
      "acdeg"
      "acdfg"
      "bcdf"
      "abdfg"
      "abdefg"
      "acf"
      "abcdefg"
      "abcdfg" ]
    
let segments = "abcdefg" |> List.ofSeq    
    
let notes =
    File.ReadAllLines("input.txt")
    |> Seq.map (fun line ->
        line
        |> String.split " | "
        |> List.map (String.split " ")
        |> List.unpack2)
        
let part1 =
    let lengths =
        numbers
        |> List.groupBy Seq.length
        |> List.filter (snd >> List.length >> ((=) 1))
        |> List.map fst
    
    notes
    |> Seq.sumBy (fun (_, digits) ->
        digits |> List.lengthBy (fun digit -> Seq.contains digit.Length lengths))

let decode (value: string) (key: char list) =
    let map = List.zip key segments |> Map.ofList
    value
    |> Seq.map (fun s -> string map.[s])
    |> Seq.sort
    |> Seq.reduce (+)

let solve (signals: string list) =
    segments
    |> List.permute
    |> List.find (fun permutation ->
        List.all (fun signal -> List.contains (decode signal permutation) numbers) signals)

let part2 =
    notes
    |> Seq.sumBy (fun (signals, digits) ->
        let key = solve signals
        digits
        |> List.map (fun digit ->
            numbers
            |> List.findIndex (fun number -> number = decode digit key)
            |> string)
        |> (List.reduce (+) >> int))

[<EntryPoint>]
let main _ =
    printfn $"%i{part1}"
    printfn $"%i{part2}"
    0