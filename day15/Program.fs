open System.IO
open day12.Extensions

let input = File.ReadAllLines("input.txt")

let map =
    input
    |> Seq.mapi (fun y -> Seq.mapi (fun x value -> (x, y), ((string >> int) value)))
    |> (Seq.concat >> Map.ofSeq)

let width, height = Seq.length input.[0], Seq.length input

let expand (map: Map<int * int, int>) =
    List.foldn width (fun a x ->
        List.foldn height (fun b y ->
            List.foldn 5 (fun c mx ->
                List.foldn 5 (fun (d: Map<int * int, int>) my ->
                    let x', y' = x + (mx * width), y + (my * height)
                    let value = (if mx > 0 then (d.[x' - width, y'] + 1) elif my > 0 then (d.[x', y' - height] + 1) else map.[x, y]) % 10 
                    Map.add (x', y') (if value > 0 then value else 1) d) c) b) a) Map.empty

let delta width height (map: Map<int * int, int>) =
    let init =
        Map.add (0, 0) map.[0, 0] Map.empty<int * int, int>
        |> List.foldn (width - 1) (fun delta' x -> Map.add (x + 1, 0) (delta'.[x, 0] + map.[x + 1, 0]) delta')
        |> List.foldn (height - 1) (fun delta' y -> Map.add (0, y + 1) (delta'.[0, y] + map.[0, y + 1]) delta')

    [1 .. width - 1]
    |> List.fold (fun outer x ->
        [1 .. height - 1]
        |> List.fold (fun (inner: Map<int * int, int>) y ->
            let value = [inner.[x - 1, y]; inner.[x, y-1]] |> List.min 
            Map.add (x, y) (value + map.[x, y]) inner) outer) init

let render (map: Map<int * int, int>) width height =
    [ 0 .. height - 1 ]
    |> List.iter (fun y ->
        [0 .. width - 1]
        |> List.map (fun x -> string map.[x, y])
        |> List.reduce (+)
        |> printfn "%s")
    
let part1 =
    let delta' = delta width height map
    delta'.[width - 1, height - 1] - map.[0, 0]

let part2 =
    let width', height' = 5 * width, 5 * height
    let map' = expand map
//    render map' width' height'

    let delta' = delta width' height' map'
    delta'.[width' - 1, height' - 1] - map.[0, 0]
    
[<EntryPoint>]
let main _ =
    printfn $"Part 1: %i{part1}"
    printfn $"Part 2: %i{part2}"
    0