module day08.Extensions

module String =
    let split (sep: string) (input: string) = input.Split(sep) |> List.ofArray
        
module List =        
    let rec distribute e = function
      | [] -> [[e]]
      | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

    let rec permute = function
      | [] -> [[]]
      | e::xs -> List.collect (distribute e) (permute xs)

    let unpack2 list =
        match list with
        | [ a; b ] -> a, b
        | _ -> failwithf $"Tried to unpack2 list without exactly 2 elements: %A{list}"

    let lengthBy predicate list =
        list
        |> List.filter predicate
        |> List.length
        
    let all predicate list =
        list
        |> List.filter predicate
        |> List.length
        |> (=) (List.length list)        