module day09.Extensions

let (><) f a b = f b a

module String =
    let split (sep: string) (input: string) = input.Split(sep) |> List.ofArray

module Set =
    let all predicate set =
        set
        |> Set.filter predicate
        |> Set.count
        |> (=) (Set.count set)
        