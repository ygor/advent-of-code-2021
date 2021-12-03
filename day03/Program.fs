open System
open System.IO
open MathNet.Numerics.LinearAlgebra

let report =
    File.ReadAllLines("input.txt")
    |> Seq.map (Seq.map (string >> float))
    |> matrix
    |> Matrix.toColSeq

let commonBits (report: Vector<float> seq) predicate =
    report
    |> Seq.map (fun col ->
        if (predicate (Vector.sum col) (float (Vector.length col) / 2.0)) then "1" else "0")
    |> Seq.reduce (+)

let part1 =
    let gamma = commonBits report (>)
    let epsilon = commonBits report (<)
    Convert.ToInt32(gamma, 2) * Convert.ToInt32(epsilon, 2)
    
[<EntryPoint>]
let main _ =
    printfn $"Part 1: %i{part1}"
    0