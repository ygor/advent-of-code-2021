open System
open System.IO
open MathNet.Numerics.LinearAlgebra

let report =
    File.ReadAllLines("input.txt")
    |> Seq.map (Seq.map (string >> float))
    |> matrix

let commonValue (column: float seq) selector =
    column
    |> Seq.groupBy id
    |> selector (fun (_, values) -> Seq.length values)
    |> fst
    
let mostCommonValue (column: float seq) =
    commonValue column Seq.maxBy

let leastCommonValue (column: float seq) =
    commonValue column Seq.minBy

let commonBits valueFn (report: Matrix<float>) =
    report
    |> Matrix.toColSeq
    |> Seq.map (valueFn >> string)
    |> Seq.reduce (+)
    |> (fun value -> Convert.ToInt32(value, 2))

let powerConsumption =
    commonBits mostCommonValue report * commonBits leastCommonValue report 

let rate valueSelector (report: Matrix<float>) =
    report
    |> Matrix.toColSeqi 
    |> Seq.fold (fun (report': Matrix<float>) (i, _) ->
        if report'.RowCount = 1 then report'
        else
            let column = report'.Column(i)
            let most, least = mostCommonValue column, leastCommonValue column                
            report'
            |> Matrix.toRowSeq
            |> Seq.filter (fun row -> row.At(i) = valueSelector most least)
            |> matrix) report
    |> Matrix.toRowSeq
    |> Seq.head
    |> Seq.map string
    |> Seq.reduce (+)
    |> (fun value -> Convert.ToInt32(value, 2))

let oxygen =
    rate (fun most least -> if most = least then 1.0 else most)

let scrubbing =
    rate (fun most least -> if most = least then 0.0 else least)     
    
[<EntryPoint>]
let main _ =
    printfn $"Part 1: %i{powerConsumption}"
    printfn $"Part 2: %i{scrubbing report * oxygen report}"
    0