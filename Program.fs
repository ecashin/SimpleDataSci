// https://archive.ics.uci.edu/ml/datasets/Amphibians
open System
open Deedle

[<EntryPoint>]
let main argv =
    match argv with
    | [|dataCsvFileName|] ->
        let data = Frame.ReadCsv(dataCsvFileName, separators=";", inferTypes=true)
        let nRow, nCol = data.RowCount, data.ColumnCount
        printfn "number of rows:%d, cols:%d" nRow nCol
        if data.RowCount > 0 then
            printfn "first row: %A" (data.GetRowAt 0)
            let colNames =
                data.ColumnKeys
                |> Seq.toArray
            // There are seven categories, each with a dummy variable.
            // We have to convert inferred boolean type to int in order
            // to sum the values.
            let labels =
                data.Columns.[colNames.[(nCol - 7)..(nCol - 1)]]
                |> Frame.mapColValues (fun c -> c.As<int>())
            printfn "%dx%d %A" labels.RowCount labels.ColumnCount (labels.GetRowAt(0))
            let catSums =
                labels
                |> Stats.sum
            // Below it shows the data isn't balanced.
            printfn "category representative counts: %A" catSums
        0
    | _ -> 1