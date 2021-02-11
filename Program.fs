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
            let labels =
                data.Columns.[colNames.[(nCol - 7)..(nCol - 1)]]
            printfn "%dx%d %A" labels.RowCount labels.ColumnCount (labels.GetRowAt(0))
        0
    | _ -> 1