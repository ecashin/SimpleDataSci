// https://archive.ics.uci.edu/ml/datasets/Amphibians
// The "dummy variables" in Amphibians data are not really dummy variables
// but non-categorical boolean indicators.
// https://archive.ics.uci.edu/ml/datasets/HCV+data
open MathNet.Numerics.Random
open Deedle

let shuffleOrder n =
    // inspred by
    // http://www.fssnip.net/kS/title/Sample-for-traintest-sets-in-Deedle-Frames
    let rng = SystemRandomSource.Default
    seq { 1..n }
    |> Seq.map (fun i -> (i, rng.Next()))
    |> Seq.sortBy snd
    |> Seq.map fst

let rebalance (catSums: Series<string,float>) (data: Frame<int,string>) =
    let n = Stats.min catSums |> int
    let order: int list =
        shuffleOrder data.RowCount
        |> Seq.take n
        |> Seq.toList
    let balanced =
        catSums.Keys
        |> Seq.collect (fun label ->
            data
            |> Frame.filterRowsBy label 1
            |> Frame.sliceRows order
            |> (fun df -> df.GetRows() |> Series.values)
        )
        |> Series.ofValues
        |> Frame.ofRows
    printfn "balanced %A has rows x cols: %dx%d" balanced balanced.RowCount balanced.ColumnCount
    balanced

[<EntryPoint>]
let main argv =
    match argv with
    | [|dataCsvFileName|] ->
        let data =
            Frame.ReadCsv(dataCsvFileName, separators=",", inferTypes=true)
            |> Frame.indexRowsOrdinally
        let nRow, nCol = data.RowCount, data.ColumnCount
        printfn "number of rows:%d, cols:%d" nRow nCol
        if data.RowCount > 0 then
            printfn "first row: %A" (data.GetRowAt 0)
            let catSums =
                data
                |> Frame.groupRowsByString "Category"
                |> Frame.getRows
                |> Stats.levelCount fst
            // Below it shows the data isn't balanced.
            printfn "initial category representative counts: %A" catSums
        0
    | _ -> 1
    