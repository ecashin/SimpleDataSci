// https://archive.ics.uci.edu/ml/datasets/Amphibians
// The "dummy variables" in Amphibians data are not really dummy variables
// but non-categorical boolean indicators.
// https://archive.ics.uci.edu/ml/datasets/HCV+data
open MathNet.Numerics.Random
open Deedle
open XPlot.Plotly

let shuffleOrder (keys: seq<int>) =
    // inspred by
    // http://www.fssnip.net/kS/title/Sample-for-traintest-sets-in-Deedle-Frames
    let rng = SystemRandomSource.Default
    keys
    |> Seq.map (fun key -> (key, rng.Next()))
    |> Seq.sortBy snd
    |> Seq.map fst

let sampleKeys (n: int) (keys: seq<int>) =
    shuffleOrder keys |> Seq.take n

let rebalance (catColumn: string) (catCounts: Series<string,int>) (data: Frame<int,string>) =
    let n = Stats.min catCounts |> int
    let balanced =
        catCounts.Keys
        |> Seq.collect (fun label ->
            let catData =
                data
                |> Frame.filterRowsBy catColumn label
            let order = sampleKeys n catData.RowKeys
            let sampled =
                catData
                |> Frame.sliceRows order
                |> (fun df -> df.GetRows() |> Series.values)
            sampled
        )
        |> Series.ofValues
        |> Frame.ofRows
    balanced

// You can do this functionally, figuring out how to avoid
// tuples with duplicate values or tuples that are duplicates
// when swapped, but the sequence comprehension syntax
// using an array is *really* easy for me to work with here.
let variablePairs vars =
    let a = vars |> Seq.toArray
    seq {
        for i in 0..(a.Length - 2) do
            for j in (i + 1)..(a.Length - 1) do
                (a.[i], a.[j])
    }

let plotDemo labelCol (df: Frame<_,_>) =
    let indepValueCols =
        df.ColumnKeys
        |> Seq.filter (fun col -> col <> labelCol)
    let yCol = Seq.head indepValueCols
    let makeTrace name xCol yCol =
        let x = xCol |> Series.values
        let y = yCol |> Series.values
        Scatter(
            x = x,
            y = y,
            mode = "markers",
            name = name
        )
    variablePairs indepValueCols
        |> Seq.map (fun (x, y) ->
        let traces =
            df
            |> Frame.groupRowsByString labelCol
            |> Frame.nest
            |> Series.map (fun k v -> makeTrace k (v.GetColumn(x)) (v.GetColumn(y)))
            |> Series.values
        traces
        |> Chart.Plot
        |> Chart.WithTitle (sprintf "%s ~ %s" y x)
    )
    |> Chart.ShowAll

// inspired by
// https://github.com/fslaborg/Deedle/blob/master/tests/Deedle.Tests/Frame.fs#L1420
let catReps (category:string) df =
    df
    |> Frame.groupRowsByString category
    |> Frame.nest
    |> Series.mapValues Frame.countRows

[<EntryPoint>]
let main argv =
    match argv with
    | [| |] ->
        GCharts.googleChartDemo () |> ignore
        0
    | [|dataCsvFileName|] ->
        let data =
            Frame.ReadCsv(dataCsvFileName, separators=",", inferTypes=true)
            |> Frame.indexRowsOrdinally
        let nRow, nCol = data.RowCount, data.ColumnCount
        printfn "number of rows:%d, cols:%d" nRow nCol
        if nRow > 0 then
            printfn "first row: %A" (data.GetRowAt 0)
            let catSums = data |> catReps "Category"
            // Below it shows the data isn't balanced.
            printfn "initial category representative counts: %A" catSums
            let balanced =
                data
                |> rebalance "Category" catSums
            balanced.SaveCsv("balanced.csv")
            printfn "balanced category representative counts: %A" (catReps "Category" balanced)
            plotDemo "Category" balanced
        0
    | _ -> 1
