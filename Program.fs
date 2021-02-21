// This demo uses the data set found at the URL below.
// https://archive.ics.uci.edu/ml/datasets/HCV+data
open Accord.Statistics.Models.Regression.Fitting
open Accord.Statistics.Models.Regression
open Accord.MachineLearning.VectorMachines
open Accord.MachineLearning.VectorMachines.Learning
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

let readData dataCsvFileName =
    Frame.ReadCsv(dataCsvFileName, separators=",", inferTypes=true)
    |> Frame.indexRowsOrdinally

let factorToBinaryOutcome factor =
    let binOutcome =
        [
            ("0=Blood Donor", 0.0);
            ("0s=suspect Blood Donor", 0.0);
            ("1=Hepatitis", 1.0);
            ("2=Fibrosis", 1.0);
            ("3=Cirrhosis", 1.0)
        ] |> Map.ofList
    factor
    |> Series.mapValues (fun v ->
        match binOutcome.TryFind v with
        | None -> failwith (sprintf "Unknown category: %s" v)
        | Some(code) -> code
    )
    |> Series.values
    |> Seq.toArray

// http://accord-framework.net/docs/html/T_Accord_Statistics_Models_Regression_LogisticRegression.htm
let fitModel (y: float []) (x: float [] []) =
    assert (y.Length = x.Length)
    assert (x.Length > 0)
    let rowLengths =
        x
        |> Array.map (fun row -> row.Length)
        |> Array.distinct
    assert (rowLengths.Length = 1)
    (*
    let learner = LogisticRegression()
    learner.NumberOfInputs <- x.[0].Length
    let teacher = IterativeReweightedLeastSquares(learner)
    let model = teacher.Learn(x, y)
    *)
    let teacher =
        ProbabilisticDualCoordinateDescent(
            Tolerance = 1e-10,
            Complexity = 1e10
        )
    let svm = teacher.Learn(x, y)
    svm (*
    let learner =
        IterativeReweightedLeastSquares<LogisticRegression>(
            Tolerance = 1e-4,
            Iterations = 100,
            Regularization = 0.0
        )
    let model = learner.Learn(x, y)
    x.[0]
    |> Array.mapi (fun i _ -> model.GetCoefficient (int32 i))
    |> Array.append [|model.Intercept|]
    *)

[<EntryPoint>]
let main argv =
    match argv with
    | [|"-m"; dataCsvFileName|] ->
        let data = readData dataCsvFileName
        let y =
            factorToBinaryOutcome (data |> Frame.getCol "Category")
        let nCols =
            data
            |> Frame.getNumericCols
            |> Series.countKeys
        let x =
            data
            |> Frame.getNumericCols
            |> Frame.ofColumns
            |> Frame.getRows
            |> Series.values
            |> Seq.map (Series.values >> Seq.toArray)
            |> Seq.mapi (fun i v ->
                printfn "%d has %d/%d: %A" i v.Length nCols v
                v
            )
            |> Seq.toArray
        let coefs = fitModel y x
        0
    | [|dataCsvFileName|] ->
        let data = readData dataCsvFileName
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
