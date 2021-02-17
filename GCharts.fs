module GCharts

open MathNet.Numerics.Random
open XPlot.GoogleCharts

let googleChartDemo () =
    let rng = SystemRandomSource.Default
    let xy1 = [for i in 1.0..10.0 do i, i * rng.NextDouble()]
    let xy2 = [for i in 1.0..10.0 do i, i + rng.NextDouble()]
    Chart.Scatter(seq {
        xy1
        xy2
    })
    |> Chart.WithOptions(
        Options(
            trendlines = [|Trendline(); Trendline()|]
        )
    )
    |> Chart.Show