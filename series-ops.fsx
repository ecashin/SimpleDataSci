// dotnet fsi series-ops.fsx

#r "bin/Debug/net5.0/Deedle.dll"

open Deedle

let a =
    seq { 1..10 }
    |> Seq.map (fun i -> i, float (i * 10))
printfn "a: %A" a

let b = Series.ofObservations a
printfn "b: %A" b

printfn "b + 1.0: %A" (b + 1.0)

printfn "b * b: %A" (b * b)

let df =
    Frame.ofColumns ["b" => b; "b+1.0" => (b + 1.0); "b*b" => (b * b)]

printfn "df row sums: %A" (df?b + df?``b+1.0`` + df?``b*b``)
