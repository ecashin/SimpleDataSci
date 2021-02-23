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
