namespace FsTs

open System

module Sampling =

    let rnd = Random()

    let randomInfiniteSeq (data: float[]) =
        Seq.initInfinite (fun i -> data.[rnd.Next(Array.length data)])

    /// Takes every nth element of the data generating process.
    /// The input sequence is assumed to be infinite (or used with caution).
    let takeEveryNthInfinite (data: seq<float>) n startWithFirst =
        Seq.initInfinite (fun i ->
            let d = data |> Seq.head
            let dd = data |> Seq.take (n-1)
            if startWithFirst then d else Seq.last dd
        )

    /// Takes every nth element of the array.
    let takeEveryNth (data: float[]) n startWithFirst =
        let moduloNr = if startWithFirst then 0 else n
        data
        |> Array.mapi (fun i s -> (i % n = moduloNr, s))
        |> Array.filter (fun (i, _) -> i)
        |> Array.map (fun (_, s) -> s)
