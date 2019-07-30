namespace FsTs

open System

module Sampling =

    let rnd = Random()

    let randomInfiniteSeq (data: float[]) =
        Seq.initInfinite (fun i -> data.[rnd.Next(Array.length data)])