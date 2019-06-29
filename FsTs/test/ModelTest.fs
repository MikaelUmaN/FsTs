namespace FsTs.Test

open FsTs
open FsTs.Model
open Xunit
open MathNet.Numerics.Distributions

type ModelTest() =

    [<Fact>]
    member __.ModelCompositionTest() =

        // Given a combined ARMA model
        let arOrder = 1
        let maOrder = 1
        let ar = AR 1
        let ma = MA 1
        let arma = Composition(ar, ma)

        // Then we can pattern match it.
        match arma with
        | Composition(AR p, MA q) -> 
            Assert.Equal(arOrder, p)
            Assert.Equal(maOrder, q)
        | _ -> failwith "failed to match"

    [<Fact>]
    member __.ARValidationTest() =
        
        // An AR p model must contain enough parameters
        let createModel () = 
            let ar = ARModel(1, 0., [| 0.2; 0.3 |])
            ()

        // Else exception is thrown
        let ex = Assert.ThrowsAny(createModel)
        Assert.True(ex.Message.Length > 0) // Give description of error

    [<Fact>]
    member __.AR1Test() =

        // Given an AR1 model
        let ar1 = ARModel(1, 0., [| 0.1 |]) :> IModel
        
        // Then we can sample from it any number of samples we require.
        let n = 5
        let samples = 
            ar1.Samples 
            |> Seq.take n
            |> Seq.toList

        Assert.Equal(n, Seq.length samples)

    [<Fact>]
    member __.AR1MeanTest() =
        
        // Given an AR1 model with given mean
        let my = 20.
        let ar1 = ARModel(1, my, [| 0.1 |]) :> IModel
        let longTermSample = ar1.Samples |> Seq.take 100_000 |> Seq.last

        // Then the long term sample stays within reasonable quantiles.
        let rangeDiff = Normal.InvCDF(0., 1., 1e-8)

        Assert.InRange(longTermSample, my + rangeDiff, my - rangeDiff)

    [<Fact>]
    member __.ARPTest() =
        
        // Given an ARP model.
        let p = 4
        let phis = [| 0.03; 0.06; 0.1; 0.5 |]
        let arp = ARModel(p, 0., phis) :> IModel

        let sampleCount = 5
        let samples = arp.Samples |> Seq.take sampleCount |> Seq.toArray

        Assert.Equal(sampleCount, samples.Length)
