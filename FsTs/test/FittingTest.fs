namespace FsTs.Test

open FsTs
open FsTs.Fitting
open Xunit
open MathNet.Numerics.Distributions

type FittingTest() =

    [<Fact>]
    member __.FitNormalTest() =

        // Distribution to fit.
        let my = 1.2
        let std = 1.5
        let x = Normal.Samples(my, std) |> Seq.take 1000 |> Seq.toArray

        // Sample size 250 works well, but size 500 -> explodes and stays at 0, 1...
        // Probably need to work with log

        // TODO: this will give back just a sample from the posterior distribution,
        // of course we want the sequence from which we can sample from the
        // posterior distribution and calculate mean, moments and so on.
        let (myFit, stdFit) = Fitting.fitNormal x

        Assert.InRange(myFit, my - my*0.3, my + my*0.3)
        Assert.InRange(stdFit, std - std*0.3, std + std*0.3)