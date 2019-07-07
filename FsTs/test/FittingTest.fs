namespace FsTs.Test

open FsTs
open FsTs.Model
open FsTs.Distributions
open Xunit
open MathNet.Numerics.Distributions
open MathNet.Numerics.Statistics

type FittingTest() =

    [<Fact>]
    member __.FitNormalTest() =

        // Data to fit.
        let my = ContinuousUniform.Sample(0., 5.)
        let std = ContinuousUniform.Sample(1e-1, 5.)
        let x = Normal.Samples(my, std) |> Seq.take 1000 |> Seq.toArray

        // Model to fit data to.
        let normalModel =
            { new IModel with
                member __.Density theta x =
                    let nd = Normal(theta.[0], theta.[1])
                    nd.Density(x)
            }

        // Proposal distributions used to sample new candidate model parameters.
        let pDists = [| 
            NormalProposalDistribution(0., 5.) :> IProposalDistribution
            ContinuousUniformProposalDistribution(1e-2, 10.) :> IProposalDistribution
        |]

        // Get a sequence of parameters and compute statistics from the tail of it.
        let paramSeq = Fitting.fitModel(x, normalModel, pDists, [| 0.; 1e-2 |])
        let paramEstimates = 
            paramSeq 
            |> Seq.take 10_000
            |> Seq.toArray
        let myEstimates = 
            paramEstimates 
            |> Seq.skip 9_000
            |> Seq.map (fun theta -> theta.[0])
        let stdEstimates = 
            paramEstimates 
            |> Seq.skip 9_000
            |> Seq.map (fun theta -> theta.[1])
        let myMean = Statistics.Mean myEstimates
        let stdMean = Statistics.Mean stdEstimates
        let myStd = Statistics.Variance myEstimates
        let stdStd = Statistics.Variance stdEstimates
        
        // Then the means are well within our tolerated range
        let tol = 0.15
        Assert.InRange(myMean, my - my*tol, my + my*tol)
        Assert.InRange(stdMean, std - std*tol, std + std*tol)

        // And the variance of parameter estimates should be limited.
        Assert.True(myStd < 1e-8)
        Assert.True(stdStd < 1e-8)