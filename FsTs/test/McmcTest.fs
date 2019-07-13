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
        let sampleSize = 500
        let maxVariance = 5.
        let my = ContinuousUniform.Sample(0., 5.)
        let std = ContinuousUniform.Sample(1e-1, maxVariance)
        let x = Normal.Samples(my, std) |> Seq.take sampleSize |> Seq.toArray

        // Estimate prior based on some subset of data.
        let xSub = x |> Seq.take (int(float(sampleSize) * 0.25))
        let priorMy = 
            xSub
            |> Statistics.Mean

        // Model to fit data to.
        let normalModel =
            let myPrior = NormalDistribution(priorMy, maxVariance) :> IConditionalDistribution
            let stdPrior = NormalDistribution(maxVariance / 2., maxVariance) :> IConditionalDistribution
            let priors = [| myPrior; stdPrior |]

            { new IModel with
                member __.Likelihood theta x =
                    let flooredVar = max theta.[1] 1e-6
                    let nd = Normal(theta.[0], flooredVar)
                    nd.Density(x)

                member __.PriorDensities theta =
                    theta
                    |> Array.zip priors
                    |> Array.map (fun (p, t) -> p.Density(t))
            }

        // Proposal distributions used to sample new candidate model parameters in the markov kernel.
        let pDists = [| 
            NormalDistribution(0., 0.01) :> IConditionalDistribution
            NormalDistribution(0., 0.01) :> IConditionalDistribution
        |]

        // Get a sequence of parameters and compute statistics from the tail of it.
        let paramSeq = Mcmc.fitModel x normalModel pDists [| priorMy; maxVariance/2. |]
        let paramEstimates = 
            paramSeq 
            |> Seq.take 200_000
            |> Seq.toArray
        let myEstimates = 
            paramEstimates 
            |> Seq.skip 190_000
            |> Seq.map (fun theta -> theta.[0])
        let stdEstimates = 
            paramEstimates 
            |> Seq.skip 190_000
            |> Seq.map (fun theta -> theta.[1])
        let myMean = Statistics.Mean myEstimates
        let stdMean = Statistics.Mean stdEstimates
        let myStd = Statistics.Variance myEstimates
        let stdStd = Statistics.Variance stdEstimates
        
        // Then the means are well within our tolerated range
        let tol = 0.15
        Assert.InRange(myMean, my - my*tol, my + my*tol)
        Assert.InRange(stdMean, std - std*tol, std + std*tol)

        Assert.True(myStd < 1.)
        Assert.True(stdStd < 1.)
