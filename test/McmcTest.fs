namespace FsTs.Test

open FsTs
open FsTs.Model
open FsTs.Distributions
open Xunit
open MathNet.Numerics.Distributions
open MathNet.Numerics.Statistics

type FittingTest() =

    [<Fact>]
    member __.FitNormalMeanTest() =

        // Data to fit.
        let sampleSize = 500
        let targetMy = 100.5
        let sigma = 1.
        let x = 
            Normal.Samples(targetMy, sigma) 
            |> Seq.take sampleSize 
            |> Seq.toArray

        let priorMy = targetMy - 0.5
        let model = Model.normalMeanModel sigma <| NormalDistribution(priorMy, sigma)

        // Get a sequence of parameters and compute statistics from the tail of it.
        let pDist = independentMultivariateProposalDistribution [| NormalDistribution(0., 0.01) |]
        let thetaSamples = Mcmc.mcmcMh x model pDist [| priorMy |] 1000 5 2000
        let myEstimates = 
            thetaSamples 
            |> Array.map (fun theta -> theta.[0])
        let myMean = Statistics.Mean myEstimates
        let myStd = sqrt <| Statistics.Variance myEstimates
           
        // Then the means are well within our tolerated range
        let tol = 0.15
        Assert.InRange(myMean, targetMy - targetMy*tol, targetMy + targetMy*tol)
        Assert.True(myStd < 1.)
