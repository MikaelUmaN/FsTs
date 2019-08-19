namespace FsTs.Test

open FsTs
open FsTs.Model
open FsTs.Distributions
open Xunit
open MathNet.Numerics.Distributions
open MathNet.Numerics.Statistics

type NormalModelsTest() =

    [<Fact>]
    member __.FitNormalModelTest() =
        let targetSigma = 2.
        let targetMy = 100.5
        let sampleSize = 1000
        let x = 
            Normal.Samples(targetMy, targetSigma) 
            |> Seq.take sampleSize 
            |> Seq.toArray
        let empiricalMy = Statistics.Mean x
        let empiricalSigma = Statistics.Variance x |> sqrt

        let priorSigma = targetSigma + 0.1
        let priorMy = targetMy - 0.5
        let model = Model.normalModel [| 
            NormalDistribution(priorMy, 10.) :> FsTsIDistribution
            UniformDistribution(1e-2, 5.) :> FsTsIDistribution 
        |]

        // Get a sequence of parameters and compute statistics from the tail of it.
        let pDist = independentMultivariateProposalDistribution [| NormalDistribution(0., 0.01); NormalDistribution(0., 0.01) |]
        let thetaSamples = Mcmc.mcmcMh x model pDist [| priorMy; priorSigma |] 1000 1 1000
        let myEstimates =
            thetaSamples 
            |> Array.map (fun theta -> theta.[0])
        let sigmaEstimates =
            thetaSamples 
            |> Array.map (fun theta -> theta.[1])

        let myMean = Statistics.Mean myEstimates
        let myStd = Statistics.Variance myEstimates |> sqrt
        let sigmaMean = Statistics.Mean sigmaEstimates
        let sigmaStd = Statistics.Variance sigmaEstimates |> sqrt
       
        // Then the means are well within our tolerated range
        let tol = 0.15

        Assert.InRange(myMean, empiricalMy - empiricalMy*tol, empiricalMy + empiricalMy*tol)
        Assert.True(myStd < 1.)

        Assert.InRange(sigmaMean, empiricalSigma - empiricalSigma*tol, empiricalSigma + empiricalSigma*tol)
        Assert.True(sigmaStd < 1.)

    [<Fact>]
    member __.FitNormalVarianceTest() =
        let targetSigma = 2.
        let my = 10.
        let sampleSize = 1000
        let x = 
            Normal.Samples(my, targetSigma) 
            |> Seq.take sampleSize 
            |> Seq.toArray
        let empiricalMy = Statistics.Mean x
        let empiricalSigma = Statistics.Variance x |> sqrt

        let priorSigma = targetSigma + 0.1
        let model = UniformDistribution(1e-2, 5.) |> Model.normalVarianceModel empiricalMy

        // Get a sequence of parameters and compute statistics from the tail of it.
        let pDist = independentMultivariateProposalDistribution [| NormalDistribution(0., 0.01) |]
        let thetaSamples = Mcmc.mcmcMh x model pDist [| priorSigma |] 1000 1 1000
        let sigmaEstimates =
            thetaSamples 
            |> Array.map (fun theta -> theta.[0])

        let sigmaMean = Statistics.Mean sigmaEstimates
        let sigmaStd = Statistics.Variance sigmaEstimates |> sqrt
           
        // Then the means are well within our tolerated range
        let tol = 0.15
        Assert.InRange(sigmaMean, empiricalSigma - empiricalSigma*tol, empiricalSigma + empiricalSigma*tol)
        Assert.True(sigmaStd < 1.)

    [<Fact>]
    member __.FitNormalVarianceUsingNonFlatPriorTest() =
        let targetSigma = 2.
        let my = 10.
        let sampleSize = 1000
        let x = 
            Normal.Samples(my, targetSigma) 
            |> Seq.take sampleSize 
            |> Seq.toArray
        let empiricalMy = Statistics.Mean x
        let empiricalSigma = Statistics.Variance x |> sqrt

        // Half-Cauchy means we ignore anything < 0
        let priorSigma = targetSigma + 0.5
        let model = CauchyDistribution(priorSigma, 5.) |> Model.normalVarianceModel empiricalMy

        // Get a sequence of parameters and compute statistics from the tail of it.
        let pDist = independentMultivariateProposalDistribution [| NormalDistribution(0., 0.01) |]
        let thetaSamples = Mcmc.mcmcMh x model pDist [| priorSigma |] 1000 1 1000
        let sigmaEstimates =
            thetaSamples 
            |> Array.map (fun theta -> theta.[0])

        let sigmaMean = Statistics.Mean sigmaEstimates
        let sigmaStd = Statistics.Variance sigmaEstimates |> sqrt
       
        // Then the means are well within our tolerated range
        let tol = 0.15
        Assert.InRange(sigmaMean, empiricalSigma - empiricalSigma*tol, empiricalSigma + empiricalSigma*tol)
        Assert.True(sigmaStd < 1.)

    [<Fact>]
    member __.FitNormalMeanTest() =
        let sampleSize = 1000
        let targetMy = 100.5
        let sigma = 1.
        let x = 
            Normal.Samples(targetMy, sigma) 
            |> Seq.take sampleSize 
            |> Seq.toArray
        let empiricalMy = Statistics.Mean x
        let empiricalSigma = Statistics.Variance x |> sqrt

        let priorMy = targetMy - 0.5
        let model =  NormalDistribution(priorMy, 10.) |> Model.normalMeanModel empiricalSigma

        // Get a sequence of parameters and compute statistics from the tail of it.
        let pDist = independentMultivariateProposalDistribution [| NormalDistribution(0., 0.01) |]
        let thetaSamples = Mcmc.mcmcMh x model pDist [| priorMy |] 1000 1 1000
        let myEstimates = 
            thetaSamples 
            |> Array.map (fun theta -> theta.[0])

        let myMean = Statistics.Mean myEstimates
        let myStd = sqrt <| Statistics.Variance myEstimates
           
        // Then the means are well within our tolerated range
        let tol = 0.15
        Assert.InRange(myMean, empiricalMy - empiricalMy*tol, empiricalMy + empiricalMy*tol)
        Assert.True(myStd < 1.)
