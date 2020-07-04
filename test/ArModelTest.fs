namespace FsTs.Test

open System
open FsTs
open FsTs.Model
open FsTs.TimeSeriesModel
open FsTs.Distributions
open Xunit
open MathNet.Numerics.Distributions
open MathNet.Numerics.Statistics

type ARModelTest() =

    // Just try to fit the phi parameter - all else is known.
    [<Fact>]
    member __.FitSimpleAR1ModelTest() =
        let c = 0.003
        let phis = [| 0.6 |]
        let std = 0.3 * Math.Sqrt(1./250.)

        let z() = Normal.Sample(0., std)

        let sampleSize = 2000
        let x = 
            arSamples c phis z
            |> Seq.take (sampleSize)
            |> Seq.toArray
        let empiricalMy = Statistics.Mean x
        let empiricalStd = Statistics.Variance x |> sqrt

        let model = TimeSeriesModel.arModel1 phis.Length c std [|
            G(2., 2.) :> FsTsIDistribution // Phi_1, some autocorrelation
        |]

        // Get a sequence of parameters and compute statistics from the tail of it.
        let pDist = independentMultivariateProposalDistribution [| 
            NormalDistribution(0., 0.01)
        |]
        let thetaSamples = Mcmc.mcmcMh x model pDist [| 0.1 |] 1000 1 1000
        let phi1Estimates =
            thetaSamples 
            |> Array.map (fun theta -> theta.[0])

        let phi1Mean = Statistics.Mean phi1Estimates
        let phi1Std = Statistics.Variance phi1Estimates |> sqrt
   
        Assert.True(true)


    [<Fact>]
    member __.FitAR1ModelTest() =
        let c = 0.003
        let phis = [| 0.3 |]
        let std = 0.3 * Math.Sqrt(1./250.)

        let z() = Normal.Sample(0., std)

        let sampleSize = 2000
        let x = 
            arSamples c phis z
            |> Seq.take (sampleSize)
            |> Seq.toArray
        let empiricalMy = Statistics.Mean x
        let empiricalStd = Statistics.Variance x |> sqrt

        let model = TimeSeriesModel.arModel phis.Length [|
            N(0., 1.) :> FsTsIDistribution // Expect c small
            G(2., 2.) :> FsTsIDistribution // Phi_1, some autocorrelation
            G(2., 2.) :> FsTsIDistribution // Daily std, small.
        |]

        // Get a sequence of parameters and compute statistics from the tail of it.
        let pDist = independentMultivariateProposalDistribution [| 
            NormalDistribution(0., 0.01)
            NormalDistribution(0., 0.01)
            NormalDistribution(0., 0.01)
        |]
        let thetaSamples = Mcmc.mcmcMh x model pDist [| 0.; 0.1; 1. |] 1000 1 1000
        let cEstimates =
            thetaSamples 
            |> Array.map (fun theta -> theta.[0])
        let phi1Estimates =
            thetaSamples 
            |> Array.map (fun theta -> theta.[1])
        let stdEstimates =
            thetaSamples 
            |> Array.map (fun theta -> theta.[2])

        let cMean = Statistics.Mean cEstimates
        let cStd = Statistics.Variance cEstimates |> sqrt
        let phi1Mean = Statistics.Mean phi1Estimates
        let phi1Std = Statistics.Variance phi1Estimates |> sqrt
        let stdMean = Statistics.Mean stdEstimates
        let stdStd = Statistics.Variance stdEstimates |> sqrt
       
        Assert.True(true)

  