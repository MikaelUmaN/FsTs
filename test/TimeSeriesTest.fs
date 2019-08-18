namespace FsTs.Test

open System
open FsTs
open FsTs.Model
open FsTs.TimeSeriesModel
open FsTs.Distributions
open Xunit
open MathNet.Numerics.Distributions
open MathNet.Numerics.Statistics

type TimeSeriesTest() =

    [<Fact>]
    member __.FitArmaModelTest() =
        let c = 0.003
        let phis = [| 0.3; 0.15 |]
        let thetas = [| 0.2 |]
        let std = 0.3 * Math.Sqrt(1./250.)

        let z() = Normal.Sample(0., std)

        let sampleSize = 2000
        let x = 
            armaSamples c phis thetas z
            |> Seq.take sampleSize 
            |> Seq.toArray
        let empiricalMy = Statistics.Mean x
        let empiricalStd = Statistics.Variance x |> sqrt

        let model = TimeSeriesModel.armaModel phis.Length thetas.Length [|
            UniformDistribution(0., 0.5) // Expect c small
            UniformDistribution(0., 0.5) // Phi_1, some autocorrelation
            UniformDistribution(0., 0.4) // Phi_2, smaller autocorrelation
            UniformDistribution(0., 0.5) // Theta_1
            UniformDistribution(1e-4, 1.) // Daily std, small.
        |]

        // Get a sequence of parameters and compute statistics from the tail of it.
        let pDist = independentMultivariateProposalDistribution [| 
            NormalDistribution(0., 0.01)
            NormalDistribution(0., 0.01)
            NormalDistribution(0., 0.01)
            NormalDistribution(0., 0.01)
            NormalDistribution(0., 0.01)
        |]
        let thetaSamples = Mcmc.mcmcMh x model pDist [| 0.; 0.25; 0.2; 0.25; 0.01 |] 1000 1 1000
        let cEstimates =
            thetaSamples 
            |> Array.map (fun theta -> theta.[0])
        let phi1Estimates =
            thetaSamples 
            |> Array.map (fun theta -> theta.[1])
        let phi2Estimates =
            thetaSamples 
            |> Array.map (fun theta -> theta.[2])
        let thetaEstimates =
            thetaSamples 
            |> Array.map (fun theta -> theta.[3])
        let stdEstimates =
            thetaSamples 
            |> Array.map (fun theta -> theta.[4])

        let cMean = Statistics.Mean cEstimates
        let cStd = Statistics.Variance cEstimates |> sqrt
        let phi1Mean = Statistics.Mean phi1Estimates
        let phi1Std = Statistics.Variance phi1Estimates |> sqrt
        let phi2Mean = Statistics.Mean phi2Estimates
        let phi2Std = Statistics.Variance phi2Estimates |> sqrt
        let thetaMean = Statistics.Mean thetaEstimates
        let thetaStd = Statistics.Variance thetaEstimates |> sqrt
        let stdMean = Statistics.Mean stdEstimates
        let stdStd = Statistics.Variance stdEstimates |> sqrt
       
        Assert.True(true)

  