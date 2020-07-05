#I __SOURCE_DIRECTORY__
#I @"bin/debug/net5"

#r "FsTs.dll"

open System
open System.IO
open FSharp.Data
open FsTs
open FsTs.Model
open FsTs.Distributions
open MathNet.Numerics.Distributions
open MathNet.Numerics.Statistics

let fitNormalModel targetMy targetSigma burnIn dataSampleSize thinning posteriorSampleSize =
    let sampleSize = dataSampleSize
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
        CauchyDistribution(priorSigma, 5.) :> FsTsIDistribution 
    |]

    // Get a sequence of parameters and compute statistics from the tail of it.
    let pDist = independentMultivariateProposalDistribution [| NormalDistribution(0., 0.1); NormalDistribution(0., 0.1) |]
    let thetaEstimates = Mcmc.mcmcMh x model pDist [| priorMy; priorSigma |] burnIn thinning posteriorSampleSize

    (thetaEstimates, empiricalMy, empiricalSigma)

let fitNormalMeanModel targetMy sigma =
    let sampleSize = 1000
    let x = 
        Normal.Samples(targetMy, sigma) 
        |> Seq.take sampleSize 
        |> Seq.toArray

    let priorMy = targetMy - 0.5
    let model = Model.normalMeanModel sigma <| NormalDistribution(priorMy, sigma)

    // Get a sequence of parameters and compute statistics from the tail of it.
    let pDist = independentMultivariateProposalDistribution [| NormalDistribution(0., 0.01) |]
    let thetaSamples = Mcmc.mcmcMh x model pDist [| priorMy |] 1000 5 2000
    thetaSamples 
    |> Array.map (fun theta -> theta.[0])

let fitNormalVarianceModel my targetSigma =
    let sampleSize = 1000
    let x = 
        Normal.Samples(my, targetSigma) 
        |> Seq.take sampleSize 
        |> Seq.toArray

    let empiricalMy = Statistics.Mean x
    let priorSigma = targetSigma + 0.1
    let model = Model.normalVarianceModel empiricalMy <| UniformDistribution(1e-2, 10.)

    // Get a sequence of parameters and compute statistics from the tail of it.
    let pDist = independentMultivariateProposalDistribution [| NormalDistribution(0., 0.01) |]
    let thetaSamples = Mcmc.mcmcMh x model pDist [| priorSigma |] 1000 5 2000
    thetaSamples 
    |> Array.map (fun theta -> theta.[0])

let fitNormalModelIteratively targetMy targetSigma burnIn dataSampleSize thinning posteriorSampleSize =
    let sampleSize = dataSampleSize
    let x = 
        Normal.Samples(targetMy, targetSigma) 
        |> Seq.take sampleSize 
        |> Seq.toArray
    let empiricalMy = Statistics.Mean x
    let empiricalSigma = Statistics.Variance x |> sqrt

    let priorSigma = targetSigma + 1. // Far off to make it more difficult.
    let priorMy = targetMy - 0.5
    let modelOne = NormalDistribution(priorMy, 10.) |> Model.normalMeanModel priorSigma

    // Get a sequence of parameters and compute statistics from the tail of it.
    let pDist = independentMultivariateProposalDistribution [| NormalDistribution(0., 0.1) |]
    let myEstimates = Mcmc.mcmcMh x modelOne pDist [| priorMy |] burnIn thinning posteriorSampleSize |> Array.map (fun t -> t.[0])
    let myAvg = Statistics.Mean myEstimates

    let modelTwo = UniformDistribution(1e-2, 10.) |> Model.normalVarianceModel myAvg
    let sigmaEstimates = Mcmc.mcmcMh x modelTwo pDist [| priorSigma |] burnIn thinning posteriorSampleSize |> Array.map (fun t -> t.[0])
    let sigmaAvg = Statistics.Mean sigmaEstimates

    let thetaEstimates = Array.init myEstimates.Length (fun i -> [| myEstimates.[i]; sigmaEstimates.[i] |])
    (thetaEstimates, empiricalMy, empiricalSigma)