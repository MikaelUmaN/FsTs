﻿#I __SOURCE_DIRECTORY__
#I @"bin\debug\netcoreapp2.2"
#I @"C:\Users\mikae\.nuget\packages"

#r "FsTs.dll"
#r @"mathnet.numerics\4.8.1\lib\netstandard2.0\MathNet.Numerics.dll"
#r @"mathnet.numerics.fsharp\4.8.1\lib\netstandard2.0\MathNet.Numerics.FSharp.dll"
#r @"fsharp.data\3.1.1\lib\netstandard2.0\FSharp.Data.dll"

open System
open System.IO
open FSharp.Data
open FsTs
open FsTs.Model
open FsTs.Distributions
open MathNet.Numerics.Distributions
open MathNet.Numerics.Statistics

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

    let priorSigma = targetSigma + 0.1
    let model = Model.normalVarianceModel my <| NormalDistribution(my, priorSigma)

    // Get a sequence of parameters and compute statistics from the tail of it.
    let pDist = independentMultivariateProposalDistribution [| NormalDistribution(0., 0.01) |]
    let thetaSamples = Mcmc.mcmcMh x model pDist [| priorSigma |] 1000 5 2000
    thetaSamples 
    |> Array.map (fun theta -> theta.[0])
