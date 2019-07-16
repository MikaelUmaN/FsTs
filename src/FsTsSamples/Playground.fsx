#I __SOURCE_DIRECTORY__
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

// MCMC run
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

type ThetaTraceCsv = 
    CsvProvider<Sample = "Iteration, My\r\n1, 2.2",
                HasHeaders = true,
                Schema = "Iteration (int), My (float)">

let myRows =
    myEstimates
    |> Array.mapi (fun i myEst -> ThetaTraceCsv.Row(i, myEst))

let myCsv = new ThetaTraceCsv(myRows)
let path = Path.Combine(__SOURCE_DIRECTORY__, "theta.csv")
File.WriteAllText(path, myCsv.SaveToString())
