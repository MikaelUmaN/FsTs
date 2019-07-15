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
    |> Array.skip 190_000
    |> Array.map (fun theta -> theta.[0])
let stdEstimates = 
    paramEstimates 
    |> Array.skip 190_000
    |> Array.map (fun theta -> theta.[1])

type ThetaTraceCsv = 
    CsvProvider<Sample = "Iteration, My, Sigma\r\n1, 2.2, 3.3",
                HasHeaders = true,
                Schema = "Iteration (int), My (float), Sigma (float)">

let myRows =
    myEstimates
    |> Array.zip stdEstimates
    |> Array.mapi (fun i (stdEst, myEst) -> ThetaTraceCsv.Row(i, myEst, stdEst))

let myCsv = new ThetaTraceCsv(myRows)
let path = Path.Combine(__SOURCE_DIRECTORY__, "theta.csv")
File.WriteAllText(path, myCsv.SaveToString())
