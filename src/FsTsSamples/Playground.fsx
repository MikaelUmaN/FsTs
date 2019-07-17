#load "FitNormalModels.fsx"

#I __SOURCE_DIRECTORY__
#I @"bin\debug\netcoreapp2.2"
#I @"C:\Users\mikae\.nuget\packages"

//#r "FsTs.dll"
//#r @"mathnet.numerics\4.8.1\lib\netstandard2.0\MathNet.Numerics.dll"
//#r @"mathnet.numerics.fsharp\4.8.1\lib\netstandard2.0\MathNet.Numerics.FSharp.dll"
#r @"fsharp.data\3.1.1\lib\netstandard2.0\FSharp.Data.dll"

open System
open System.IO
open FSharp.Data

let myEstimates = FitNormalModels.fitNormalMeanModel 10. 1.
type ThetaTraceCsv = 
    CsvProvider<Sample = "Iteration, Theta\r\n1, 2.2",
                HasHeaders = true,
                Schema = "Iteration (int), Theta (float)">
let myRows =
    myEstimates
    |> Array.mapi (fun i thetaEst -> ThetaTraceCsv.Row(i, thetaEst))
let myCsv = ThetaTraceCsv(myRows)
let path = Path.Combine(__SOURCE_DIRECTORY__, "myTheta.csv")
File.WriteAllText(path, myCsv.SaveToString())

let sigmaEstimates = FitNormalModels.fitNormalVarianceModel 10. 2.
let sigmaRows =
    sigmaEstimates
    |> Array.mapi (fun i thetaEst -> ThetaTraceCsv.Row(i, thetaEst))
let sigmaCsv = new ThetaTraceCsv(sigmaRows)
let path = Path.Combine(__SOURCE_DIRECTORY__, "sigmaTheta.csv")
File.WriteAllText(path, sigmaCsv.SaveToString())