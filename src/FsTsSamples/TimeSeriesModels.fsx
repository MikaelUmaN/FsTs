#I __SOURCE_DIRECTORY__
#I @"bin/debug/net5"

#r "FsTs.dll"

open System

// To avoid conflicts with mathnet...
open XPlot.Plotly
type Hist = Histogram

open FsTs.TimeSeriesModel
open FsTs.Distributions
open FsTs.Sampling
open MathNet.Numerics.Distributions
open MathNet.Numerics.Statistics

let zt() = sampleZ |> Seq.head

// MA model is normally distributed with the given mean,
// Because autoregression is on the N(0,1) values.
let my = 10.
let thetas = [|0.5; 0.3; 0.01 |]
let xs = maSamples my thetas zt 
let xss = xs |> Seq.take 5000 |> Seq.toArray;

Hist(x = xss)
|> Chart.Plot
|> Chart.Show

// AR model is normally distributed with displaced mean.
// For AR 1 -> my = c/(1-phi).
// 10/0.1 = 100 = my
// Variance 1./(1-0.9^2) -> 5.263157895
let c = 10.
let phis = [| 0.9 |]
let ys = arSamples c phis zt
let yss = 
    ys 
    |> Seq.take 5100
    |> Seq.skip 100 // Need "burn-in" because we don't supply values for X_-1, X_-2, ...
    |> Seq.toArray

Hist(x = yss)
|> Chart.Plot
|> Chart.Show

// ARMA PQ model.
// In this case, P=1, Q=3
let zs = armaSamples c phis thetas zt
let zss = 
    zs 
    |> Seq.take 5100
    |> Seq.skip 100 // Need "burn-in" because we don't supply values for X_-1, X_-2, ...
    |> Seq.toArray

Hist(x = zss)
|> Chart.Plot
|> Chart.Show

// Suppose each Xt is a log-return from minute close prices.
// Suppose the price series starts at some level Px.
// Then we can generate a price chart from ARMA simulated log-returns.
// (Note: here we just accept the probability of prices going negative...)
let pxStart = 100.
let minutesPerYear = 250 * 8 * 60 // 250d * 8h * 60m
let minutesPerYearF = float minutesPerYear
let annualizedVol = 0.3
let minuteVol = annualizedVol / Math.Sqrt(minutesPerYearF)
let z() = Normal.Sample(0., minuteVol)

let rc = 0.0000015 // Small trend upwards
let rphis = [| 0.32; 0.12; 0.003; 0.000064 |]
let rthetas = [| 0.17; 0.06; 0.000055 |]
let retSeq = armaSamples rc rphis rthetas z
let rets =
    retSeq 
    |> Seq.take (minutesPerYear+100)
    |> Seq.skip 100
    |> Seq.toArray

let minutesPerDay = 60 * 8
let dailyChunks = minutesPerYear / minutesPerDay // Obv 250
let dailyRets =
    rets
    |> Array.splitInto dailyChunks
    |> Array.map (fun dr -> dr |> Array.sum) // Summing log rets gives daily returns.

let minuteMean = Statistics.Mean rets
let minuteStd = Statistics.StandardDeviation rets
let dailyMean = Statistics.Mean dailyRets
let dailyStd = Statistics.StandardDeviation dailyRets

// Return charts.
Scatter(x = [0..dailyRets.Length], y = dailyRets)
|> Chart.Plot
|> Chart.Show

Scatter(x = [0..rets.Length], y = rets)
|> Chart.Plot
|> Chart.Show

// Convert to prices.
let (prices,_) =
    rets
    |> Array.mapFold (fun px r -> (px * (1. + r), px * (1. + r))) pxStart

Scatter(x = [0..prices.Length], y = prices)
|> Chart.Plot
|> Chart.Show

let (dailyPrices,_) =
    dailyRets
    |> Array.mapFold (fun px r -> (px * (1. + r), px * (1. + r))) pxStart

Scatter(x = [0..dailyPrices.Length], y = dailyPrices)
|> Chart.Plot
|> Chart.Show
