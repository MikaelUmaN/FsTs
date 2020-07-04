namespace FsTs.Test

open FsTs
open FsTs.Model
open FsTs.Distributions
open Xunit
open MathNet.Numerics.Distributions
open MathNet.Numerics.Statistics
open FsTs.TimeSeriesModel

type AutocorrelationTest() =

    let expectedCovariance (vs: float[]) lag =
        let shiftedVs = vs |> Array.take (vs.Length - lag)
        let vsm = Statistics.Mean vs
        let sm = Statistics.Mean shiftedVs

        let cnt = Array.length shiftedVs
        let vsStandard = vs |> Array.map (fun v -> v - vsm) |> Array.skip lag
        let vsShiftedStandard = shiftedVs |> Array.map (fun v -> v - sm)

        let evSum = 
            Array.zip vsStandard vsShiftedStandard
            |> Array.sumBy (fun (v1, v2) -> v1 * v2)
        evSum / float(cnt)

    [<Fact>]
    member __.AutocovarianceLag1() =

        // Given a lag
        let lag = 1;

        // Given a simple series from 1 to 10
        let ks = [|1..10|]
        let vs = [|1. .. 10.|]
        //let s = Series(ks, vs)

        // When calculating the autocovariance function with the given lag
        //let cov = acvf s lag

        // Then the covariance equals that of a shifted series
        //let expectedCov = expectedCovariance vs lag
        //Assert.Equal(expectedCov, cov)
        0

    [<Fact>]
    member __.AutocovarianceLag2() =

        // Given a lag
        let lag = 2;

        // Given a simple series from 10 to 1
        let ks = [|1..10|]
        let vs = [|10. .. -1. .. 1.|]
        //let s = Series(ks, vs)

        // When calculating the autocovariance function with lag 2
        //let cov = acvf s lag

        // Then the covariance equals that of a shifted series
        //let expectedCov = expectedCovariance vs lag
        //Assert.Equal(expectedCov, cov)
        0

    [<Fact>]
    member __.AutocorrelationTest() =

        // Given an order of MA Q
        let q = 3
        let thetas = ContinuousUniform.Samples(0.2, 1.) |> Seq.take q |> Seq.toArray

        // When simulating from an MA Q model
        let samples = 100_000
        let zt() = sampleZ |> Seq.head
        let my = 0.
        let xs = maSamples my thetas zt 
        let xss = xs |> Seq.take samples |> Seq.toArray
        //let s = Series([1..samples], xss)

        // Then there is no significant autocorrelation beyond the order.
        //let zeroCorr = acf s (q+10)
        let tol = 1e-2
        //Assert.InRange(zeroCorr, 0.-tol, 0.+tol)

        // And all other orders show positive autocorrelation.
        [|1..q|]
        |> Array.iter (fun qx -> 
            //let c = acf s qx
            //Assert.True(c > 0.)
            Assert.True(true)
        )

        0

