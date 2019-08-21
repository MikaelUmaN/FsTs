namespace FsTs

module McmcDiagnostics =

    open Deedle

    // Autocovariance with given lag
    let acvf (s: Series<'K,float>) lag =
        let sx = s |> Series.shift lag
        let sm = Stats.mean s
        let sxm = Stats.mean sx

        let cov = (s - sm)*(sx - sxm)
        let evSum = cov |> Stats.sum
        let n = cov |> Stats.count |> float
        evSum / n

    // Autocorrelation with given lag
    let acf (s: Series<'K,float>) lag =
        let cov = acvf s lag
        let sStd = s |> Series.skip lag |> Stats.stdDev
        let sxStd = s |> Series.shift lag |> Stats.stdDev

        cov / (sStd*sxStd)


        

