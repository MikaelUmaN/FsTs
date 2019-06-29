namespace FsTs

/// Module for fitting model parameters to data using Bayesian statistics.
module Fitting =

    open MathNet.Numerics.Distributions

    // TODO: Generalize...
    /// Given observations, fits a normal distribution to it.
    let fitNormal (xs: float[]) =

        // TODO: prior distributions should be supplied by user.
        // my ~ N(0, 1)
        // std ~ U(0+, 4)
        let myDist = Normal(0., 3.)
        let stdDist = ContinuousUniform(1e-2, 4.)

        let likelihood my std =
            let nd = Normal(my, std)
            xs
            |> Array.map (fun x -> nd.Density(x)) // Prob of this point for the given dist
            |> Array.fold (*) 1. // Likelihood (probably really small number...) -> need logs

        // The acceptance probability function.
        let alfa (myTheta, stdTheta) (myThetan, stdThetan) =
            let thetaL = likelihood myTheta stdTheta
            let thetanL = likelihood myThetan stdThetan

            // TODO: currently only supporting independence hastings
            //let myThetaDiff = myTheta - myThetan
            //let stdThetaDiff = stdTheta - stdThetan
            let numerator = 
                (thetaL * myDist.Density(myTheta) * stdDist.Density(stdTheta)) * 
                (myDist.Density(myTheta)) * // Independence hastings, unconditional probabilities
                (stdDist.Density(stdTheta))
            let denominator = 
                (thetanL * myDist.Density(myThetan) * stdDist.Density(stdThetan)) * 
                (myDist.Density(myThetan)) *
                (stdDist.Density(stdThetan))

            min (numerator/denominator) 1.


        let rec metropolisHastings my std i  =
            // need some way to stop...
            if (i > 100_000) then
                (my, std)
            else
                // Generate proposal from distributions.
                let myProposal = myDist.Sample()
                let thetaProposal = stdDist.Sample()

                // Compute acceptance probability
                let a = alfa (myProposal, thetaProposal) (my, std)
                let u = ContinuousUniform.Sample(0., 1.)
                if (a > u) then
                    metropolisHastings myProposal thetaProposal (i+1)
                else
                    metropolisHastings my std (i+1)

        metropolisHastings 0. 1. 0

