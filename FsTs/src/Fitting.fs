namespace FsTs

/// Module for fitting model parameters to data using Bayesian statistics.
module Fitting =

    open Model
    open Distributions
    open MathNet.Numerics.Distributions

    /// Given data observations, fits a model to it using
    /// a set of proposal distributions.
    let fitModel (data: float[], model: IModel, propDists: IProposalDistribution[], initialGuess: float[]) =

        // Evaluate the density function at all observations for the given parameters
        let densityEval pDists =
            let d = model.Density pDists
            data
            |> Array.map d // Prob of this point for the given dist

        // The acceptance probability function.
        let alfa candidateTheta currentTheta =
            let densitiesCandidate = densityEval candidateTheta
            let densitiesThetan = densityEval currentTheta

            // TODO: maybe we don't have to convert to logs if the ratios don't go crazy?
            let ratios = 
                Array.zip densitiesCandidate densitiesThetan 
                |> Array.map (fun (x, y) -> x/y)
            let likelihoodRatio = ratios |> Array.fold (*) 1.

            // likelihood * prior * proposalDistribution
            // TODO: currently just supports Indepdence MH, unconditional probabilities
            let proposalDistNumerator =
                propDists 
                |> Array.zip currentTheta
                |> Array.map (fun (x, pd) -> pd.Density(x))
                |> Array.fold (*) 1.
            let proposalDistDenominator =            
                propDists 
                |> Array.zip candidateTheta
                |> Array.map (fun (x, pd) -> pd.Density(x))
                |> Array.fold (*) 1.
            let proposalDistRatio = proposalDistNumerator / proposalDistDenominator
            let a = likelihoodRatio * proposalDistRatio
            min a 1.

        let metropolisHastings current =
            // Generate proposal from distributions.
            let candidates = propDists |> Array.map (fun pd -> pd.Sample())

            // Compute acceptance probability
            let a = alfa candidates current
            let u = ContinuousUniform.Sample(0., 1.)
            if (a > u) then
                candidates
            else
                current

        // Infinite seqeunce of parameter proposals.
        let mutable theta = initialGuess
        seq {
            while (true) do 
                theta <- metropolisHastings theta
                yield theta
        }

