namespace FsTs

/// Module for fitting model parameters to data using Bayesian statistics.
module Mcmc =

    open Model
    open Distributions
    open MathNet.Numerics.Distributions

    /// Algorithms to use for sampling from the joint posterior distribution
    /// of the parameters given the observed data.
    type Algorithm =
        /// Requires that the proposal distribution be symmetric.
        | Metropolis
        /// Corrects any asymmetry in the proposal distribution.
        | MetropolisHastings

    // TODO: Only supports Metropolis-Hastings
    /// Given data observations, fits a model to it using a set of proposal distributions.
    let fitModel
        (data: float[])
        (model: IModel)
        (propDists: IConditionalDistribution[]) 
        (initialGuess: float[]) =

        // Evaluate the density function at all observations for the given parameters
        let densityEval pDists =
            let d = model.Likelihood pDists
            data
            |> Array.map d // Prob of this point for the given dist

        // The acceptance probability function.
        let alfa candidateTheta currentTheta =
            // p(X|data) using the two parameter sets.
            let densitiesCandidate = densityEval candidateTheta
            let densitiesThetan = densityEval currentTheta

            // TODO: maybe we don't have to convert to logs if the ratios don't go crazy?
            // This idea is only applicable because of the assumption that parameters
            // are independent.
            let ratios = 
                Array.zip densitiesCandidate densitiesThetan 
                |> Array.map (fun (x, y) -> x/y)
            let likelihoodRatio = ratios |> Array.fold (*) 1.

            // Density of the prior
            let candidatePrior = model.PriorDensities candidateTheta |> Array.fold (*) 1.
            let currentPrior = model.PriorDensities currentTheta |> Array.fold (*) 1.
            let priorRatio = candidatePrior / currentPrior

            // Density of moving from prev parameter value to current (and vice versa)
            let numeratorMove =
                Array.zip3 propDists currentTheta candidateTheta
                |> Array.map (fun (pd, current, candidate) -> pd.ConditionalDensity current candidate)
                |> Array.fold (*) 1.
            let denominatorMove =
                Array.zip3 propDists currentTheta candidateTheta
                |> Array.map (fun (pd, current, candidate) -> pd.ConditionalDensity candidate current)
                |> Array.fold (*) 1.
            let moveRatio = numeratorMove / denominatorMove

            // likelihood * prior * proposal move correction
            let a = likelihoodRatio * priorRatio * moveRatio
            min a 1.

        // One step in the markov kernel.
        // Computes a new candidate given the current state of the chain,
        // then accepts or rejects it.
        let markovKernel current =

            // Generate proposal from distributions.
            let candidates = 
                propDists 
                |> Array.zip current 
                |> Array.map (fun (c, pd) -> pd.ConditionalSample(c))

            // Compute acceptance probability
            let a = alfa candidates current
            let u = ContinuousUniform.Sample(0., 1.)
            if (a > u) then
                candidates
            else
                current

        // Infinite seqeunce of parameter proposals.
        let mutable theta = initialGuess
        Seq.initInfinite(fun i ->
            theta <- markovKernel theta
            theta
        )
