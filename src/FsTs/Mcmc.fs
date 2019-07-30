namespace FsTs

/// Module for fitting model parameters to data using Bayesian statistics.
module Mcmc =

    open Model
    open Distributions
    open MathNet.Numerics.Distributions

    /// Given data observations, fits a model to it using a set of proposal distributions.
    let mcmcMhSeq
        data
        (model: #IModel)
        (propDist: #IProposalDistribution) 
        initialGuess =

        // Evaluate the density function at all observations for the given parameters,
        // using all the information available at the time.
        let likelihoodEval theta =
            data
            |> Array.mapi (fun i d -> 
                let xhist = data.[0..i]
                xhist |> Array.take 1 |> ignore
                model.ConditionalLikelihood d xhist theta
            )

        // The acceptance probability function.
        // Based on difference between sum of logs of numerator and denominator.
        let acceptanceProbability candidateTheta currentTheta =

            // p(X|data) using the two parameter sets.
            let densitiesCandidate = likelihoodEval candidateTheta |> Array.map log |> Array.sum
            let densitiesThetan = likelihoodEval currentTheta |> Array.map log |> Array.sum

            // Density of the prior
            let candidatePrior = model.PriorDensities candidateTheta |> Array.map log |> Array.sum
            let currentPrior = model.PriorDensities currentTheta |> Array.map log |> Array.sum

            // Density of moving from prev parameter value to current (and vice versa)
            let numeratorMove = propDist.ConditionalDensity currentTheta candidateTheta |> Array.map log |> Array.sum
            let denominatorMove = propDist.ConditionalDensity candidateTheta currentTheta |> Array.map log |> Array.sum

            // likelihood * prior * proposal move correction
            let numeratorSum = densitiesCandidate + candidatePrior + numeratorMove
            let denominatorSum = densitiesThetan + currentPrior + denominatorMove
            let a = numeratorSum - denominatorSum
            let logA = min a 0. // ln 1 = 0
            exp logA // Get back to normal scale

        // One step in the markov kernel.
        // Computes a new candidate given the current state of the chain,
        // then accepts or rejects it.
        let markovKernel current =

            // Generate proposal from distributions.
            let candidates = propDist.ConditionalSample current

            // Compute acceptance probability
            let a = acceptanceProbability candidates current
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

    /// Given data observations, fits a model to it using Metropolis-Hastings
    /// Thinning takes every i:th sample after skipping an initial n burn-in iterations.
    /// The final result is of size samples.
    let mcmcMh
        data
        (model: #IModel)
        (propDist: #IProposalDistribution) 
        initialGuess
        burnIn
        thinning
        samples =

        let thetaSeq = mcmcMhSeq data model propDist initialGuess
        thetaSeq
        |> Seq.take burnIn
        |> Seq.toArray
        |> ignore

        thetaSeq
        |> Seq.take (thinning * samples)
        |> Seq.toArray
        |> Array.mapi (fun i s -> (i % thinning = 0, s))
        |> Array.filter (fun (i, _) -> i)
        |> Array.map (fun (_, s) -> s)