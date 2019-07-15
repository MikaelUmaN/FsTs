namespace FsTs

module Distributions =

    open MathNet.Numerics.Distributions

    let sampleZ = Normal.Samples(0., 1.)

    /// A distribution used to sample new proposals
    /// for parameters.
    type IDistribution =

        /// Generates a new sample from the distribution.
        abstract Sample: unit -> float

        /// Computes the density of the distribution at the given point.
        abstract Density: float -> float

    /// A proposal distribution where the density and sampling functions
    /// are dependent on the previous state in the markov chain.
    type IConditionalDistribution =
        inherit IDistribution

        /// Generates a new sample from the distribution,
        /// conditional on the past sample being xprev.
        abstract ConditionalSample: xprev:float -> float

        /// Comptutes the density of x, conditional on the
        /// previous state xprev.
        abstract ConditionalDensity: x:float -> xprev:float -> float

    /// Normal proposal distribution.
    type NormalDistribution(my, std) =
        let nd = Normal(my, std)
        interface IConditionalDistribution with
            member __.Sample() = nd.Sample()
            member __.Density(x) = nd.Density(x)
            member __.ConditionalSample(xprev) =
                let ndm = Normal(my + xprev, std) // Prev state shifts the mean.
                ndm.Sample()
            member __.ConditionalDensity x xprev = 
                let ndm = Normal(my + xprev, std) // Prev state shifts the mean.
                ndm.Density(x)


    /// Continuous uniform proposal distribution.
    type ContinuousUniformDistribution(lower, upper) =
        let ud = ContinuousUniform(lower, upper)
        interface IConditionalDistribution with
            member __.Sample() = ud.Sample()
            member __.Density(x) = ud.Density(x)
            member __.ConditionalSample(xprev) =
                let udm = ContinuousUniform(lower + xprev, upper + xprev)
                udm.Sample()
            member __.ConditionalDensity x xprev =
                let udm = ContinuousUniform(lower + xprev, upper + xprev)
                udm.Density(x)