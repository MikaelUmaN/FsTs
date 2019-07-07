namespace FsTs

module Distributions =

    open MathNet.Numerics.Distributions

    let sampleZ = Normal.Samples(0., 1.)

    /// A distribution used to sample new proposals
    /// for parameters.
    type IProposalDistribution =

        /// Generates a new sample from the distribution.
        abstract Sample: unit -> float

        /// Computes the density of the distribution at the given point.
        abstract Density: float -> float

    /// Normal proposal distribution.
    type NormalProposalDistribution(my, std) =
        let nd = Normal(my, std)
        interface IProposalDistribution with
            member __.Sample() = nd.Sample()
            member __.Density(x) = nd.Density(x)

    /// Continuous uniform proposal distribution.
    type ContinuousUniformProposalDistribution(lower, upper) =
        let ud = ContinuousUniform(lower, upper)
        interface IProposalDistribution with
            member __.Sample() = ud.Sample()
            member __.Density(x) = ud.Density(x)  