namespace FsTs

module Distributions =

    open MathNet.Numerics.Distributions

    let sampleZ = Normal.Samples(0., 1.)

    /// A continuous probability distribution.
    type IDistribution =

        /// Generates a new sample from the distribution.
        abstract Sample: unit -> float

        /// Computes the density of the distribution at the given point.
        abstract Density: float -> float

    type GammaDistribution(shape, rate) =
        interface IDistribution with
            member __.Sample() = Gamma.Sample(shape, rate)
            member __.Density x = Gamma.PDF(shape, rate, x)

    type CauchyDistribution(location, scale) =
        interface IDistribution with
            member __.Sample() = Cauchy.Sample(location, scale)
            member __.Density x = Cauchy.PDF(location, scale, x)

    // Left half is just set to zero. -> TODO: does not work.
    type HalfCauchyDistribution(location, scale) =
        interface IDistribution with
            member __.Sample() =
                let mutable s = Cauchy.Sample(location, scale)
                while (s < location) do s <- Cauchy.Sample(location, scale)
                s
            member __.Density x = if x < location then 0. else 2. * Cauchy.PDF(location, scale, x)

    type NormalDistribution(my, sigma) =
        interface IDistribution with
            member __.Sample() = Normal.Sample(my, sigma)
            member __.Density x = Normal.PDF(my, sigma, x)

    type UniformDistribution(a, b) =
        interface IDistribution with
            member __.Sample() = ContinuousUniform.Sample(a, b)
            member __.Density x = ContinuousUniform.PDF(a, b, x)

    /// A multivariate proposal distribution used to propose new values
    /// of parameters used in a model.
    type IProposalDistribution =

        /// Conditionally samples new parameter proposals
        /// based on the previous value.
        abstract ConditionalSample: xprev:float[] -> float[] 

        /// Conditionally computes the density of x, given the
        /// previous value of xprev. That is, the jump from xprev to x.
        abstract ConditionalDensity: x:float[] -> xprev:float[] -> float[]

    /// Creates an independent multivariate proposal distribution consisting of the given
    /// input distributions.
    let independentMultivariateProposalDistribution (dists: array<#IDistribution>) =
        { new IProposalDistribution with
            member __.ConditionalSample xprev =
                xprev
                |> Array.zip dists
                |> Array.map (fun (d, x) -> d.Sample() + x)
            member __.ConditionalDensity x xprev =
                xprev
                |> Array.zip x
                |> Array.map (fun (x, xprev) -> x - xprev)
                |> Array.zip dists
                |> Array.map (fun (d, y) -> d.Density(y))
        }
