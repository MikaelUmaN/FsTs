namespace FsTs

module Model =

    open Distributions

    type FsTsIDistribution = IDistribution

    open MathNet.Numerics.Distributions

    let minVal = 1e-8

    type ILikelihoodFunction =    
        /// Evaluates the conditional likelihood of a data point at the given parameters,
        /// given the information set available at the time of the data point (e.g. the data history).
        abstract ConditionalLikelihood: x:float -> xhist:float[] -> theta:float[] -> float

    /// A target model to fit data to.
    /// The target distribution is 
    type IModel =
        inherit ILikelihoodFunction

        /// Evaluates the prior densities at the given parameter values.
        abstract PriorDensities: theta:float[] -> float[]

    /// Creates a model based on a likelihood function where the parameters of the model
    /// are independent.
    let independentParametersModel (likelihoodF: #ILikelihoodFunction) (thetaDists: array<#FsTsIDistribution>) =
        { new IModel with
            member __.ConditionalLikelihood x xhist theta = likelihoodF.ConditionalLikelihood x xhist theta

            member __.PriorDensities theta =
                thetaDists
                |> Array.zip theta
                |> Array.map (fun (t, d) -> d.Density(t))
        }

    /// Computes likelihood for a normal target distribution with the given
    /// location and variance.
    let normalLikelihood =
        { new ILikelihoodFunction with
            member __.ConditionalLikelihood x _ theta = 
                if (theta.[1] < minVal) then 0. else Normal.PDF(theta.[0], theta.[1], x)
        }
    let normalMeanLikelihood sigma =
        { new ILikelihoodFunction with
            member __.ConditionalLikelihood x _ theta = 
                 if (sigma < minVal) then 0. else Normal.PDF(theta.[0], sigma, x)
        }
    let normalVarianceLikelihood my =
         { new ILikelihoodFunction with
             member __.ConditionalLikelihood x _ theta = 
                if (theta.[0] < minVal) then 0. else Normal.PDF(my, theta.[0], x)
         }

    let normalMeanModel sigma dist = 
        independentParametersModel (normalMeanLikelihood sigma) [| dist |] 
    let normalVarianceModel my dist = 
        independentParametersModel (normalVarianceLikelihood my) [| dist |] 
    let normalModel dist =
        independentParametersModel normalLikelihood dist
