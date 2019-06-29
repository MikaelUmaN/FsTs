namespace FsTs

module Model =

    open MathNet.Numerics.Distributions

    type ModelSpec =
    | AR of p: int
    | MA of q: int
    | Composition of ModelSpec * ModelSpec

    type IModel =

        abstract Spec: ModelSpec

        abstract Samples: seq<float>


    // TODO: Generalize to other distributions.

    /// Autoregressive model.
    /// p is the order.
    /// my is the mean.
    /// phis are the parameters, length of p in order of increasing importance: phi_p, phi_p-1, ... phi_1
    type ARModel(p, my, phis: float[]) =

        // Validate
        do
            if (phis.Length <> p) then
                failwith "AR(p) requires p phi parameters"

        let spec = AR p

        /// X_t = phit*X_t-1 + z_t 
        let xt bxt phit zt = my + phit*bxt + zt

        interface IModel with

            member __.Spec = spec

            member __.Samples =

                // State variables
                let (xs: ResizeArray<float>) = ResizeArray()
                xs.AddRange(Array.zeroCreate p)
                let zs = Distributions.sampleZ
                
                // TODO: nu stöds bara AR1 haha.
                Seq.initInfinite (
                    fun i ->
                        
                        // Compute the autoregressive sum
                        let sumXt = 
                            Array.zip phis (xs.ToArray())
                            |> Array.map (fun (phi, x) -> phi * x)
                            |> Array.sum

                        // Sample innovation and compute new Xt.
                        let z = Seq.take 1 zs |> Seq.exactlyOne
                        let xt = my + sumXt + z

                        // Shift memory of x.
                        for j in [0..1.. p-2] do
                            xs.[j] <- xs.[j+1]
                        xs.[p-1] <- xt

                        xt
                )
            

