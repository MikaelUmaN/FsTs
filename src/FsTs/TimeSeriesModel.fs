namespace FsTs

module TimeSeriesModel =

    let slideArr xt (xs: float[]) =
        [| xt |] |> Array.append xs.[1..]

    let autoregress (pqs: float[]) (xs: float[]) =
        let lags = min pqs.Length xs.Length
        xs
        |> Array.rev
        |> Array.take lags
        |> Array.zip (pqs |> Array.take lags)
        |> Array.map (fun (phi, xt) -> phi * xt)
        |> Array.sum

    /// Produces a single sample from an AR P model.
    /// phis are ordered 1 to t
    /// xs are ordered from oldest to most recent
    /// X_t = c + phit*X_t-1 + z
    let arSample c (phis: float[]) (xs: float[]) z =
        c + autoregress phis xs + z

    let arSamples c (phis: float[]) (z: unit -> float) =
        let len = phis.Length
        let mutable xs = Array.zeroCreate len
        Seq.initInfinite(fun i -> 
            let zt = z()
            let xt = arSample c phis xs zt
            xs <- slideArr xt xs
            xt
        )

    /// Produces a single sample from an MA Q model.
    /// thetas are ordered 1 to t
    /// zs are ordered from oldest to most recent
    /// X_t = my + thetat*z_t-1 + z
    let maSample my (thetas: float[]) (zs: float[]) z =
        my + z + autoregress thetas zs

    let maSamples my (thetas: float[]) (z: unit -> float) =
        let len = thetas.Length
        let mutable zs = Array.zeroCreate len
        Seq.initInfinite(fun i -> 
            let zt = z()
            let xt = maSample my thetas zs zt
            zs <- slideArr zt zs
            xt
        )

    /// Produces a single sample from an ARMA PQ model.
    /// phis are ordered 1 to t
    /// xs are ordered from oldest to most recent
    /// thetas are ordered 1 to t
    /// zs are ordered from oldest to most recent
    /// X_t = my + z + phit*X_t-1 + thetat*z_t-1 
    let armaSample c (phis: float[]) (xs: float[]) (thetas: float[]) (zs: float[]) z =
        c + z + (arSample 0. phis xs 0.) + (maSample 0. thetas zs 0.)

    let armaSamples c (phis: float[]) (thetas: float[]) (z: unit -> float) =
        let phiLen = phis.Length
        let thetaLen = thetas.Length
        let mutable xs = Array.zeroCreate phiLen
        let mutable zs = Array.zeroCreate thetaLen
        Seq.initInfinite(fun i -> 
            let zt = z()
            let xt = armaSample c phis xs thetas zs zt
            xs <- slideArr xt xs
            zs <- slideArr zt zs
            xt
        )