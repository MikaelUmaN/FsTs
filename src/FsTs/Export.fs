namespace FsTs

module Export =

    open System
    open System.IO
    open FSharp.Data
    open FSharp.Data.CsvExtensions

    let thetaTraceCsv (thetas:array<string>) (samples: seq<float[]>) (filePath: string) =
        let fs = new FileStream(filePath, FileMode.CreateNew)
        let traceCsv = CsvFile.Load(fs)
        traceCsv.

        traceCsv.Headers <- Some(thetas)




        3

