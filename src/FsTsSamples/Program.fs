// Learn more about F# at http://fsharp.org

open System
open FSharp.Data

type ExampleCsv =
    CsvProvider<Schema = "MyInt (int), MyString (string)", HasHeaders = false>

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
