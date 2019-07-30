#r "paket:
nuget Fake.IO.FileSystem
nuget Fake.DotNet.Cli
nuget Fake.DotNet.MSBuild
nuget Fake.Core.Target //"
#load "./.fake/build.fsx/intellisense.fsx"

// include Fake modules, see Fake modules section

open Fake.Core
open Fake.DotNet

// *** Define Targets ***
Target.create "Clean" (fun _ ->
    Trace.log " --- Cleaning stuff --- "
    DotNet.exec (fun o -> o) "clean" "--configuration Release" |> ignore
)

Target.create "Build" (fun _ ->
    Trace.log " --- Building the app --- "
    DotNet.build (fun b -> 
        { b with
            Configuration = DotNet.Release
        }) 
        "FsTs.sln"
)

Target.create "Test" (fun _ ->
    Trace.log " --- Testing the app --- "
    DotNet.test (fun t ->
        { t with
            Configuration = DotNet.Release
        })
        "FsTs.sln"
)

Target.create "Deploy" (fun _ ->
    Trace.log " --- Deploying app --- "
)

open Fake.Core.TargetOperators

// *** Define Dependencies ***
"Clean"
    ==> "Build"
    ==> "Test"
    ==> "Deploy"

// *** Start Build ***
Target.runOrDefault "Deploy"