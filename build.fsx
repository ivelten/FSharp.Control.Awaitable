#r "paket:
nuget FSharp.Core 4.7.0.0
nuget Fake.Core.Target
nuget Fake.DotNet.Cli
nuget Fake.DotNet.AssemblyInfoFile
nuget Fake.Core.ReleaseNotes
nuget Fake.DotNet.Paket //"

#load ".fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators

let netCore (path : FilePath) =
    [ path ] |> CreateProcess.fromRawCommand "dotnet"

let netFramework (path : FilePath) =
    if Environment.isWindows
    then CreateProcess.fromRawCommand path []
    else [ path ] |> CreateProcess.fromRawCommand "mono"

let runTests (runner : FilePath -> CreateProcess<ProcessResult<unit>>) (pattern : IGlobbingPattern) =
    pattern
    |> Seq.map (runner >> Proc.run >> (fun x -> x.ExitCode))
    |> Seq.iter (fun code -> if code <> 0 then failwith "Test run failed.")

Target.create "Clean" <| fun _ ->
    !! "src/**/bin"
    ++ "src/**/obj"
    ++ "tests/**/bin"
    ++ "tests/**/obj"
    |> Shell.cleanDirs

Target.create "Restore" <| fun _ ->
    !! "src/**/*.fsproj"
    ++ "tests/**/*.fsproj"
    |> Seq.iter (DotNet.restore id)

Target.create "Build" <| fun _ ->
    !! "src/**/*.fsproj"
    ++ "tests/**/*.fsproj"
    |> Seq.iter (DotNet.build (fun options ->
        { options with 
            Configuration = DotNet.BuildConfiguration.Release
            Common = { options.Common with 
                        CustomParams = Some "--no-restore" } }))

Target.create "Test" <| fun _ ->
    !! "tests/**/bin/Release/net48/*Tests.exe"
    |> runTests netFramework
    !! "tests/**/bin/Release/netcoreapp3.1/*Tests.dll"
    |> runTests netCore

Target.create "All" ignore

"Clean"
  ==> "Restore"
  ==> "Build"
  ==> "Test"
  ==> "All"

Target.runOrDefault "All"