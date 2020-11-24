module FSharp.Control.Awaitable.Tests.Program

open Expecto
open Expecto.Logging

[<EntryPoint>]
let main args =
    let config = { defaultConfig with verbosity = Verbose }
    runTestsInAssembly config args