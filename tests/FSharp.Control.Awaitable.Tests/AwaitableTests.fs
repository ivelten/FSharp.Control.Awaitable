module FSharp.Control.Awaitable.Tests.AwaitableTests

open Expecto
open FSharp.Control

[<Tests>]
let properties =
    testList "Awaitable module tests" [
        testProperty "isSync should return true when Awaitable is a synchronous value" <| fun a ->
            match a with
            | Sync _ -> Awaitable.isSync a
            | _ -> not <| Awaitable.isSync a
        testProperty "isAsync should return true when Awaitable is an asynchronous value" <| fun a ->
            match a with
            | Async _ -> Awaitable.isAsync a
            | _ -> not <| Awaitable.isAsync a
    ]