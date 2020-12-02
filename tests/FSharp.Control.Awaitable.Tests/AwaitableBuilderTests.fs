module FSharp.Control.Awaitable.Tests.AwaitableBuilderTests

open Expecto
open FSharp.Control
open System.Threading.Tasks
open Config
open Helpers
open System.Diagnostics

let transform =
    function
    | null -> null
    | x -> x.GetHashCode().ToString()

let transformAsync x =
    async {
        let! bound = x
        return transform bound
    }

[<Tests>]
let properties =
    testList "AwaitableBuilder tests" [
        testProp "Short circuit" <| fun x y z ->
            let result =
                awaitable {
                    let! x = async.Return x
                    let! y = Task.FromResult y
                    return x + y + z
                }
            Awaitable.get result = x + y + z

        testProp "Task Delay" <| fun () ->
            let mutable y = 0
            let result =
                awaitable {
                    do! Task.Delay 50
                    y <- y + 1
                }
            Awaitable.get result
            y = 1

        testProp "Async Sleep" <| fun () ->
            let mutable y = 0
            let result =
                awaitable {
                    do! Async.Sleep 50
                    y <- y + 1
                }
            Awaitable.get result
            y = 1

        testProp "Task short Delay" <| fun () ->
            let mutable y = 0
            let result =
                awaitable {
                    y <- y + 1
                    do! Task.Delay 5
                    y <- y + 1
                }
            Awaitable.get result
            y = 2

        testProp "Async short Sleep" <| fun () ->
            let mutable y = 0
            let result =
                awaitable {
                    y <- y + 1
                    do! Async.Sleep 5
                    y <- y + 1
                }
            Awaitable.get result
            y = 2

        testProp "Task should not block" <| fun () ->
            let sw = Stopwatch ()
            sw.Start ()
            let result = awaitable { do! Task.Delay 100 }
            sw.Stop ()
            Awaitable.get result
            sw.ElapsedMilliseconds < 50L

        testProp "Async should not block" <| fun () ->
            let sw = Stopwatch ()
            sw.Start ()
            let result = awaitable { do! Async.Sleep 100 }
            sw.Stop ()
            Awaitable.get result
            sw.ElapsedMilliseconds < 50L

        testProp "Task catch should work as expected" <| fun () ->
            let mutable a = 0
            let mutable b = 0
            let mutable exceptionWasThrown = true
            let result =
                awaitable {
                    try
                        do! Task.Delay 0
                        raise <| exn ("hello")
                        a <- 1
                        exceptionWasThrown <- false
                        do! Task.Delay 100
                    with
                    | ex -> exceptionWasThrown <- ex.Message = "hello"
                    b <- 1
                }
            Awaitable.get result
            exceptionWasThrown && a = 0 && b = 1

        testProp "Async catch should work as expected" <| fun () ->
            let mutable a = 0
            let mutable b = 0
            let mutable exceptionWasThrown = true
            let result =
                awaitable {
                    try
                        do! Async.Sleep 0
                        raise <| exn ("hello")
                        a <- 1
                        exceptionWasThrown <- false
                        do! Async.Sleep 100
                    with
                    | ex -> exceptionWasThrown <- ex.Message = "hello"
                    b <- 1
                }
            Awaitable.get result
            exceptionWasThrown && a = 0 && b = 1

        testProp "Bind should transform Awaitable" <| fun (x : Awaitable<obj>) ->
            let result = 
                awaitable {
                    let! bound = x
                    return transform bound
                }
            match x, result with
            | Sync x, Sync y -> transform x = y
            | Async x, Async y -> transformAsync x |> asyncEquals y
            | _ -> false

        testProp "Bind should transform Async" <| fun (x : Async<obj>) ->
            let result = 
                awaitable {
                    let! bound = x
                    return transform bound
                }
            match result with
            | Async y -> transformAsync x |> asyncEquals y
            | _ -> false
        
        testProp "Bind should transform Task" <| fun (x : Task<obj>) ->
            let result = 
                awaitable {
                    let! bound = x
                    return transform bound
                }
            match result with
            | Async y -> x |> Async.AwaitTask |> transformAsync |> asyncEquals y
            | _ -> false
    ]