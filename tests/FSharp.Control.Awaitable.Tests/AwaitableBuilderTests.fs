module FSharp.Control.Awaitable.Tests.AwaitableBuilderTests

open Expecto
open FSharp.Control
open System.Threading.Tasks
open Config
open Helpers
open System.Diagnostics
open Assertions
open System

type TestException (msg) =
    inherit exn (msg)

let (|TestException|_|) (ex : exn) =
    match ex with
    | :? TestException as tex -> Some tex.Message
    | _ -> None

let raiseTestException msg = raise <| TestException (msg)

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
let tests =
    testList "AwaitableBuilder tests" [
        testProp "Basic Bind" <| fun x y z ->
            let result =
                awaitable {
                    let! x = async.Return x
                    let! y = Task.FromResult y
                    return x + y + z
                }
            Awaitable.get result = x + y + z

        test "Task Delay" {
            let mutable x = 0
            let result =
                awaitable {
                    do! Task.Delay 50
                    x <- x + 1
                }
            require (x = 0)
            Awaitable.get result
            require (x = 1)
        }

        test "Async Sleep" {
            let mutable x = 0
            let result =
                awaitable {
                    do! Async.Sleep 50
                    x <- x + 1
                }
            require (x = 0)
            Awaitable.get result
            require (x = 1)
        }

        test "Task no Delay" {
            let mutable x = 0
            let result =
                awaitable {
                    x <- x + 1
                    do! Task.Delay 5
                    x <- x + 1
                }
            require (x = 1)
            Awaitable.get result
        }

        test "Async no Sleep" {
            let mutable x = 0
            let result =
                awaitable {
                    x <- x + 1
                    do! Async.Sleep 5
                    x <- x + 1
                }
            require (x = 1)
            Awaitable.get result
        }

        test "Task should not block" {
            let sw = Stopwatch ()
            sw.Start ()
            let result = awaitable { do! Task.Delay 100 }
            sw.Stop ()
            Awaitable.get result
            require (sw.ElapsedMilliseconds < 50L)
        }

        test "Async should not block" {
            let sw = Stopwatch ()
            sw.Start ()
            let result = awaitable { do! Async.Sleep 100 }
            sw.Stop ()
            Awaitable.get result
            require (sw.ElapsedMilliseconds < 50L)
        }

        test "Task catch should work as expected" {
            let mutable x = 0
            let mutable y = 0
            let result =
                awaitable {
                    try
                        do! Task.Delay 0
                        raiseTestException "hello"
                        x <- 1
                        do! Task.Delay 100
                    with
                    | TestException msg -> require (msg = "hello")
                    | _ -> require false
                    y <- 1
                }
            Awaitable.get result
            require (x = 0 && y = 1)
        }

        test "Async catch should work as expected" {
            let mutable x = 0
            let mutable y = 0
            let result =
                awaitable {
                    try
                        do! Async.Sleep 0
                        raiseTestException "hello"
                        x <- 1
                        do! Async.Sleep 100
                    with
                    | TestException msg -> require (msg = "hello")
                    | _ -> require false
                    y <- 1
                }
            Awaitable.get result
            require (x = 0 && y = 1)
        }

        test "Nested Task catch should work as expected" {
            let mutable counter = 1
            let mutable caughtInner = 0
            let mutable caughtOuter = 0
            let result1 () =
                awaitable {
                    try
                        do! Task.Delay 0
                        raiseTestException "hello"
                    with 
                    | TestException _ as ex -> 
                        caughtInner <- counter
                        counter <- counter + 1
                        raise ex
                    | _ -> require false
                }
            let result2 =
                awaitable {
                    try
                        do! result1 ()
                    with
                    | TestException _ as ex ->
                        caughtOuter <- counter
                        raise ex
                    | _ -> require false
                }
            try
                Awaitable.get result2
                require false
            with 
            | TestException msg ->
                require (msg = "hello")
                require (caughtInner = 1)
                require (caughtOuter = 2)
        }

        test "Nested Async catch should work as expected" {
            let mutable counter = 1
            let mutable caughtInner = 0
            let mutable caughtOuter = 0
            let result1 () =
                awaitable {
                    try
                        do! Async.Sleep 0
                        raiseTestException "hello"
                    with 
                    | TestException _ as ex -> 
                        caughtInner <- counter
                        counter <- counter + 1
                        raise ex
                    | _ -> require false
                }
            let result2 =
                awaitable {
                    try
                        do! result1 ()
                    with
                    | TestException _ as ex ->
                        caughtOuter <- counter
                        raise ex
                    | _ -> require false
                }
            try
                Awaitable.get result2
                require false
            with 
            | TestException msg ->
                require (msg = "hello")
                require (caughtInner = 1)
                require (caughtOuter = 2)
        }

        test "Try-Finally Task happy path" {
            let mutable ran = false
            let result =
                awaitable {
                    try
                        require (not ran)
                        do! Task.Delay 100
                        require (not ran)
                    finally
                        ran <- true
                }
            try Awaitable.get result
            with _ -> ()
            require ran
        }

        test "Try-Finally Async happy path" {
            let mutable ran = false
            let result =
                awaitable {
                    try
                        require (not ran)
                        do! Async.Sleep 100
                        require (not ran)
                    finally
                        ran <- true
                }
            try Awaitable.get result
            with _ -> ()
            require ran
        }

        test "Try-Finally Task sad path" {
            let mutable ran = false
            let result =
                awaitable {
                    try
                        require (not ran)
                        do! Task.Delay 100
                        require (not ran)
                        raiseTestException "oh no"
                    finally
                        ran <- true
                }
            try Awaitable.get result
            with _ -> ()
            require ran
        }

        test "Try-Finally Async sad path" {
            let mutable ran = false
            let result =
                awaitable {
                    try
                        require (not ran)
                        do! Async.Sleep 100
                        require (not ran)
                        raiseTestException "oh no"
                    finally
                        ran <- true
                }
            try Awaitable.get result
            with _ -> ()
            require ran
        }

        test "Try-Finally Task caught path" {
            let mutable ran = false
            let result =
                awaitable {
                    try
                        try
                            require (not ran)
                            do! Task.Delay 100
                            require (not ran)
                            raiseTestException "oh no"
                        finally
                            ran <- true
                        return 1
                    with _ -> return 2
                }
            require (Awaitable.get result = 2)
            require ran
        }

        test "Try-Finally Async caught path" {
            let mutable ran = false
            let result =
                awaitable {
                    try
                        try
                            require (not ran)
                            do! Async.Sleep 100
                            require (not ran)
                            raiseTestException "oh no"
                        finally
                            ran <- true
                        return 1
                    with _ -> return 2
                }
            require (Awaitable.get result = 2)
            require ran
        }

        test "Using with Task" {
            let mutable disposed = false
            let result =
                awaitable {
                    use _ = { new IDisposable with member __.Dispose () = disposed <- true }
                    require (not disposed)
                    do! Task.Delay 100
                    require (not disposed)
                }
            Awaitable.get result
            require disposed
        }

        test "Using with Async" {
            let mutable disposed = false
            let result =
                awaitable {
                    use _ = { new IDisposable with member __.Dispose () = disposed <- true }
                    require (not disposed)
                    do! Async.Sleep 100
                    require (not disposed)
                }
            Awaitable.get result
            require disposed
        }

        test "Using inside Task Awaitable" {
            let mutable disposedInner = false
            let mutable disposed = false
            let result =
                awaitable {
                    use! _x =
                        awaitable {
                            do! Task.Delay 50
                            use _ = { new IDisposable with member __.Dispose () = disposedInner <- true }
                            require (not disposed && not disposedInner)
                            return { new IDisposable with member __.Dispose () = disposed <- true }
                        }
                    require disposedInner
                    require (not disposed)
                    do! Task.Delay 50
                    require (not disposed)
                }
            Awaitable.get result
            require disposed
        }

        test "Using inside Async Awaitable" {
            let mutable disposedInner = false
            let mutable disposed = false
            let result =
                awaitable {
                    use! _x =
                        awaitable {
                            do! Async.Sleep 50
                            use _ = { new IDisposable with member __.Dispose () = disposedInner <- true }
                            require (not disposed && not disposedInner)
                            return { new IDisposable with member __.Dispose () = disposed <- true }
                        }
                    require disposedInner
                    require (not disposed)
                    do! Async.Sleep 50
                    require (not disposed)
                }
            Awaitable.get result
            require disposed
        }

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