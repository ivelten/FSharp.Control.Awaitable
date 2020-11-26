module FSharp.Control.Awaitable.Tests.AwaitableTests

open Expecto
open FSharp.Control
open System.Threading.Tasks
open Config
open Helpers
open System.Collections.Generic

let mapper x =
    match x with
    | null -> null
    | _ -> x.GetHashCode().ToString()

let binder ctor (x : obj) =
    match x with
    | null -> null
    | _ -> x.GetHashCode().ToString()
    |> ctor

[<Tests>]
let properties =
    testList "Awaitable module tests" [

        testProp "isSync should return true when Awaitable is a synchronous value" <| fun x ->
            match x with
            | Sync _ -> Awaitable.isSync x
            | _ -> not (Awaitable.isSync x)
        
        testProp "isAsync should return true when Awaitable is an asynchronous value" <| fun x ->
            match x with
            | Async _ -> Awaitable.isAsync x
            | _ -> not (Awaitable.isAsync x)
        
        testProp "ofValue should create a synchronous value" <| fun x ->
            match Awaitable.ofValue x with
            | Sync v -> x = v
            | _ -> false
        
        testProp "ofAsync should create an asynchronous value" <| fun x ->
            match Awaitable.ofAsync x with
            | Async a -> asyncEquals x a
            | _ -> false
        
        testProp "ofTask should create an asynchronous value" <| fun x ->
            match Awaitable.ofTask x with
            | Async a -> asyncEqualsTask x a
            | _ -> false
        
        testProp "get should return value synchronously" <| fun x ->
            match x with
            | Sync v -> Awaitable.get x = v
            | Async a -> Awaitable.get x = Async.RunSynchronously a

        testProp "toAsync should return value asynchronously" <| fun x ->
            let asyncValue = Awaitable.toAsync x
            match x with
            | Sync v -> asyncEqualsSync v asyncValue
            | Async a -> asyncEquals asyncValue a

        testProp "map should transform inner value" <| fun x ->
            let mapped = Awaitable.map mapper x
            match x, mapped with
            | Sync x, Sync y ->  mapper x = y
            | Async x, Async y -> mapAsync mapper x |> asyncEquals y
            | _ -> false

        testProp "bind should transform inner value" <| fun x ->
            let bound = Awaitable.bind (binder Awaitable.ofValue) x
            match x, bound with
            | Sync x, Sync y -> binder id x = y
            | Async x, Async y -> bindAsync (binder async.Return) x |> asyncEquals y
            | _ -> false
            
        testProp "bindAsync should transform inner value" <| fun x ->
            let bound = Awaitable.bindAsync (binder Awaitable.ofValue) x
            match bound with
            | Sync v -> binder async.Return x |> asyncEqualsSync v
            | Async a -> bindAsync (binder async.Return) x |> asyncEquals a
            
        testProp "bindTask should transform inner value" <| fun x ->
            let bound = Awaitable.bindTask (binder Awaitable.ofValue) x
            match bound with
            | Sync v -> binder Task.FromResult x |> taskEqualsSync v
            | Async a -> bindTask (binder async.Return) x |> asyncEquals a

        testProp "rescue should generate value when exception occurs" <| fun x ->
            let awaitable = Async (async { return failwith "Test error!" })
            let rescued = Awaitable.rescue (fun _ -> x) awaitable
            match rescued with
            | Sync v -> v = x
            | Async a -> asyncEqualsSync x a

        testProp "combine should return second awaitable after running first in case of asynchronous" <| fun y ->
            let mutable wasRun = false
            let x = Async (async { wasRun <- true })
            let z = Awaitable.combine y x
            match y, z with
            | Sync y, Async z -> asyncEqualsSync y z && wasRun
            | Async y, Async z -> asyncEquals y z && wasRun
            | _ -> false

        testProp "combine should return second awaitable in case of synchronous" <| fun y ->
            let x = Sync ()
            let z = Awaitable.combine y x
            match y, z with
            | Sync y, Sync z -> y = z
            | Async y, Async z -> asyncEquals y z
            | _ -> false

        testProp "forAll should iterate sequence" <| fun xs ->
            let items = List<_>(0)
            let binder x = items.Add x; Sync ()
            match Awaitable.forAll binder xs with
            | Sync () -> List.ofSeq items = xs
            | _ -> false
    ]