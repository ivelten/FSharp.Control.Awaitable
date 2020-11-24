module FSharp.Control.Awaitable.Tests.Gen

open System.Threading.Tasks
open Expecto
open FsCheck
open FSharp.Control

let async =
    gen {
        let! x = Arb.generate<obj>
        return async { return x }
    }

let task = Gen.map Async.StartAsTask async

let sync = gen { return! Arb.generate<obj> }

let asyncArb = Arb.fromGen async

let taskArb = Arb.fromGen task

let awaitableArb =
    [ Gen.map Async async; Gen.map Sync sync ]
    |> Gen.oneof
    |> Arb.fromGen

type AwaitableValue<'T> = AwaitableValue of Awaitable<'T>

let awaitableValueArb =
    awaitableArb |> Arb.convert AwaitableValue (fun (AwaitableValue x) -> x)

type AsyncValue<'T> = AsyncValue of Async<'T>

let asyncValueArb =
    asyncArb |> Arb.convert AsyncValue (fun (AsyncValue x) -> x)

type TaskValue<'T> = TaskValue of Task<'T>

let taskValueArb =
    taskArb |> Arb.convert TaskValue (fun (TaskValue x) -> x)

let addToConfig (config : FsCheckConfig) =
    let types = 
        [ typeof<AwaitableValue<_>>.DeclaringType
          typeof<AsyncValue<_>>.DeclaringType
          typeof<TaskValue<_>>.DeclaringType ]
    { config with arbitrary = types @ config.arbitrary }