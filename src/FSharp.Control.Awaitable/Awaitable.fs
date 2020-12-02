namespace FSharp.Control

open System.Threading.Tasks

type Awaitable<'T> =
    | Sync of value : 'T
    | Async of computation : Async<'T>

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Awaitable =
    let inline isSync (x : Awaitable<'T>) = 
        match x with
        | Sync _ -> true
        | _ -> false

    let inline isAsync (x : Awaitable<'T>) = 
        match x with
        | Async _ -> true
        | _ -> false

    let inline ofValue (x : 'T) = Sync x

    let inline ofAsync (x : Async<'T>) = Async x

    let inline ofTask (x : Task<'T>) = Async (Async.AwaitTask x)

    let inline ofPlainTask (x : Task) = Async (Async.AwaitTask x)

    let inline get (x : Awaitable<'T>) =
        match x with
        | Sync v -> v
        | Async a -> Async.RunSynchronously a

    let inline toAsync (x : Awaitable<'T>) =
        match x with
        | Sync v -> async.Return v
        | Async a -> a

    let inline map (mapper : 'T -> 'U) x =
        match x with
        | Sync v -> Sync (mapper v)
        | Async a ->
            async {
                let! v = a
                return mapper v
            } |> Async

    let inline bind (binder : 'T -> Awaitable<'U>) x =
        match x with
        | Sync v -> binder v
        | Async a ->
            async {
                let! v = a
                match binder v with
                | Sync v -> return v
                | Async a -> return! a
            } |> Async

    let inline bindAsync (binder : 'T -> Awaitable<'U>) (x : Async<'T>) =
        ofAsync x |> bind binder

    let inline bindTask (binder : 'T -> Awaitable<'U>) (x : Task<'T>) =
        ofTask x |> bind binder

    let inline bindPlainTask (binder : unit -> Awaitable<'T>) (x : Task) =
        ofPlainTask x |> bind binder

    let inline rescue (rescuer : exn -> Awaitable<'T>) x =
        match x with
        | Sync v -> Sync v
        | Async a ->
            async {
                try
                    return! a
                with ex ->
                    match rescuer ex with
                    | Sync v -> return v
                    | Async a -> return! a
            } |> Async

    let combine (awaitable2 : Awaitable<'T>) (awaitable1 : Awaitable<unit>) =
        match awaitable1 with
        | Sync _ -> awaitable2
        | Async a ->
            async {
                do! a
                match awaitable2 with
                | Sync v -> return v
                | Async a -> return! a
            } |> Async

    let forAll (binder : 'T -> Awaitable<unit>) (xs : seq<'T>) =
        if not (isNull xs || Seq.isEmpty xs)
        then xs |> Seq.map binder |> Seq.reduce combine
        else Sync ()

type AwaitableBuilder () =
    member inline __.Bind (awaitable : Awaitable<'T>, binder : 'T -> Awaitable<'U>) = Awaitable.bind binder awaitable

    member inline __.Bind (computation : Async<'T>, binder : 'T -> Awaitable<'U>) = Awaitable.bindAsync binder computation

    member inline __.Bind (task : Task<'T>, binder : 'T -> Awaitable<'U>) = Awaitable.bindTask binder task

    member inline __.Bind (task : Task, binder : unit -> Awaitable<'T>) = Awaitable.bindPlainTask binder task

    member inline __.Delay (producer : unit -> Awaitable<'T>) = producer ()

    member inline __.Return (value : 'T) = Awaitable.ofValue value

    member inline __.ReturnFrom (awaitable : Awaitable<'T>) = awaitable

    member inline __.ReturnFrom (computation : Async<'T>) = Awaitable.ofAsync computation

    member inline __.ReturnFrom (task : Task<'T>) = Awaitable.ofTask task

    member inline __.Combine (awaitable1, awaitable2) = Awaitable.combine awaitable2 awaitable1

    member __.For (sequence : seq<'T>, body) = Awaitable.forAll body sequence

    member inline __.TryWith (awaitable : Awaitable<'T>, rescuer) = Awaitable.rescue rescuer awaitable

    member inline __.TryFinally (awaitable : Awaitable<'T>, compensation) = compensation (); awaitable

    member inline __.Zero () = Awaitable.ofValue ()

[<AutoOpen>]
module AwaitableOperators =
    let awaitable = AwaitableBuilder ()