namespace FSharp.Control

module AsyncExtensions =
    type Async with
        static member inline Await (awaitable : Awaitable<'T>) =
            Awaitable.toAsync awaitable

    type AsyncBuilder with
        member inline __.Bind (awaitable : Awaitable<'T>, binder : 'T -> Async<'U>) =
            async {
                let! v = Async.Await awaitable
                return! binder v
            }

        member inline __.Delay (producer : unit -> Awaitable<'T>) =
            Async.Await (producer ())

        member inline __.ReturnFrom (awaitable : Awaitable<'T>) =
            Async.Await awaitable

        member inline __.Combine (awaitable1 : Awaitable<unit>, awaitable2 : Awaitable<'T>) =
            Awaitable.combine awaitable2 awaitable1
            |> Async.Await

        member __.For (sequence : seq<'T>, body) = 
            Awaitable.forAll body sequence
            |> Async.Await

        member inline __.TryWith (awaitable : Awaitable<'T>, rescuer) =
            Awaitable.rescue rescuer awaitable
            |> Async.Await

        member inline __.TryFinally (awaitable : Awaitable<'T>, compensation) =
            compensation ()
            Async.Await awaitable