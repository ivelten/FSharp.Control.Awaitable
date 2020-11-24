module FSharp.Control.Awaitable.Tests.Helpers

let asyncEquals a b =
    async {
        let! a' = a
        let! b' = b
        return a' = b'
    } |> Async.RunSynchronously

let asyncEqualsTask a b =
    async {
        let! a' = Async.AwaitTask a
        let! b' = b
        return a' = b'
    } |> Async.RunSynchronously

let asyncEqualsSync a b =
    async {
        let! b' = b
        return a = b'
    } |> Async.RunSynchronously

let taskEquals a b =
    asyncEquals (Async.AwaitTask a) (Async.AwaitTask b)

let taskEqualsSync a b =
    asyncEqualsSync a (Async.AwaitTask b)

let taskEqualsAsync a b =
    asyncEqualsTask b a

let mapAsync mapper x =
    async {
        let! x' = x
        return mapper x'
    }

let bindAsync binder x =
    async {
        let! x' = x
        return! binder x'
    }

let bindTask binder x =
    bindAsync binder (Async.AwaitTask x)