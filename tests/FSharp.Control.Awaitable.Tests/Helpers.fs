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

let mapAsync mapper x =
    async {
        let! x' = x
        return mapper x'
    }

let bindAsync (binder : 'a -> Async<'b>) (x : Async<'a>) =
    async {
        let! x' = x
        return! binder x'
    }