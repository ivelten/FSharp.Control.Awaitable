module FSharp.Control.Awaitable.Tests.Assertions

open Expecto

let require condition = Expect.isTrue condition "Test failed."