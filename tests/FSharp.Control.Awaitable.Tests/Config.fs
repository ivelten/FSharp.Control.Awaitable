module FSharp.Control.Awaitable.Tests.Config

open Expecto

let defaultConfig = Gen.addToConfig FsCheckConfig.defaultConfig

let testProp name = testPropertyWithConfig defaultConfig name