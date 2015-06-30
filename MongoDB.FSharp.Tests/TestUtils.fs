module TestUtils

open Xunit
open System.Threading.Tasks

let fail msg =
    Assert.True(false, msg)

let awaitTask (t: Task) = t |> Async.AwaitIAsyncResult |> Async.Ignore