module TestUtils

open Xunit
open System.Threading.Tasks
open System

let fail msg =
    Assert.True(false, msg)

let AwaitVoidTask (task : Task) : Async<unit> =
    Async.FromContinuations(fun (cont, econt, ccont) ->
        task.ContinueWith(fun task ->
            if task.IsFaulted then econt task.Exception
            elif task.IsCanceled then ccont (OperationCanceledException())
            else cont ()) |> ignore)