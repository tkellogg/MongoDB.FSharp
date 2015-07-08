module TestUtils

open Xunit
open MongoDB.Bson
open MongoDB.Driver
open System.Threading.Tasks


let fail msg =
    Assert.True(false, msg)


let newObjectId () =
  BsonObjectId(ObjectId.GenerateNewId())

let inline awaitTask (t: Task) = t |> Async.AwaitIAsyncResult |> Async.Ignore
