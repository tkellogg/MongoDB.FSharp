namespace MongoDB.FSharp

open System.Threading.Tasks

module Helpers =

  let inline (!>) (x:^a) : ^b = ((^a or ^b) : (static member op_Implicit : ^a -> ^b) x) 
  let inline awaitTask (t: Task) = t |> Async.AwaitIAsyncResult |> Async.Ignore