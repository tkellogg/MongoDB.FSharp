module MongoDB.Driver

open System
open MongoDB.Driver
open MongoDB.FSharp.Helpers

type MongoDB.Driver.IMongoCollection<'T> with 

    member this.FindOne (filter : FilterDefinition<_>) = 
      async {
        let! list = 
          filter
          |> this.Find
          |> fun x -> x.ToListAsync()
          |> Async.AwaitTask

        return list |> Seq.head
      }

    member this.FindOneById id = 
      let options = FindOptions()
      options.BatchSize <- !> 1

      async {
        let! list = 
          Builders<'T>.Filter.Eq(!> "_id", id)
          |> fun x -> this.Find (x, options)
          |> fun x -> x.ToListAsync()
          |> Async.AwaitTask

        return list |> Seq.head
      }

    member this.InsertOne doc = 
      async {        
          do! this.InsertOneAsync doc |> awaitTask
      }