module TestUtils

open MongoDB.Bson
open MongoDB.Driver
open Xunit

let fail msg =
    Assert.True(false, msg)
    
let newBsonObjectId() = ObjectId.GenerateNewId() |> BsonObjectId

let findById id (collection: IMongoCollection<BsonDocument>) =
    let filter = FilterDefinition<BsonDocument>.op_Implicit(BsonDocument("_id", id))
    collection.Find(filter).ToList() |> Seq.head