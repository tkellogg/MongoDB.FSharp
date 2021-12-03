module ``Acceptance Tests``

open Mongo2Go
open MongoDB.Bson
open MongoDB.Driver
open MongoDB.FSharp
open Xunit

module TestUtils =
    let newBsonObjectId() = ObjectId.GenerateNewId() |> BsonObjectId

    let findById id (collection: IMongoCollection<BsonDocument>) =
        let filter = FilterDefinition<BsonDocument>.op_Implicit(BsonDocument("_id", id))
        collection.Find(filter).ToList() |> Seq.head
        
open TestUtils

type ObjectWithOptions() =
    member val Id : BsonObjectId = newBsonObjectId() with get, set
    member val Age : int option = None with get, set

type ``Backward compatibility tests``() =
    let runner = MongoDbRunner.Start()
    let db = MongoClient(runner.ConnectionString).GetDatabase("IntegrationTest")
    do Serializers.Register({ UseOptionNull = false })

    interface System.IDisposable with
        member this.Dispose() = runner.Dispose()
        
    [<Fact>]
    member this.``It can serialize option types``() =
        let collection = db.GetCollection<ObjectWithOptions> "objects"
        let obj = ObjectWithOptions()
        obj.Age <- Some 42
        collection.InsertOne obj

        let collection = db.GetCollection<BsonDocument> "objects"
        let fromDb = collection |> findById obj.Id
        let age = fromDb.GetElement("Age")
        Assert.NotNull(age);
        Assert.Equal<string>("Some", age.Value.AsBsonDocument.GetElement("_t").Value.AsString)
        let value = age.Value.AsBsonDocument.GetElement("_v").Value
        Assert.True(value.IsBsonArray)
        let array = value.AsBsonArray
        Assert.Equal(1, array.Count)
        Assert.Equal(42, array.[0].AsInt32)

