namespace MongoDB.FSharp.Tests

open Xunit
open Swensen.Unquote
open Swensen.Unquote.Assertions
open MongoDB.Bson
open MongoDB.Driver
open MongoDB.FSharp

type ObjectWithList() =
    member val Id : BsonObjectId = BsonObjectId.GenerateNewId() with get, set
    member val List : string list = [] with get, set

type RecordType = {
    Id : BsonObjectId
    Name : string
}

type ``When serializing lists``() = 
    let db = MongoDatabase.Create "mongodb://localhost/test"
    do
        Serializers.Register()

    interface System.IDisposable with
        member this.Dispose() =
            db.DropCollection "objects" |> ignore

    /// Seems to be fixed in version 1.5 of the C# driver
    [<Fact>]
    member this.``It can serialize an object with a list``() =
        let collection = db.GetCollection<ObjectWithList> "objects"
        let obj = ObjectWithList()
        obj.List <- [ "hello"; "world" ]
        collection.Save obj |> ignore

        let genCollection = db.GetCollection "objects"
        let fromDb = genCollection.FindOne(new QueryDocument("_id", obj.Id))
        let array = fromDb.["List"].AsBsonArray
        Assert.Equal(2, array.Count)
        
    [<Fact>]
    member this.``It can deserialze lists``() =
        let collection = db.GetCollection<ObjectWithList> "objects"
        let obj = ObjectWithList()
        obj.List <- [ "hello"; "world" ]
        collection.Save obj |> ignore

        let fromDb = collection.FindOne(new QueryDocument("_id", obj.Id))
        let array = fromDb.List
        Assert.Equal(2, array.Length)

    [<Fact>]
    member this.``It can serialize records``() =
        let collection = db.GetCollection<RecordType> "objects"
        let obj = { Id = BsonObjectId.GenerateNewId(); Name = "test"  }
        collection.Save obj |> ignore

        let genCollection = db.GetCollection "objects"
        let fromDb = genCollection.FindOne(new QueryDocument("_id", obj.Id))
        let test = fromDb.["Name"].AsString
        Assert.Equal<string>("test", test)

