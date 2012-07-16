namespace ``Acceptance Tests``

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

type DimmerSwitch =
    | Off
    | Dim of int
    | DimMarquee of int * string
    | On

type ObjectWithOptions() =
    member val Id : BsonObjectId = BsonObjectId.GenerateNewId() with get, set
    member val Age : int option = None with get, set

type ObjectWithDimmer() =
    member val Id : BsonObjectId = BsonObjectId.GenerateNewId() with get, set
    member val Switch : DimmerSwitch = Off with get, set

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
        let list = BsonArray([ "hello"; "world" ])
        let id = BsonObjectId.GenerateNewId()
        let document = BsonDocument([ BsonElement("_id", id); BsonElement("List", list) ])
        let collection = db.GetCollection "objects"
        collection.Save document |> ignore

        let collection = db.GetCollection<ObjectWithList> "objects"
        let fromDb = collection.FindOne(new QueryDocument("_id", id))
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

    [<Fact(Skip = "Relies on CSHARP-528 from official C# driver")>]
    member this.``It can deserialize records``() =
        let id = BsonObjectId.GenerateNewId()
        let document = BsonDocument([BsonElement("_id", id); BsonElement("Name", BsonString("value"))])
        let collection = db.GetCollection "objects"
        collection.Save(document) |> ignore

        let collection = db.GetCollection<RecordType>("objects")
        let fromDb = collection.FindOneById(id)
        Assert.NotNull(fromDb)
        Assert.Equal<string>("value", fromDb.Name)

    [<Fact>]
    member this.``It can serialize option types``() =
        let collection = db.GetCollection<ObjectWithOptions> "objects"
        let obj = ObjectWithOptions()
        obj.Age <- Some 42
        collection.Save obj |> ignore

        let collection = db.GetCollection "objects"
        let fromDb = collection.FindOneById(obj.Id)
        let age = fromDb.GetElement("Age")
        Assert.NotNull(age);
        Assert.Equal<string>("Some", age.Value.AsBsonDocument.GetElement("_t").Value.AsString)
        let value = age.Value.AsBsonDocument.GetElement("_v").Value
        Assert.True(value.IsBsonArray)
        let array = value.AsBsonArray
        Assert.Equal(1, array.Count)
        Assert.Equal(42, array.[0].AsInt32)

    [<Fact>]
    member this.``It can serialize DimmerSwitch types``() =
        let collection = db.GetCollection<ObjectWithOptions> "objects"
        let obj = ObjectWithDimmer()
        obj.Switch <- DimMarquee(42, "loser")
        collection.Save obj |> ignore

        let collection = db.GetCollection "objects"
        let fromDb = collection.FindOneById(obj.Id)
        let switch = fromDb.GetElement("Switch")
        Assert.NotNull(switch);
        Assert.Equal<string>("DimMarquee", switch.Value.AsBsonDocument.GetElement("_t").Value.AsString)
        let value = switch.Value.AsBsonDocument.GetElement("_v").Value
        Assert.True(value.IsBsonArray)
        let array = value.AsBsonArray
        Assert.Equal(2, array.Count)
        Assert.Equal(42, array.[0].AsInt32)
        Assert.Equal<string>("loser", array.[1].AsString)

