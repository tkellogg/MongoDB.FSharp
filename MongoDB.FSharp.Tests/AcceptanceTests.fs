namespace ``Acceptance Tests``

open Mongo2Go
open Xunit
open Swensen.Unquote
open Swensen.Unquote.Assertions
open MongoDB.Bson
open MongoDB.Driver
open MongoDB.Driver.Linq

open MongoDB.FSharp
open System.Linq
open Microsoft.FSharp.Linq

open TestUtils

type ObjectWithList() =
    member val Id : BsonObjectId = BsonObjectId.GenerateNewId() with get, set
    member val List : string list = [] with get, set

type RecordType = {
    Id : BsonObjectId
    Name : string
}

type Child = {
    ChildName: string
    Age: int
}

type Person = {
    Id: BsonObjectId
    PersonName: string
    Age: int
    Childs: Child seq
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

type ObjectWithDimmers() =
    member val Id : BsonObjectId = BsonObjectId.GenerateNewId() with get, set
    member val Kitchen : DimmerSwitch = Off with get, set
    member val Bedroom1 : DimmerSwitch = Off with get, set
    member val Bedroom2 : DimmerSwitch = Off with get, set

type ``When serializing lists``() =
    let runner = MongoDbRunner.Start()
    let db = MongoClient(runner.ConnectionString).GetDatabase("IntegrationTest")
    do Serializers.Register()

    interface System.IDisposable with
        member this.Dispose() = runner.Dispose()

    [<Fact>]
    member this.``It can serialize an object with a list``() =
        let collection = db.GetCollection<ObjectWithList> "objects"
        let obj = ObjectWithList()
        obj.List <- [ "hello"; "world" ]
        collection.InsertOne(obj)

        let genCollection = db.GetCollection<ObjectWithList> "objects"
        let fromDb = genCollection.Find(fun x -> x.Id = obj.Id).ToList().First()
        let array = fromDb.List
        test <@ List.length array = 2 @>
        
    [<Fact>]
    member this.``It can deserialze lists``() =
        let list = BsonArray([ "hello"; "world" ])
        let id = BsonObjectId(ObjectId.GenerateNewId())
        let document = BsonDocument([ BsonElement("_id", id); BsonElement("List", list) ])
        let collection = db.GetCollection<BsonDocument> "objects"
        collection.InsertOne document

        let collection = db.GetCollection<ObjectWithList> "objects"
        let fromDb = collection.Find(fun x -> x.Id = id).ToList().First()
        let array = fromDb.List
        test <@ List.length array = 2 @>

    [<Fact>]
    member this.``It can serialize records``() =
        let collection = db.GetCollection<RecordType> "objects"
        let obj = { Id = BsonObjectId(ObjectId.GenerateNewId()); Name = "test"  }
        collection.InsertOne obj

        let genCollection = db.GetCollection<BsonDocument> "objects"
        let filter = FilterDefinition<BsonDocument>.op_Implicit(BsonDocument("_id", obj.Id))
        let fromDb = genCollection.Find(filter).ToList().First()
        let name = fromDb["Name"].AsString
        test <@ name = "test" @>

       (*
    [<Fact>]
    member this.``It can deserialize records``() =
        let id = BsonObjectId.GenerateNewId()
        let document = BsonDocument([BsonElement("_id", id); BsonElement("Name", BsonString("value"))])
        let collection = db.GetCollection "objects"
        collection.InsertOne(document)

        let collection = db.GetCollection<RecordType>("objects")
        let fromDb = collection.FindOneById(id)
        Assert.NotNull(fromDb)
        Assert.Equal<string>("value", fromDb.Name)

    [<Fact>]
    member this.``It can serialize and deserialize nested records``() =
        let collection = db.GetCollection<Person> "persons"
        let obj = { Id = BsonObjectId.GenerateNewId(); PersonName = "test"; Age = 33; Childs = [{ChildName = "Adrian"; Age = 3}] }
        collection.InsertOne obj

        let genCollection = db.GetCollection<Person> "persons"
        let person = query { 
            for p in genCollection.AsQueryable() do 
            where (p.Id = obj.Id) 
            select p
            headOrDefault
        }

        Assert.NotNull person
        Assert.Equal<string>("test",person.PersonName)
        Assert.Equal<int>(33,person.Age)
        Assert.Equal<int>(1 ,person.Childs |> Seq.length)

        let child = person.Childs |> Seq.head

        Assert.Equal<string>("Adrian", child.ChildName)
        Assert.Equal<int>(3, child.Age)


    [<Fact>]
    member this.``It can serialize option types``() =
        let collection = db.GetCollection<ObjectWithOptions> "objects"
        let obj = ObjectWithOptions()
        obj.Age <- Some 42
        collection.InsertOne obj

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
        collection.InsertOne obj |> ignore

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

    [<Fact>]
    member this.``It can deserialize option types``() =
        let id = BsonObjectId.GenerateNewId()
        let arrayPart = BsonArray([ BsonInt32(42) ])
        let structure = BsonDocument(BsonElement("_t", BsonString("Some")), BsonElement("_v", arrayPart))
        let document = BsonDocument(BsonElement("_id", id), BsonElement("Age", structure))
        let collection = db.GetCollection "objects"
        collection.InsertOne(document) |> ignore

        let collection = db.GetCollection<ObjectWithOptions> "objects"
        let fromDb = collection.FindOneById id
        match fromDb.Age with
        | Some 42 -> ()
        | _ -> fail "expected Some 42 but got something else"

    [<Fact>]
    member this.``We can integrate serialize & deserialize on DimmerSwitches``() =
        let collection = db.GetCollection<ObjectWithDimmers> "objects"
        let obj = ObjectWithDimmers()
        obj.Kitchen <- Off
        obj.Bedroom1 <- Dim 42
        obj.Bedroom2 <- DimMarquee(12, "when I was little...")
        collection.Save obj |> ignore

        let fromDb = collection.FindOneById obj.Id
        match fromDb.Kitchen with
        | Off -> ()
        | _ -> fail "Kitchen light wasn't off"

        match fromDb.Bedroom1 with
        | Dim 42 -> ()
        | _ -> fail "Bedroom1 light wasn't dim enough"

        match fromDb.Bedroom2 with
        | DimMarquee(12, "when I was little...") -> ()
        | _ -> fail "Bedroom2 doesn't have the party we thought"
        
       *) 
