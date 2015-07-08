namespace ``Acceptance Tests``

open Xunit
open Swensen.Unquote
open Swensen.Unquote.Assertions
open MongoDB.Bson
open MongoDB.Driver
open MongoDB.Driver.Linq

open MongoDB.FSharp
open System.Linq
open System.Threading.Tasks
open Microsoft.FSharp.Linq

open TestUtils

type ObjectWithList() =
    member val Id : BsonObjectId = newObjectId() with get, set
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
    member val Id : BsonObjectId = newObjectId() with get, set
    member val Age : int option = None with get, set

type ObjectWithDimmer() =
    member val Id : BsonObjectId = newObjectId() with get, set
    member val Switch : DimmerSwitch = Off with get, set

type ObjectWithDimmers() =
    member val Id : BsonObjectId = newObjectId() with get, set
    member val Kitchen : DimmerSwitch = Off with get, set
    member val Bedroom1 : DimmerSwitch = Off with get, set
    member val Bedroom2 : DimmerSwitch = Off with get, set

type ``When serializing lists``() = 
    let db = MongoClient("mongodb://localhost/test").GetDatabase("test")
    do
        Serializers.Register()

    let newObjectId () = BsonObjectId(ObjectId.GenerateNewId())

    let insertOnePerson = insertOne (db.GetCollection "persons")
    let insertOneObject (o : 'T)  = insertOne (db.GetCollection "objects") o

    member this.findOne<'T> name id = 
      async {
        let collection = db.GetCollection<'T> name
        let! fromDb = findOneById collection id
        return fromDb
      }

    member this.findOnePerson<'T> id = 
      this.findOne<'T> "persons" id

    member this.findOneObject<'T> id = 
      this.findOne<'T> "objects" id

    interface System.IDisposable with
      member this.Dispose() = 
        async {
          do! db.DropCollectionAsync "objects" |> awaitTask
          do! db.DropCollectionAsync "persons" |> awaitTask
        } |> Async.RunSynchronously

    /// Seems to be fixed in version 1.5 of the C# driver
    [<Fact>]
    member this.``It can serialize an object with a list``() = 
        async {
          let obj = ObjectWithList()
          obj.List <- [ "hello"; "world" ]
          do! insertOneObject obj
          
          let! fromDb = this.findOneObject<BsonDocument> obj.Id
          let array = fromDb.["List"].AsBsonArray
          Assert.Equal(2, array.Count)
        } |> Async.RunSynchronously
        
    [<Fact>]
    member this.``It can deserialze lists``() =
        async {
          let list = BsonArray([ "hello"; "world" ])
          let id = newObjectId()
          let document = BsonDocument([ BsonElement("_id", id); BsonElement("List", list) ])
          do! insertOneObject document

          let! fromDb = this.findOneObject<ObjectWithList> id
          
          let array = fromDb.List
          Assert.Equal(2, array.Length)
        } |> Async.RunSynchronously

    [<Fact>]
    member this.``It can serialize records``() =
        async {
          let obj = { Id = newObjectId(); Name = "test"  }
          do! insertOneObject obj

          let! fromDb = this.findOneObject<BsonDocument> obj.Id
          let test = fromDb.["Name"].AsString
          Assert.Equal<string>("test", test)
        } |> Async.RunSynchronously

    [<Fact>]
    member this.``It can deserialize records``() =
        async {
          let id = newObjectId()
          let document = BsonDocument([BsonElement("_id", id); BsonElement("Name", BsonString("value"))])
          do! insertOneObject document

          let! fromDb = this.findOneObject<RecordType> id
          Assert.NotNull(fromDb)
          Assert.Equal<string>("value", fromDb.Name)
        } |> Async.RunSynchronously

    [<Fact>]
    member this.``It can serialize and deserialize nested records``() =
        async {
          let collection = db.GetCollection<Person> "persons"
          let obj = { Id = newObjectId(); PersonName = "test"; Age = 33; Childs = [{ChildName = "Adrian"; Age = 3}] }
          do! insertOnePerson obj

          let! person = this.findOnePerson obj.Id

          Assert.NotNull person
          Assert.Equal<string>("test",person.PersonName)
          Assert.Equal<int>(33,person.Age)
          Assert.Equal<int>(1 ,person.Childs |> Seq.length)

          let child = person.Childs |> Seq.head

          Assert.Equal<string>("Adrian", child.ChildName)
          Assert.Equal<int>(3, child.Age)
        } |> Async.RunSynchronously


    [<Fact>]
    member this.``It can serialize option types``() =
      async {
        let obj = ObjectWithOptions()
        obj.Age <- Some 42
        
        do! insertOneObject obj
        
        let! fromDb = this.findOneObject<BsonDocument> obj.Id

        let age = fromDb.GetElement("Age")
        Assert.NotNull(age);
        Assert.Equal<string>("Some", age.Value.AsBsonDocument.GetElement("_t").Value.AsString)
        let value = age.Value.AsBsonDocument.GetElement("_v").Value
        Assert.True(value.IsBsonArray)
        let array = value.AsBsonArray
        Assert.Equal(1, array.Count)
        Assert.Equal(42, array.[0].AsInt32)
      } |> Async.RunSynchronously

    [<Fact>]
    member this.``It can serialize DimmerSwitch types``() =
        async {
          let obj = ObjectWithDimmer()
          obj.Switch <- DimMarquee(42, "loser")

          do! insertOneObject obj

          let! fromDb = this.findOne<BsonDocument> "objects" obj.Id

          let switch = fromDb.GetElement("Switch")
          Assert.NotNull(switch);
          Assert.Equal<string>("DimMarquee", switch.Value.AsBsonDocument.GetElement("_t").Value.AsString)
          let value = switch.Value.AsBsonDocument.GetElement("_v").Value
          Assert.True(value.IsBsonArray)
          let array = value.AsBsonArray
          Assert.Equal(2, array.Count)
          Assert.Equal(42, array.[0].AsInt32)
          Assert.Equal<string>("loser", array.[1].AsString)
        } |> Async.RunSynchronously

    [<Fact>]
    member this.``It can deserialize option types``() =
      async {
        let id = newObjectId()
        let arrayPart = BsonArray([ BsonInt32(42) ])
        let structure = BsonDocument([BsonElement("_t", BsonString("Some")); BsonElement("_v", arrayPart)])
        let document = BsonDocument([BsonElement("_id", id); BsonElement("Age", structure)])

        do! insertOneObject document
        
        let! fromDb = this.findOneObject<ObjectWithOptions> id
        match fromDb.Age with
        | Some 42 -> ()
        | _ -> fail "expected Some 42 but got something else"
      } |> Async.RunSynchronously

    [<Fact>]
    member this.``We can integrate serialize & deserialize on DimmerSwitches``() =
      async {
        let obj = ObjectWithDimmers()
        obj.Kitchen <- Off
        obj.Bedroom1 <- Dim 42
        obj.Bedroom2 <- DimMarquee(12, "when I was little...")

        do! insertOneObject obj
        

        let! fromDb = this.findOneObject<ObjectWithDimmers> obj.Id
        match fromDb.Kitchen with
        | Off -> ()
        | _ -> fail "Kitchen light wasn't off"

        match fromDb.Bedroom1 with
        | Dim 42 -> ()
        | _ -> fail "Bedroom1 light wasn't dim enough"

        match fromDb.Bedroom2 with
        | DimMarquee(12, "when I was little...") -> ()
        | _ -> fail "Bedroom2 doesn't have the party we thought"
      } |> Async.RunSynchronously