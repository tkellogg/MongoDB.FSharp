[MongoDB][1] is a popular NoSQL document oriented database. F# is a strongly
typed functional language for .NET, similar to OCaml or Haskell. There is a
great [driver for C#][2] that gives us most of the infrastructure that we need
for F#. However, F# has many special types to support the functional paradigm -
such as immutable lists, immutable record types and discriminated unions (to 
name a few).


Installation
============

[`Install-Package MongoDB.FSharp`](https://nuget.org/packages/MongoDB.FSharp/)


Usage
==========

The idea is to keep this as invisible as possible. The only touch point is 
bootstrapping. Most of the contents of this driver extension are just extra
serializers. To register all serializers:

    open MongoDB.FSharp
    Serializers.Register()

It can be done anywhere and as many times as you want. Although, it only 
actually registers anything on the first call.


What does this get you?
-----------------------

It just allows you to code F# and persist data without thinking about it. For
instance, disriminated unions:


```ocaml
type DimmerSwitch =
  | Off
  | Dim of int
  | DimMarquee of int * string
  | On

type ObjectWithDimmers() =
  member val Id : BsonObjectId = BsonObjectId.GenerateNewId() with get, set
  member val Kitchen : DimmerSwitch = Off with get, set
  member val Bedroom1 : DimmerSwitch = Off with get, set
  member val Bedroom2 : DimmerSwitch = Off with get, set

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
```


 [1]: http://www.mongodb.org/
 [2]: http://www.mongodb.org/display/DOCS/CSharp+Language+Center
