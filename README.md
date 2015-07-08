This project takes the [official .NET MongoDB driver][1] and fixes the warts that
make it hard to work with in F#. By default, F# loves immutable types but the 
MongoDB driver can't handle them out of the box. F# collections like list, map and seq 
aren't properly 


Installation
============

Note also that this requires at least version 1.8.1 of the official MongoDB driver. That
version includes some API that makes this whole project possible.

Install NuGet and run this from the Package Manager Console. 

    PM> Install-Package MongoDB.FSharp

To use the C# driver just type

    open MongoDB.Bson
    open MongoDB.Driver

Somewhere during startup you have to register MongoDB.FSharp. It's basically a one-liner 
that looks like:

    open MongoDB.FSharp
    Serializers.Register()

The `Serializers.Register()` call just registers serializers with the MongoDB driver.

Usage
=====

Use MongoDB.FSharp like you normally would in C#. 

```ocaml
type Person = { Id : BsonObjectId; Name : string; Scores : int list }

let connectionString = "mongodb://localhost"
let client = new MongoClient(connectionString)
let db = client.GetDatabase("test")

let collection = db.GetCollection<Person> "people"

let id = BsonObjectId(ObjectId.GenerateNewId())
do! insertOne collection { Id = id; Name = "George"; Scores = [13; 52; 6] }

let! george = findOneById collection id
```

The example above would work naturally in C#, but remember that record
types like `Person` are immutable! With MongoDB.FSharp, this just works.

What's Implemented
==================

Serializing and Deserializing these items should work great:

* list
* Options
* Discriminated unions
* Records

Road Map
========

* Better support for Linq and quotations
* A functional API for simpler access
* Better testing so you can be confident putting this in production

 [1]: http://www.mongodb.org/display/DOCS/CSharp+Language+Center
