namespace MongoDB.FSharp

open System
open MongoDB.Bson
open MongoDB.Bson.Serialization
open MongoDB.Bson.Serialization.Options
open MongoDB.Bson.Serialization.Serializers

module SerializationOptions =
  type System.Object with
      member inline this.GetFriendlyTypeName() =
          this.GetType() |> BsonUtils.GetFriendlyTypeName
