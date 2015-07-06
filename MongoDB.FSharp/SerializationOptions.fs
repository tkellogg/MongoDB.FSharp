namespace MongoDB.Driver.FSharp

open MongoDB.Bson 

module SerializationOptions =
  type System.Object with
      member inline this.GetFriendlyTypeName() =
          this.GetType() |> BsonUtils.GetFriendlyTypeName
