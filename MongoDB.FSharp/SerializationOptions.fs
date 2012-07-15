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

  type ListSerializationOptions(itemOptions : IBsonSerializationOptions) =
      inherit BsonBaseSerializationOptions()
      let mutable itemSerializatonOptions = itemOptions
      new() = ListSerializationOptions(null)

      member val ItemSerializationOptions = itemSerializatonOptions

      override this.ApplyAttribute(serializer : IBsonSerializer, attribute : Attribute) =
          this.EnsureNotFrozen()
          let itemSerializer = 
              if (serializer :? IBsonArraySerializer) then
                  let fmt = sprintf "a serialization options attribute of type %s cannot be used with serializer of type %s."
                  let attrType = attribute.GetFriendlyTypeName()
                  let serializerType = serializer.GetFriendlyTypeName()
                  let msg = fmt attrType serializerType
                  raise(NotSupportedException(msg))

              (serializer :?> IBsonArraySerializer).GetItemSerializationInfo().Serializer

          let ensureSerializationOptions() =
              if itemSerializatonOptions = null then
                  let defaultOptions = itemSerializer.GetDefaultSerializationOptions()
                  if defaultOptions = null then
                      let fmt = sprintf "A serialization options attribute of type %s cannot be used when the serializer is of type %s and the item serializer is of type %s."
                      let msg = fmt (attribute.GetFriendlyTypeName()) (serializer.GetFriendlyTypeName()) 
                      raise(NotSupportedException(msg (itemSerializer.GetFriendlyTypeName())))

                  defaultOptions.Clone()
              else
                  itemSerializatonOptions

          itemSerializatonOptions <- ensureSerializationOptions()
          itemSerializatonOptions.ApplyAttribute(itemSerializer, attribute)

        override this.Clone() = 
            ListSerializationOptions(itemSerializatonOptions) :> IBsonSerializationOptions

