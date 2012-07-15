namespace MongoDB.FSharp

open System
open MongoDB.Bson
open MongoDB.Bson.IO
open MongoDB.Bson.Serialization
open MongoDB.Bson.Serialization.Options
open MongoDB.Bson.Serialization.Serializers

open SerializationOptions

module Seq =
    let tryHead s =
        if Seq.isEmpty s then
            None
        else
            Some (s |> Seq.head)

module Serializers =

    type MongoDB.Bson.IO.BsonWriter with
        member inline this.WriteEmptyArray() =
            this.WriteStartArray()
            this.WriteEndArray()

    type ListSerializer<'T>() =
        inherit MongoDB.Bson.Serialization.Serializers.BsonBaseSerializer()

        override this.Serialize(writer : BsonWriter, nominalType : Type, value : Object, options : IBsonSerializationOptions) =
            if value = null then
                // There aren't supposed to be null values in F#
                writer.WriteEmptyArray()
            else
                let actualType = value.GetType()
                this.VerifyTypes(nominalType, actualType, typeof<list<'T>>)

                let lst = value :?> list<'T>
                writer.WriteStartArray()
                
                lst |> List.iter (fun item -> 
                    BsonSerializer.Serialize(writer, typeof<'T>)
                )

                writer.WriteEndArray()

        override this.Deserialize(reader : BsonReader, nominalType : Type, actualType : Type, options : IBsonSerializationOptions) =
            let serializationOptions = this.EnsureSerializationOptions<ListSerializationOptions>(options)
            let itemOptions = serializationOptions.ItemSerializationOptions
            let readItems() =
                seq {
                    reader.ReadStartDocument()
                    let convention = BsonSerializer.LookupDiscriminatorConvention(typeof<'T>)
                    while reader.ReadBsonType() <> BsonType.EndOfDocument do
                        let actualElementType = convention.GetActualType(reader, nominalType)
                        let serializer = BsonSerializer.LookupSerializer(actualElementType)
                        let element = serializer.Deserialize(reader, nominalType, itemOptions)
                        yield element :?> 'T
                    reader.ReadEndArray()
                }
            
            readItems() |> List.ofSeq :> Object

        interface IBsonArraySerializer with
            member this.GetItemSerializationInfo() : BsonSerializationInfo =
                let elementName = null
                let nominalType = typeof<'T>
                let serializer = BsonSerializer.LookupSerializer nominalType
                BsonSerializationInfo(elementName, serializer, nominalType, null)

    let ensureClassMapRegistered actualType =
        let rec getMember (_type : Type) name other =
            let memberInfos = _type.GetMember name
            if not (memberInfos |> Seq.isEmpty) then
                Some(Seq.head memberInfos)
            elif other <> null then
                getMember _type other null
            else
                None

        if not (BsonClassMap.IsClassMapRegistered actualType) then
            let genericType = typedefof<BsonClassMap<_>>.MakeGenericType(actualType)
            let classMap = Activator.CreateInstance(genericType) :?> BsonClassMap

            classMap.AutoMap()

            match getMember actualType "Id" "_id" with
            | Some memberInfo -> classMap.MapIdMember memberInfo |> ignore
            | None -> ()

            classMap.Freeze() |> BsonClassMap.RegisterClassMap

    type RecordSerializer() =
        inherit MongoDB.Bson.Serialization.Serializers.BsonBaseSerializer()


    type RecordSerializationProvider() =
        let recordType (typ : Type) =
            typ.GetCustomAttributes(typeof<CompilationMappingAttribute>, true) 
                |> Seq.cast<CompilationMappingAttribute>
                |> Seq.map(fun t -> t.SourceConstructFlags)
                |> Seq.tryHead

        let recordSerializer = lazy ()
            
        interface IBsonSerializationProvider with
            member this.GetSerializer(typ : Type) =
                match recordType typ with
                | Some SourceConstructFlags.RecordType ->
                    ensureClassMapRegistered typ
                    null // let BsonClassMapSerializerProvider pick it up. We've already
                         // done the work to ensure it's mapped correctly

                // other F# types, when we're ready (list, seq, discriminated union)
                | Some SourceConstructFlags.SumType ->
                    // Maybe it's a list?
                    if typ.IsGenericType && typ.GetGenericTypeDefinition() = typedefof<List<_>> then
                         typedefof<ListSerializer<_>>.MakeGenericType(typ.GetGenericArguments())
                         |> Activator.CreateInstance :?> IBsonSerializer
                    else
                        null
                | _ -> null

    let mutable isRegistered = false

    /// Registers all F# serializers
    let Register() =
        if not isRegistered then
            BsonSerializer.RegisterSerializationProvider(RecordSerializationProvider())
            BsonSerializer.RegisterGenericSerializerDefinition(typeof<list<_>>, typeof<ListSerializer<_>>)
            isRegistered <- true
