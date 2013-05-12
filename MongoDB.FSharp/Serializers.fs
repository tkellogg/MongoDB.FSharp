namespace MongoDB.FSharp

open System
open System.Reflection
open Microsoft.FSharp.Reflection
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
        inherit MongoDB.Bson.Serialization.Serializers.BsonBaseSerializer(ListSerializationOptions())

        override this.Serialize(writer : BsonWriter, nominalType : Type, value : Object, options : IBsonSerializationOptions) =
            if value = null then
                // There aren't supposed to be null values in F#
                writer.WriteEmptyArray()
            else
                let actualType = value.GetType()
                this.VerifyTypes(nominalType, actualType, typeof<list<'T>>)

                let lst = value :?> list<'T>
                writer.WriteStartArray()
                
                lst |> List.iter (fun x -> BsonSerializer.Serialize (writer,typeof<'T>, x))

                writer.WriteEndArray()

        override this.Deserialize(reader : BsonReader, nominalType : Type, actualType : Type, options : IBsonSerializationOptions) =
            let serializationOptions = this.EnsureSerializationOptions<ListSerializationOptions>(options)
            let itemOptions = serializationOptions.ItemSerializationOptions
            let readArray() =
                seq {
                    reader.ReadStartArray()
                    let convention = BsonSerializer.LookupDiscriminatorConvention(typeof<'T>)
                    while reader.ReadBsonType() <> BsonType.EndOfDocument do
                        let actualElementType = convention.GetActualType(reader, typeof<'T>)
                        let serializer = BsonSerializer.LookupSerializer(actualElementType)
                        let element = serializer.Deserialize(reader, typeof<'T>, itemOptions)
                        yield element :?> 'T
                    reader.ReadEndArray()
                }

            let readArrayFromObject () =
                reader.ReadStartDocument()
                reader.ReadString("_t") |> ignore
                reader.ReadName("_v")
                let value = this.Deserialize(reader, actualType, actualType, options)
                reader.ReadEndDocument()
                value
            
            let bsonType = reader.GetCurrentBsonType()
            match bsonType with
            | BsonType.Null ->
                reader.ReadNull()
                null
            | BsonType.Array -> readArray() |> List.ofSeq :> Object
            | BsonType.Document -> readArrayFromObject ()
            | _ -> 
                let msg = sprintf "Can't deserialize a %s from BsonType %s" actualType.FullName (bsonType.ToString())
                raise(InvalidOperationException(msg))

        interface IBsonArraySerializer with
            member this.GetItemSerializationInfo() : BsonSerializationInfo =
                let elementName = null
                let nominalType = typeof<'T>
                let serializer = BsonSerializer.LookupSerializer nominalType
                BsonSerializationInfo(elementName, serializer, nominalType, null)


    let fsharpType (typ : Type) =
        typ.GetCustomAttributes(typeof<CompilationMappingAttribute>, true) 
        |> Seq.cast<CompilationMappingAttribute>
        |> Seq.map(fun t -> t.SourceConstructFlags)
        |> Seq.tryHead

    let getClassMap isClassMapRegistered (actualType : Type) =
        let rec getMember (_type : Type) name other =
            let memberInfos = _type.GetMember name
            if not (memberInfos |> Seq.isEmpty) then
                Some(Seq.head memberInfos)
            elif other <> null then
                getMember _type other null
            else
                None

        if not (isClassMapRegistered actualType) then
            let genericType = typedefof<BsonClassMap<_>>.MakeGenericType(actualType)
            let classMap = Activator.CreateInstance(genericType) :?> BsonClassMap

            classMap.AutoMap()

            // TODO: don't just map properties -> anything public, maybe consider using C#'s conventions to some extent
            actualType.GetProperties() 
            |> Seq.where (fun prop -> 
                classMap.AllMemberMaps |> Seq.exists (fun mm -> mm.MemberInfo = (prop :> MemberInfo)) |> not
            )
            |> Seq.where (fun prop -> prop.GetGetMethod() <> null)
            |> Seq.iter (fun prop -> classMap.MapMember(prop :> MemberInfo) |> ignore )

            // TODO: use conventions
            match getMember actualType "Id" "_id" with
            | Some memberInfo -> classMap.MapIdMember memberInfo |> ignore
            | None -> ()

            match fsharpType actualType with 
            | Some SourceConstructFlags.RecordType -> 
                // Map creator function. Requires Mongo >1.8
                match actualType.GetConstructors() |> Seq.sortBy (fun c -> c.GetParameters().Length) |> Seq.tryHead with
                | Some c -> 
                    let parms = classMap.DeclaredMemberMaps |> Seq.map (fun m -> m.ElementName) |> Array.ofSeq
                    classMap.MapConstructor (c, parms) |> ignore
                | None -> ()
            | _ -> ()

            classMap.Freeze() |> Some
        else 
            None

    let ensureClassMapRegistered actualType =
        let fn = BsonClassMap.IsClassMapRegistered 
        match getClassMap fn actualType with
        | Some map -> 
            map |> BsonClassMap.RegisterClassMap
            Some map
        | None -> 
            None

    type RecordSerializer(classMap : BsonClassMap) =
        inherit MongoDB.Bson.Serialization.Serializers.BsonBaseSerializer()

        let classMapSerializer =
            let typ = typeof<BsonClassMap>.Assembly.GetType("MongoDB.Bson.Serialization.BsonClassMapSerializer")
            let ctor = typ.GetConstructor([ typeof<BsonClassMap> ] |> Seq.toArray)
            ctor.Invoke([ classMap ] |> Seq.cast<Object> |> Seq.toArray) :?> IBsonSerializer

        let getter = 
            match classMap.IdMemberMap with
            | null -> None
            | mm -> Some(mm.Getter)

        let idProvider = classMapSerializer :?> IBsonIdProvider

        override this.Serialize(writer : BsonWriter, nominalType : Type, value : Object, options : IBsonSerializationOptions) =
            classMapSerializer.Serialize(writer, nominalType, value, options)

        override this.Deserialize(reader : BsonReader, nominalType : Type, options : IBsonSerializationOptions) =
            classMapSerializer.Deserialize(reader, nominalType, options)

        override this.Deserialize(reader : BsonReader, nominalType : Type, actualType : Type, options : IBsonSerializationOptions) =
            classMapSerializer.Deserialize(reader, nominalType, actualType, options)


        interface IBsonDocumentSerializer  with
            member this.GetMemberSerializationInfo(memberName:string) = 
                let m = classMap.AllMemberMaps |> Seq.tryFind (fun x -> x.MemberName = memberName)
                match m with
                | Some(x) -> new BsonSerializationInfo(x.ElementName, x.GetSerializer(x.MemberType), x.MemberType, x.SerializationOptions)
                | None -> 
                    let msg = sprintf "Class has no member called %s" memberName 
                    raise(ArgumentOutOfRangeException(msg))       
                

        interface IBsonIdProvider with
            member this.GetDocumentId(document : Object, id : Object byref, nominalType : Type byref, idGenerator : IIdGenerator byref) =
                match getter with
                | Some(i) -> 
                    id <- i.DynamicInvoke(([document] |> Array.ofList))
                    idProvider.GetDocumentId(document, ref id, ref nominalType, ref idGenerator)
                | None -> false

            member this.SetDocumentId(document : Object, id : Object) = idProvider.SetDocumentId(document, id)


    type UnionCaseSerializer() =
        inherit MongoDB.Bson.Serialization.Serializers.BsonBaseSerializer()

        let readItems (reader : BsonReader) (types : Type seq) (options : IBsonSerializationOptions) =
            types |> Seq.fold(fun state t ->
                let serializer = BsonSerializer.LookupSerializer(t)
                let item = serializer.Deserialize(reader, t, options)
                item :: state
            ) []
            |> Seq.toArray |> Array.rev

        override this.Serialize(writer : BsonWriter, nominalType : Type, value : Object, options : IBsonSerializationOptions) =
            writer.WriteStartDocument()
            let info, values = FSharpValue.GetUnionFields(value, nominalType)
            writer.WriteName("_t")
            writer.WriteString(info.Name)
            writer.WriteName("_v")
            writer.WriteStartArray()
            values |> Seq.zip(info.GetFields()) |> Seq.iter (fun (field, value) ->
                let itemSerializer = BsonSerializer.LookupSerializer(field.PropertyType)
                itemSerializer.Serialize(writer, field.PropertyType, value, options)
            )
            writer.WriteEndArray()
            writer.WriteEndDocument()

        override this.Deserialize(reader : BsonReader, nominalType : Type, actualType : Type, options : IBsonSerializationOptions) =
            reader.ReadStartDocument()
            reader.ReadName("_t")
            let typeName = reader.ReadString()
            let unionType = 
                FSharpType.GetUnionCases(nominalType) 
                |> Seq.where (fun case -> case.Name = typeName) |> Seq.head
            reader.ReadStartArray()
            let items = readItems reader (unionType.GetFields() |> Seq.map(fun f -> f.PropertyType)) options
            reader.ReadEndArray()
            reader.ReadEndDocument()
            FSharpValue.MakeUnion(unionType, items)

    type FsharpSerializationProvider() =

        interface IBsonSerializationProvider with
            member this.GetSerializer(typ : Type) =
                let t = fsharpType typ
                match t with
                | Some SourceConstructFlags.RecordType ->
                    match ensureClassMapRegistered typ with
                    | Some classMap -> RecordSerializer(classMap) :> IBsonSerializer
                    // return null means to try the next provider to see if it has a better answer
                    | None -> null

                // other F# types, when we're ready (list, seq, discriminated union)
                | Some SourceConstructFlags.SumType ->
                    // Maybe it's a list?
                    if typ.IsGenericType && typ.GetGenericTypeDefinition() = typedefof<List<_>> then
                         typedefof<ListSerializer<_>>.MakeGenericType(typ.GetGenericArguments())
                         |> Activator.CreateInstance :?> IBsonSerializer
                    elif FSharpType.IsUnion typ then
                        UnionCaseSerializer() :> IBsonSerializer
                    else
                        null

                | Some SourceConstructFlags.UnionCase -> UnionCaseSerializer() :> IBsonSerializer
                | _ -> null

    let mutable isRegistered = false

    /// Registers all F# serializers
    let Register() =
        if not isRegistered then
            BsonSerializer.RegisterSerializationProvider(FsharpSerializationProvider())
            BsonSerializer.RegisterGenericSerializerDefinition(typeof<list<_>>, typeof<ListSerializer<_>>)
            isRegistered <- true