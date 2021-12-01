namespace MongoDB.FSharp

open System
open System.Reflection
open Microsoft.FSharp.Reflection
open MongoDB.Bson
open MongoDB.Bson.IO
open MongoDB.Bson.Serialization
open MongoDB.Bson.Serialization.Serializers

module Serializers =

    type IBsonWriter with
        member inline this.WriteEmptyArray() =
            this.WriteStartArray()
            this.WriteEndArray()

    type ListSerializer<'T>() =
        inherit SerializerBase<list<'T>>()

        override this.Serialize(context, _, value) =
            let writer = context.Writer
            writer.WriteStartArray()
            
            value |> List.iter (fun x -> BsonSerializer.Serialize (writer,typeof<'T>, x))

            writer.WriteEndArray()

        override this.Deserialize(context, args) =
            let reader = context.Reader
            let readArray() =
                seq {
                    reader.ReadStartArray()
                    let convention = BsonSerializer.LookupDiscriminatorConvention(typeof<'T>)
                    while reader.ReadBsonType() <> BsonType.EndOfDocument do
                        let actualElementType = convention.GetActualType(reader, typeof<'T>)
                        let serializer = BsonSerializer.LookupSerializer(actualElementType)
                        let element = serializer.Deserialize(context, args)
                        yield element :?> 'T
                    reader.ReadEndArray()
                }

            let readArrayFromObject () =
                reader.ReadStartDocument()
                reader.ReadString("_t") |> ignore
                reader.ReadName("_v")
                let value = this.Deserialize(context, args)
                reader.ReadEndDocument()
                value
            
            let bsonType = reader.GetCurrentBsonType()
            match bsonType with
            | BsonType.Null ->
                reader.ReadNull()
                List.empty
            | BsonType.Array -> readArray() |> List.ofSeq
            | BsonType.Document -> readArrayFromObject ()
            | _ -> 
                let msg = $"Can't deserialize a %s{args.NominalType.FullName} from BsonType %s{bsonType.ToString()}"
                raise <| InvalidOperationException(msg)

        interface IBsonArraySerializer with
            member this.TryGetItemSerializationInfo(serializationInfo) =
                let elementName = null
                let nominalType = typeof<'T>
                let serializer = BsonSerializer.LookupSerializer nominalType
                serializationInfo <- BsonSerializationInfo(elementName, serializer, nominalType)
                true


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
            
    let createClassMapSerializer (type': Type) (classMap: BsonClassMap) =
        let concreteType = type'.MakeGenericType(classMap.ClassType)
        let ctor = concreteType.GetConstructor([| typeof<BsonClassMap> |])
        ctor.Invoke([| classMap |]) :?> IBsonSerializer

    type RecordSerializerBase(classMap : BsonClassMap) =
        let classMapSerializer = classMap |> createClassMapSerializer typedefof<BsonClassMapSerializer<_>>

        let getter = 
            match classMap.IdMemberMap with
            | null -> None
            | mm -> Some(mm.Getter)

        let idProvider = classMapSerializer :?> IBsonIdProvider

        member val _ClassMapSerializer = classMapSerializer
        
        interface IBsonSerializer with
            member _.ValueType = classMap.ClassType
            
            member _.Serialize(context, args, value) = classMapSerializer.Serialize(context, args, value)
            member _.Deserialize(context, args) = classMapSerializer.Deserialize(context, args)


        interface IBsonDocumentSerializer  with
            member this.TryGetMemberSerializationInfo(memberName, serializationInfo) = 
                let m = classMap.AllMemberMaps |> Seq.tryFind (fun x -> x.MemberName = memberName)
                match m with
                | Some(x) ->
                    serializationInfo <- BsonSerializationInfo(x.ElementName, x.GetSerializer(), x.MemberType)
                    true        
                | None -> 
                    raise <| ArgumentOutOfRangeException($"Class has no member called %s{memberName}")
                

        interface IBsonIdProvider with
            member this.GetDocumentId(document : Object, id : Object byref, nominalType : Type byref, idGenerator : IIdGenerator byref) =
                match getter with
                | Some(i) -> 
                    id <- i.DynamicInvoke(([document] |> Array.ofList))
                    idProvider.GetDocumentId(document, ref id, ref nominalType, ref idGenerator)
                | None -> false

            member this.SetDocumentId(document : Object, id : Object) = idProvider.SetDocumentId(document, id)

    type RecordSerializer<'T>(classMap : BsonClassMap) =
        inherit RecordSerializerBase(classMap)
        
        do assert (typeof<'T> = classMap.ClassType)
        
        member private my.Serializer = my._ClassMapSerializer :?> IBsonSerializer<'T>

        interface IBsonSerializer<'T> with
            member my.Serialize(context: BsonSerializationContext, args: BsonSerializationArgs, value: 'T) =
                my.Serializer.Serialize(context, args, value)
            member my.Deserialize(context, args) = my.Serializer.Deserialize(context, args)
    
    type UnionCaseSerializer(original) =

        let readItems context args (types : Type seq) =
            types |> Seq.fold(fun state t ->
                let serializer = BsonSerializer.LookupSerializer(t)
                let item = serializer.Deserialize(context, args)
                item :: state
            ) []
            |> Seq.toArray |> Array.rev

        interface IBsonSerializer with
            member _.ValueType = original
            
            member _.Serialize(context, args, value) =
                let writer = context.Writer
                writer.WriteStartDocument()
                let info, values = FSharpValue.GetUnionFields(value, args.NominalType)
                writer.WriteName("_t")
                writer.WriteString(info.Name)
                writer.WriteName("_v")
                writer.WriteStartArray()
                values |> Seq.zip(info.GetFields()) |> Seq.iter (fun (field, value) ->
                    let itemSerializer = BsonSerializer.LookupSerializer(field.PropertyType)
                    itemSerializer.Serialize(context, args, value)
                )
                writer.WriteEndArray()
                writer.WriteEndDocument()
                
            member _.Deserialize(context, args) =
                let reader = context.Reader
                reader.ReadStartDocument()
                reader.ReadName("_t")
                let typeName = reader.ReadString()
                let unionType = 
                    FSharpType.GetUnionCases(args.NominalType) 
                    |> Seq.where (fun case -> case.Name = typeName) |> Seq.head
                reader.ReadStartArray()
                let items = readItems context args (unionType.GetFields() |> Seq.map(fun f -> f.PropertyType))
                reader.ReadEndArray()
                reader.ReadEndDocument()
                FSharpValue.MakeUnion(unionType, items)

    type FsharpSerializationProvider() =

        interface IBsonSerializationProvider with
            member this.GetSerializer(typ : Type) =
                match fsharpType typ with
                | Some SourceConstructFlags.RecordType ->
                    match ensureClassMapRegistered typ with
                    | Some classMap -> classMap |> createClassMapSerializer typedefof<RecordSerializer<_>>
                    // return null means to try the next provider to see if it has a better answer
                    | None -> null

                // other F# types, when we're ready (list, seq, discriminated union)
                | Some SourceConstructFlags.SumType ->
                    // Maybe it's a list?
                    if typ.IsGenericType && typ.GetGenericTypeDefinition() = typedefof<List<_>> then
                         typedefof<ListSerializer<_>>.MakeGenericType(typ.GetGenericArguments())
                         |> Activator.CreateInstance :?> IBsonSerializer
                    elif FSharpType.IsUnion typ then
                        UnionCaseSerializer(typ) :> IBsonSerializer
                    else
                        null

                | Some SourceConstructFlags.UnionCase -> UnionCaseSerializer(typ) :> IBsonSerializer
                | _ -> null

    let mutable isRegistered = false

    /// Registers all F# serializers
    let Register() =
        if not isRegistered then
            BsonSerializer.RegisterSerializationProvider(FsharpSerializationProvider())
            BsonSerializer.RegisterGenericSerializerDefinition(typeof<list<_>>, typeof<ListSerializer<_>>)
            isRegistered <- true