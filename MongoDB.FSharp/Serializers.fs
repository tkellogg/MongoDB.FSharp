namespace MongoDB.FSharp

open System
open Microsoft.FSharp.Reflection
open MongoDB.Bson
open MongoDB.Bson.IO
open MongoDB.Bson.Serialization
open MongoDB.Bson.Serialization.Serializers

module Serializers =

    type MongoOptionSerializer<'T>() =
        inherit SerializerBase<'T option>()
        
        let contentSerializer = BsonSerializer.LookupSerializer(typeof<'T>)
        
        override my.Serialize(context, _, value) =
            let writer = context.Writer
            match value with
            | Some v -> contentSerializer.Serialize(context, v)
            | None -> writer.WriteNull()
            ()

        override my.Deserialize(context, _) =
            let reader = context.Reader
            let savePos = reader.GetBookmark()
            let readDefined() =
                if reader.CurrentBsonType = BsonType.Undefined
                then None
                else Some (contentSerializer.Deserialize(context) :?> 'T)
                
            match reader.CurrentBsonType with
            | BsonType.Null ->
                reader.ReadNull()
                None
            | BsonType.Document ->
                reader.ReadStartDocument()
                if reader.FindElement("_v") then
                    reader.ReadStartArray()
                    let result = readDefined()
                    if result.IsNone then reader.ReadUndefined()
                    reader.ReadEndArray()
                    reader.ReadEndDocument()
                    result
                else
                    if reader.CurrentBsonType = BsonType.EndOfDocument
                    then reader.ReadEndDocument()
                    else reader.ReturnToBookmark(savePos)
                    None
            | _ -> readDefined()
            
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
                
    let private getGenericArgumentOf baseType (typ: Type) =
        if typ.IsGenericType && typ.GetGenericTypeDefinition() = baseType
        then Some <| typ.GetGenericArguments()
        else None
        
    let inline private createInstance<'T> typ = Activator.CreateInstance(typ) :?> 'T
    let inline private makeGenericType<'T> typ = typedefof<'T>.MakeGenericType typ
        
    let specificSerializer<'nominal,'serializer> =
        getGenericArgumentOf typedefof<'nominal> >> Option.map (makeGenericType<'serializer> >> createInstance<IBsonSerializer>)
    let listSerializer typ = typ |> specificSerializer<List<_>, ListSerializer<_>>
    let simplisticOptionSerializer typ = typ |> specificSerializer<Option<_>, MongoOptionSerializer<_>>
    
    let unionCaseSerializer typ = Some (typ |> UnionCaseSerializer :> IBsonSerializer)
    
    type FsharpSerializationProvider(useOptionNull) =
        let serializers =
          seq {
              yield SourceConstructFlags.SumType, listSerializer
              if useOptionNull then yield SourceConstructFlags.SumType, simplisticOptionSerializer
              yield SourceConstructFlags.SumType, unionCaseSerializer
          } |> List.ofSeq

        interface IBsonSerializationProvider with
            member this.GetSerializer(typ : Type) =
                match fsharpType typ with
                | Some flag ->
                    serializers |> Seq.filter (fst >> (=) flag)
                                |> Seq.map snd
                                |> Seq.fold (fun result s -> result |> Option.orElseWith (fun _ -> s typ)) None
                | _ -> None
                |> Option.defaultValue null

    let mutable isRegistered = false
    
    type RegistrationOption = {
        UseOptionNull: bool
    }
    let defaultRegistrationOption = { UseOptionNull=true }

    /// Registers all F# serializers
    let RegisterWithOptions(opt) =
        if not isRegistered then
            BsonSerializer.RegisterSerializationProvider(FsharpSerializationProvider(opt.UseOptionNull))
            isRegistered <- true

type Serializers() =
    static member Register(?opts: Serializers.RegistrationOption) =
        Serializers.RegisterWithOptions(opts |> Option.defaultValue Serializers.defaultRegistrationOption)
