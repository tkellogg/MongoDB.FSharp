module MongoDB.FSharp

open System
open System.Reflection
open Microsoft.FSharp.Reflection
open MongoDB.Bson
open MongoDB.Bson.IO
open MongoDB.Bson.Serialization
open MongoDB.Bson.Serialization.Options
open MongoDB.Bson.Serialization.Serializers
open MongoDB.Bson.Serialization.Conventions
open MongoDB.Driver

module Seq =
    let tryHead s =
        if Seq.isEmpty s then
            None
        else
            Some (s |> Seq.head)

module Serializers =

    type ListSerializer<'T>() =
        inherit MongoDB.Bson.Serialization.Serializers.SerializerBase<list<'T>>()

        let discriminatorConvention = ScalarDiscriminatorConvention("_t")
        let itemSerializer = BsonSerializer.LookupSerializer(typeof<'T>)

        override this.Deserialize(context : BsonDeserializationContext, args : BsonDeserializationArgs) =
            let reader = context.Reader
            let readArray() =
                let itemArgs = BsonDeserializationArgs(NominalType = typeof<'T>)
                seq {
                    reader.ReadStartArray()
                    let convention = BsonSerializer.LookupDiscriminatorConvention(typeof<'T>)
                    while reader.ReadBsonType() <> BsonType.EndOfDocument do
                        let element = itemSerializer.Deserialize(context, itemArgs)
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
                []
            | BsonType.Array -> readArray() |> List.ofSeq
            | BsonType.Document -> readArrayFromObject ()
            | _ -> 
                let msg = sprintf "Can't deserialize a %s from BsonType %s" typeof<list<'T>>.FullName (bsonType.ToString())
                raise(InvalidOperationException(msg))

        override this.Serialize(context : BsonSerializationContext, args : BsonSerializationArgs, value : list<'T>) =
            let writer = context.Writer
            match value with
            | [] -> 
                // There aren't supposed to be null values in F#
                //writer.WriteEmptyArray()
                writer.WriteStartArray()
                writer.WriteEndArray()
            | lst ->
                let itemArgs = BsonSerializationArgs(typeof<'T>, true, true)
                
                writer.WriteStartArray()                
                lst |> List.iter (fun x -> itemSerializer.Serialize (context, itemArgs, x))
                writer.WriteEndArray()

        interface IBsonArraySerializer with
            member this.TryGetItemSerializationInfo(serializationInfo : BsonSerializationInfo byref) =
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
        let rec getMember (_type : Type) names =
            match names with
            | [] -> None
            | x :: xs -> 
              let memberInfos = _type.GetMember x
              if not (memberInfos |> Seq.isEmpty) then
                  Some(Seq.head memberInfos)
              else getMember _type xs

        if not (isClassMapRegistered actualType) then
            let genericType = typedefof<BsonClassMap<_>>.MakeGenericType(actualType)
            let classMap = Activator.CreateInstance(genericType) :?> BsonClassMap

            classMap.AutoMap()

            // TODO: don't just map properties -> anything public, maybe consider using C#'s conventions to some extent
            actualType.GetProperties() 
            |> Seq.where (fun prop -> 
                classMap.AllMemberMaps 
                |> Seq.exists (fun mm -> mm.MemberInfo = (prop :> MemberInfo)) 
                |> not
            )
            |> Seq.where (fun prop -> prop.GetGetMethod() <> null)
            |> Seq.iter (fun prop -> classMap.MapMember(prop :> MemberInfo) |> ignore )

            // TODO: use conventions
            match getMember actualType ["Id"; "_id"] with
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

    let ensureClassMapRegistered typ =
        let fn = BsonClassMap.IsClassMapRegistered 
        match getClassMap fn typ with
        | Some map -> 
            map |> BsonClassMap.RegisterClassMap
            Some map
        | None -> 
            None

    type RecordSerializer<'T>(classMap : BsonClassMap) =
        inherit SerializerBase<'T>()

        let typ  = typeof<'T>
        let classMapSerializer =
            let bcmsType = typeof<BsonClassMapSerializer<'T>>
            let cmType = typeof<BsonClassMap<'T>>
            let ctor = bcmsType.GetConstructor([ cmType ] |> Seq.toArray)
            ctor.Invoke([ classMap :?> BsonClassMap<'T> ] |> Seq.cast<Object> |> Seq.toArray) :?> IBsonSerializer

        let getter = 
            match classMap.IdMemberMap with
            | null -> None
            | mm -> Some(mm.Getter)

        let idProvider = classMapSerializer :?> IBsonIdProvider

        
        override this.Serialize(context : BsonSerializationContext, args : BsonSerializationArgs, value : 'T) =
            classMapSerializer.Serialize(context, args, value)

        override this.Deserialize(context : BsonDeserializationContext, args : BsonDeserializationArgs) =
            classMapSerializer.Deserialize(context, args) :?> 'T

          
        interface IBsonDocumentSerializer  with
            member this.TryGetMemberSerializationInfo(memberName:string, serializationInfo : BsonSerializationInfo byref) = 
                let m = classMap.AllMemberMaps |> Seq.tryFind (fun x -> x.MemberName = memberName)
                match m with
                | Some(x) -> serializationInfo <- new BsonSerializationInfo(x.ElementName, x.GetSerializer(), x.MemberType)
                             true
                | None -> false
                

        interface IBsonIdProvider with
            member this.GetDocumentId(document : Object, id : Object byref, nominalType : Type byref, idGenerator : IIdGenerator byref) =
                match getter with
                | Some(i) -> 
                    id <- i.DynamicInvoke(([document] |> Array.ofList))
                    idProvider.GetDocumentId(document, ref id, ref nominalType, ref idGenerator)
                | None -> false

            member this.SetDocumentId(document : Object, id : Object) = idProvider.SetDocumentId(document, id)


    type UnionCaseSerializer<'T>() =    
        inherit SerializerBase<'T>()

        let readItems (context : BsonDeserializationContext) (types : Type seq) =
          types |> Seq.fold(fun state t ->
              let serializer = BsonSerializer.LookupSerializer(t)
              let mutable args = BsonDeserializationArgs()
              args.NominalType <- t
              let item = serializer.Deserialize(context, args)
              item :: state
          ) []
          |> Seq.toArray |> Array.rev

          
        override this.Deserialize(context : BsonDeserializationContext, args : BsonDeserializationArgs) =
            let reader = context.Reader
            let nominalType = args.NominalType

            reader.ReadStartDocument()
            reader.ReadName("_t")
            let typeName = reader.ReadString()
            let unionType = 
                FSharpType.GetUnionCases(nominalType) 
                |> Seq.where (fun case -> case.Name = typeName) |> Seq.head
            reader.ReadStartArray()
            let items = readItems context (unionType.GetFields() |> Seq.map(fun f -> f.PropertyType))
            reader.ReadEndArray()
            reader.ReadEndDocument()
            FSharpValue.MakeUnion(unionType, items) :?> 'T

        override this.Serialize(context : BsonSerializationContext, args : BsonSerializationArgs, value : 'T) =
            let writer = context.Writer
            let nominalType = args.NominalType

            writer.WriteStartDocument()
            let info, values = FSharpValue.GetUnionFields(value, nominalType)
            writer.WriteName("_t")
            writer.WriteString(info.Name)
            writer.WriteName("_v")
            writer.WriteStartArray()
            values |> Seq.zip(info.GetFields()) |> Seq.iter (fun (field, value) ->
                let itemSerializer = BsonSerializer.LookupSerializer(field.PropertyType)
                let args = BsonSerializationArgs(field.PropertyType, true, false)
                itemSerializer.Serialize(context, args, value)
            )
            writer.WriteEndArray()
            writer.WriteEndDocument()

              

    type FsharpSerializationProvider() =
        
        let makeUnionSerializer (typ : Type) =          
          typedefof<UnionCaseSerializer<_>>.MakeGenericType(typ)
          |> Activator.CreateInstance :?> IBsonSerializer

        interface IBsonSerializationProvider with
            member this.GetSerializer(typ : Type) =
                let t = fsharpType typ
                match t with
                | Some SourceConstructFlags.RecordType ->
                    match ensureClassMapRegistered typ with
                    | Some classMap -> 
                      let recType = typedefof<RecordSerializer<_>>.MakeGenericType(typ)
                      
                      let ctor = recType.GetConstructor( [typeof<BsonClassMap>] |> Seq.toArray )
                      ctor.Invoke([classMap] |> Seq.cast<Object> |> Seq.toArray) :?> IBsonSerializer
                    // return null means to try the next provider to see if it has a better answer
                    | None -> null

                // other F# types, when we're ready (list, seq, discriminated union)
                | Some SourceConstructFlags.SumType ->
                    // Maybe it's a list?
                    if typ.IsGenericType && typ.GetGenericTypeDefinition() = typedefof<List<_>> then
                         typedefof<ListSerializer<_>>.MakeGenericType(typ.GetGenericArguments())
                         |> Activator.CreateInstance :?> IBsonSerializer
                    elif FSharpType.IsUnion typ then
                        makeUnionSerializer typ
                    else
                        null

                | Some SourceConstructFlags.UnionCase -> makeUnionSerializer typ
                | _ -> null

    let mutable isRegistered = false

    /// Registers all F# serializers
    let Register() =
        if not isRegistered then
            BsonSerializer.RegisterSerializationProvider(FsharpSerializationProvider())
            BsonSerializer.RegisterGenericSerializerDefinition(typeof<list<_>>, typeof<ListSerializer<_>>)
            isRegistered <- true



let findOne (collection : IMongoCollection<_>) (filter : FilterDefinition<_>) = 
    async {
      let! list = 
        filter
        |> collection.Find
        |> fun x -> x.ToListAsync()
        |> Async.AwaitTask

      return list |> Seq.head
    }

let findOneById (collection : IMongoCollection<_>) id = 
  let options = FindOptions()
  options.BatchSize <- Nullable(1)      

  async {
    let! list = 
      Builders<'T>.Filter.Eq(StringFieldDefinition<'T, 'TField>("_id"), id)
      |> fun x -> collection.Find (x, options)
      |> fun x -> x.ToListAsync()
      |> Async.AwaitTask

    return list |> Seq.head
  }

let insertOne (collection : IMongoCollection<_>) doc = 
  async {        
      return! doc
      |> collection.InsertOneAsync  
      |> Async.AwaitIAsyncResult 
      |> Async.Ignore
  }