namespace MongoDB.Driver.FSharp

open System
open System.Reflection
open Microsoft.FSharp.Reflection
open MongoDB.Bson
open MongoDB.Bson.IO
open MongoDB.Bson.Serialization 
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

    type ListSerializer<'T when 'T:equality>() = 
        inherit SerializerBase<list<'T>>()

        override this.Deserialize(context:BsonDeserializationContext , args:BsonDeserializationArgs) : 'T list =        

            let readArray() =
                seq {
                    context.Reader.ReadStartArray()
                    let convention = BsonSerializer.LookupDiscriminatorConvention(typeof<'T>)
                    while context.Reader.ReadBsonType() <> BsonType.EndOfDocument do
                        let actualElementType = convention.GetActualType(context.Reader, typeof<'T>)
                        let serializer = BsonSerializer.LookupSerializer(actualElementType)
                        let element = serializer.Deserialize(context, args)
                        yield element :?> 'T
                    context.Reader.ReadEndArray()
                }

            let readArrayFromObject () =
                context.Reader.ReadStartDocument()
                context.Reader.ReadString("_t") |> ignore
                context.Reader.ReadName("_v")
                let value = this.Deserialize(context, args)
                context.Reader.ReadEndDocument()
                value
            
            let bsonType = context.Reader.GetCurrentBsonType()
            match bsonType with
            | BsonType.Null ->
                context.Reader.ReadNull()
                Unchecked.defaultof<'T list>
            | BsonType.Array -> (readArray() |> List.ofSeq :> obj) :?> 'T list
            | BsonType.Document -> readArrayFromObject ()
            | _ -> 
                let msg = sprintf "Can't deserialize a %s from BsonType %s" args.NominalType.FullName (bsonType.ToString())
                raise(InvalidOperationException(msg))
        
        override this.Serialize(context:BsonSerializationContext,  args:BsonSerializationArgs, value:'T list) =
            if value = Unchecked.defaultof<'T list> then
                // There aren't supposed to be null values in F#
                context.Writer.WriteStartArray()
                context.Writer.WriteEndArray()
            else
                let actualType = value.GetType()
                //this.VerifyTypes(nominalType, actualType, typeof<list<'T>>)

                let lst = box value :?> list<'T>
                context.Writer.WriteStartArray()
                
                lst |> List.iter (fun x -> BsonSerializer.Serialize (context.Writer,typeof<'T>, x))

                context.Writer.WriteEndArray()



        interface IBsonArraySerializer with            
            member x.TryGetItemSerializationInfo(serializationInfo: byref<BsonSerializationInfo>): bool = 
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

    type RecordSerializer<'T>(classMap : BsonClassMap) =
        inherit MongoDB.Bson.Serialization.BsonClassMapSerializer<'T>(classMap)

        let classMapSerializer =
            Activator.CreateInstance((typedefof<MongoDB.Bson.Serialization.BsonClassMapSerializer<_>>.MakeGenericType(typedefof<'T>)),
                                            [ classMap ] |> Seq.cast<Object> |> Seq.toArray) :?> IBsonSerializer
        let getter = 
            match classMap.IdMemberMap with
            | null -> None
            | mm -> Some(mm.Getter)

        let idProvider = classMapSerializer :?> IBsonIdProvider
        override this.Serialize(ctx:BsonSerializationContext, args:BsonSerializationArgs, value:'T) = 
            classMapSerializer.Serialize(ctx,args,value)

        interface IBsonDocumentSerializer  with            
            member x.TryGetMemberSerializationInfo(memberName: string, serializationInfo: byref<BsonSerializationInfo>): bool = 
                let m = classMap.AllMemberMaps |> Seq.tryFind (fun x -> x.MemberName = memberName)
                match m with
                    | Some(x) -> 
                        serializationInfo <- (new BsonSerializationInfo(x.ElementName, x.GetSerializer(), x.MemberType))
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

        let readItems (types : Type seq) (ctx:BsonDeserializationContext)  =
            types |> Seq.fold(fun state t ->
                let serializer = BsonSerializer.LookupSerializer(t)
                let mutable a = new BsonDeserializationArgs()
                a.NominalType <- t
                let item = serializer.Deserialize(ctx, a)
                item :: state
            ) []
            |> Seq.toArray |> Array.rev

        override x.Deserialize(ctx:BsonDeserializationContext, args:BsonDeserializationArgs) =
            let reader = ctx.Reader
            reader.ReadStartDocument()
            reader.ReadName("_t")
            let typeName = reader.ReadString()
            let unionType = 
                FSharpType.GetUnionCases(args.NominalType) 
                |> Seq.where (fun case -> case.Name = typeName) |> Seq.head
            reader.ReadStartArray()
            let items = readItems (unionType.GetFields() |> Seq.map(fun f -> f.PropertyType)) ctx
            reader.ReadEndArray()
            reader.ReadEndDocument()
            FSharpValue.MakeUnion(unionType, items) :?> 'T

        override  x.Serialize(ctx:BsonSerializationContext, args:BsonSerializationArgs, value:'T) =
            let writer = ctx.Writer
            writer.WriteStartDocument()
            let info, values = FSharpValue.GetUnionFields(value, value.GetType())
            writer.WriteName("_t")
            writer.WriteString(info.Name)
            writer.WriteName("_v")
            writer.WriteStartArray()
            values |> Seq.zip(info.GetFields()) |> Seq.iter (fun (field, value) ->
                let itemSerializer = BsonSerializer.LookupSerializer(field.PropertyType)
                itemSerializer.Serialize(ctx, value)
            )
            writer.WriteEndArray()
            writer.WriteEndDocument()


    type FsharpSerializationProvider() =

        interface IBsonSerializationProvider with
            member this.GetSerializer(typ : Type) =
                let t = fsharpType typ
                match t with
                | Some SourceConstructFlags.RecordType ->
                    match ensureClassMapRegistered typ with
                    | Some classMap ->  Activator.CreateInstance(
                                            (typedefof<RecordSerializer<_>>.MakeGenericType(typ)),
                                            classMap) :?> IBsonSerializer
                    // return null means to try the next provider to see if it has a better answer
                    | None -> null

                // other F# types, when we're ready (list, seq, discriminated union)
                | Some SourceConstructFlags.SumType ->
                    // Maybe it's a list?
                    if typ.IsGenericType && typ.GetGenericTypeDefinition() = typedefof<List<_>> then
                         typedefof<ListSerializer<_>>.MakeGenericType(typ.GetGenericArguments())
                         |> Activator.CreateInstance :?> IBsonSerializer
                    elif FSharpType.IsUnion typ then
                         typedefof<UnionCaseSerializer<_>>.MakeGenericType(typ)
                         |> Activator.CreateInstance :?> IBsonSerializer
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