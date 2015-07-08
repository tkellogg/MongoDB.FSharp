namespace SerializersTests

open Xunit

open MongoDB.FSharp.Serializers
open TestUtils


type ``When registering classes``() =

    let stubbed = MongoDB.FSharp.Serializers.getClassMap (fun t -> false)
    
    [<Fact>]
    member this.``It sets serialization options``() =
        let classMap = stubbed typeof<List<string>>
        match classMap with
        | Some cm -> ()
        | None -> fail "expected a classmap"
