namespace SerializersTests

open Xunit

open MongoDB.FSharp.Serializers
open TestUtils


type ``When registering classes``() =

    let stubbed = getClassMap (fun _ -> false)
    
    [<Fact>]
    member this.``It sets serialization options``() =
        let classMap = stubbed typeof<List<string>>
        match classMap with
        | Some _ -> ()
        | None -> fail "expected a classmap"
