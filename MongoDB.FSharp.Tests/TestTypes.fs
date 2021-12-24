namespace ``Acceptance Tests``

open MongoDB.Bson
open TestUtils

type ObjectWithOptions() =
    member val Id : BsonObjectId = newBsonObjectId() with get, set
    member val Age : int option = None with get, set
