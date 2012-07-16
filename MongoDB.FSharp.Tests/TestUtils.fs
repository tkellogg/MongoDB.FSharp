module TestUtils

open Xunit

let fail msg =
    Assert.True(false, msg)