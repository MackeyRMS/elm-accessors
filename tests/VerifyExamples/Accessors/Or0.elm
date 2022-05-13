module VerifyExamples.Accessors.Or0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Accessors exposing (..)
import Lens as L
import Dict exposing (Dict)



dict : Dict String {bar : Int}
dict =
    Dict.fromList [("foo", {bar = 2})]



spec0 : Test.Test
spec0 =
    Test.test "#or: \n\n    get ((key \"baz\" << try << L.bar) |> or 0) dict\n    --> 0" <|
        \() ->
            Expect.equal
                (
                get ((key "baz" << try << L.bar) |> or 0) dict
                )
                (
                0
                )