module VerifyExamples.Accessors.Err0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Accessors exposing (..)
import Lens as L
import Accessors exposing (..)



maybeRecord : { foo : Result String { bar : Int }, qux : Result String { bar : Int } }
maybeRecord = { foo = Ok { bar = 2 }
              , qux = Err "Not an Int"
              }



spec0 : Test.Test
spec0 =
    Test.test "#err: \n\n    over (L.qux << err) String.toUpper maybeRecord\n    --> { foo = Ok { bar = 2 }, qux = Err \"NOT AN INT\" }" <|
        \() ->
            Expect.equal
                (
                over (L.qux << err) String.toUpper maybeRecord
                )
                (
                { foo = Ok { bar = 2 }, qux = Err "NOT AN INT" }
                )