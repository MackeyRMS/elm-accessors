module VerifyExamples.Accessors.Try3 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Accessors exposing (..)
import Test.Accessors.Record as R
import Accessors exposing (..)



maybeRecord : { foo : Maybe { bar : Int }, qux : Maybe { bar : Int } }
maybeRecord = { foo = Just { bar = 2 }
              , qux = Nothing
              }



spec3 : Test.Test
spec3 =
    Test.test "#try: \n\n    get (R.foo << try << R.bar) maybeRecord\n    --> Just 2" <|
        \() ->
            Expect.equal
                (
                get (R.foo << try << R.bar) maybeRecord
                )
                (
                Just 2
                )