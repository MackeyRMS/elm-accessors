module Tuple.Accessors exposing (fst, snd)

{-|

@docs fst, snd

-}

import Base exposing (Lens_)


{-| Lens over the first component of a Tuple

    import Base exposing (get, set, map)
    import Tuple.Accessors as Tuple

    charging : (String, Int)
    charging = ("It's over", 1)

    get Tuple.fst charging
    --> "It's over"

    set Tuple.fst "It's over" charging
    --> ("It's over", 1)

    map Tuple.fst (\s -> String.toUpper s ++ "!!!") charging
    --> ("IT'S OVER!!!", 1)

-}
fst : Lens_ ls ( a, two ) ( b, two ) a b x y
fst =
    Base.lens ".1" Tuple.first (\( _, two ) a -> ( a, two ))


{-|

    import Base exposing (get, set, map)
    import Tuple.Accessors as Tuple

    meh : (String, Int)
    meh = ("It's over", 1)

    get Tuple.snd meh
    --> 1

    set Tuple.snd 1125 meh
    --> ("It's over", 1125)

    meh
        |> set Tuple.snd 1125
        |> map Tuple.fst (\s -> String.toUpper s ++ "!!!")
        |> map Tuple.snd ((*) 8)
    --> ("IT'S OVER!!!", 9000)

-}
snd : Lens_ ls ( one, a ) ( one, b ) a b x y
snd =
    Base.lens ".2" Tuple.second (\( one, _ ) b -> ( one, b ))
