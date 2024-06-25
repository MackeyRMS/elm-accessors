module Accessors exposing
    ( Optic
    , Iso, Lens, Prism, Traversal
    , traversal, lens, prism, iso
    , get, all, try, has, is, map, over, set, new, name, to, from, swap
    , ixd
    , Iso_, Lens_, Prism_, Traversal_
    , An_Optic, An_Iso, A_Lens, A_Prism
    , An_Optic_, An_Iso_, A_Lens_, A_Prism_
    , just, ok, err
    , values, keyed, key, keyI, key_
    , each, eachIdx, at
    , every, everyIdx, ix
    , fst, snd
    , hexA, transparent, solid, oklch, oklab, elmui, hsluv
    )

{-| Accessors are a way of operating on nested data in Elm that doesn't require gobs of boilerplate.


## Optic: is the opaque underlying interface that enables the rest of the library to work.

@docs Optic


## Type Aliases: are shorthands from the Optics nomenclature that make writing your own accessors more convenient and hopefully easier to understand.

@docs Iso, Lens, Prism, Traversal


## Constructors

Accessors are built using these functions:

@docs traversal, lens, prism, iso


## Action functions

Action functions are functions that take an accessor and let you perform a
specific action on data using that accessor.

@docs get, all, try, has, is, map, over, set, new, name, to, from, swap


## Lifters for composing w/ indexed optics

@docs ixd


## Type aliases for custom and action functions

@docs Iso_, Lens_, Prism_, Traversal_
@docs An_Optic, An_Iso, A_Lens, A_Prism
@docs An_Optic_, An_Iso_, A_Lens_, A_Prism_


## Common Optics to mitigate `import` noise. Not everything is re-exported.

@docs just, ok, err
@docs values, keyed, key, keyI, key_
@docs each, eachIdx, at
@docs every, everyIdx, ix
@docs fst, snd


## Color Iso's :raised\_hands: wooo!!! For seemless composition of colors.

@docs hexA, transparent, solid, oklch, oklab, elmui, hsluv

-}

import Array exposing (Array)
import Array.Accessors as Array
import Base
import Color exposing (Color)
import Color.ElmUI.Accessors as ElmUI
import Color.HSLuv.Accessors as HSLuv
import Color.Hex.Accessors as Hex
import Color.Oklab exposing (Oklab)
import Color.Oklch exposing (Oklch)
import Color.Oklch.Accessors as Oklch
import Color.Palette.Accessors as Palette
import Dict exposing (Dict)
import Dict.Accessors as Dict
import Element as E
import HSLuv exposing (HSLuv)
import List.Accessors as List
import Maybe.Accessors as Maybe
import Result.Accessors as Result
import SolidColor exposing (SolidColor)
import TransparentColor exposing (TransparentColor)
import Tuple.Accessors as Tuple



-- Optics / Type Aliases


{-| Any Optic is both "lens" and "prism".
-}
type alias Optic pr ls s t a b x y =
    Base.Optic pr ls s t a b x y


{-| This MUST be a Prism or Iso
-}
type alias An_Optic pr ls s a =
    Base.An_Optic pr ls s a


{-| Any Optic
-}
type alias An_Optic_ pr ls s t a b =
    Base.An_Optic_ pr ls s t a b


{-| The isomorphism is both "lens" and "prism".
-}
type alias Iso pr ls s a x y =
    Base.Iso pr ls s a x y


{-| The isomorphism is both "lens" and "prism".
-}
type alias Iso_ pr ls s t a b x y =
    Base.Iso_ pr ls s t a b x y


{-| This MUST be a Prism or Iso
-}
type alias An_Iso s a =
    Base.An_Iso s a


{-| This MUST be a Prism or Iso
-}
type alias An_Iso_ s t a b =
    Base.An_Iso_ s t a b


{-| `Lens` that cannot change type of the object.
-}
type alias Lens ls s a x y =
    Base.Lens ls s a x y


{-| The lens is "not a prism".
-}
type alias Lens_ ls s t a b x y =
    Base.Lens_ ls s t a b x y


{-| This MUST be a non-type changing Lens or Iso
-}
type alias A_Lens pr s a =
    Base.A_Lens pr s a


{-| This MUST be a Lens or Iso
-}
type alias A_Lens_ pr s t a b =
    Base.A_Lens_ pr s t a b


{-| `Prism` that cannot change the type of the object.
-}
type alias Prism pr s a x y =
    Base.Prism pr s a x y


{-| The prism is "not a lens".
-}
type alias Prism_ pr s t a b x y =
    Base.Prism_ pr s t a b x y


{-| This MUST be a non-type changing Prism or Iso
-}
type alias A_Prism ls s a =
    Base.A_Prism ls s a


{-| This MUST be a Prism or Iso
-}
type alias A_Prism_ ls s t a b =
    Base.A_Prism_ ls s t a b


{-| `Traversal` that cannot change the type of the object.
-}
type alias Traversal s a x y =
    Base.Traversal s a x y


{-| The traversal is neither "lens" or "prism".
-}
type alias Traversal_ s t a b x y =
    Base.Traversal_ s t a b x y



-- Constructors


{-| An isomorphism constructor.
-}
iso :
    String
    -> (s -> a)
    -> (b -> t)
    -> Iso_ pr ls s t a b x y
iso =
    Base.iso


{-| This exposes a description field that's necessary for use with the name function
for getting unique names out of compositions of accessors. This is useful when you
want type safe keys for a Dictionary but you still want to use elm/core implementation.

    foo : Optic attr view over -> Optic { rec | foo : attr } view over
    foo =
        makeOneToOne
            ".foo"
            .foo
            (\change rec -> { rec | foo = change rec.foo })

-}
lens :
    String
    -> (s -> a)
    -> (s -> b -> t)
    -> Lens_ ls s t a b x y
lens =
    Base.lens


{-| A prism constructor.

Parameters are: reconstructor and a splitter.

Reconstructor takes a final value and constructs a final object.

The splitter turns initial object either to final object directly (if initial object is of wrong variant),
or spits out `a`.

-}
prism :
    String
    -> (b -> t)
    -> (s -> Result t a)
    -> Prism_ pr s t a b x y
prism =
    Base.prism


{-| This exposes a description field that's necessary for use with the name function
for getting unique names out of compositions of accessors. This is useful when you
want type safe keys for a Dictionary but you still want to use elm/core implementation.

    each : Optic pr ls a b x y -> Traversal (List a) (List b) x y
    each =
        Base.traversal "[]"
            identity
            List.map

-}
traversal :
    String
    -> (s -> List a)
    -> ((a -> b) -> s -> t)
    -> Traversal_ s t a b x y
traversal =
    Base.traversal



-- Lifters for composing w/ indexed optics


{-| Lift an optic over an indexed traversal
-}
ixd : An_Optic_ pr ls s t a b -> Traversal_ ( ix, s ) t a b x y
ixd =
    Base.ixd


{-| Get the inverse of an isomorphism
-}
from : An_Iso_ s t a b -> b -> t
from =
    Base.from


{-| Alias of `get` for isomorphisms
-}
to : An_Iso_ s t a b -> s -> a
to =
    Base.to


{-| swap the parent and target types of an iso for composition chains.
-}
swap : An_Iso_ s t a b -> Optic pr ls b a t s x y
swap i =
    iso (String.join "_" (List.reverse (String.split "_" (name i))))
        (from i)
        (to i)



-- Actions


{-| The get function takes:

  - An accessor,
  - A datastructure with type `super`
    and returns the value accessed by that combinator.

```
get (foo << bar) myRecord
```

-}
get : A_Lens_ pr s t a b -> s -> a
get =
    Base.get


{-| Used with a Prism, think of `!!` boolean coercion in Javascript except type safe.

    Just "Stuff"
        |> all just
    --> ["Stuff"]

    Nothing
        |> all just
    --> []

-}
all : An_Optic_ pr ls s t a b -> s -> List a
all =
    Base.all


{-| Used with a Prism, think of `!!` boolean coercion in Javascript except type safe.

    ["Stuff", "things"]
        |> try (at 2)
    --> Nothing

    ["Stuff", "things"]
        |> try (at 0)
    --> Just "Stuff"

-}
try : An_Optic_ pr ls s t a b -> s -> Maybe a
try =
    Base.try


{-| Used with a Prism, think of `!!` boolean coercion in Javascript except type safe.

    Just 1234
        |> has just
    --> True

    Nothing
        |> has just
    --> False

    [ "Wooo", "Things" ]
        |> has (at 7)
    --> False

    [ "Wooo", "Things" ]
        |> has (at 0)
    --> True

-}
has : An_Optic_ pr ls s t a b -> s -> Bool
has =
    Base.has


{-| alias for `has`
-}
is : An_Optic_ pr ls s t a b -> s -> Bool
is =
    Base.has


{-| The map function takes:

  - An accessor,
  - A function `(sub -> sub)`,
  - A datastructure with type `super`
    and it returns the data structure, with the accessible field changed by applying
    the function to the existing value.

```
map (foo << qux) ((+) 1) myRecord
```

-}
map : An_Optic_ pr ls s t a b -> (a -> b) -> s -> t
map =
    Base.map


{-| alias for `map`

    over (foo << qux) ((+) 1) myRecord

-}
over : An_Optic_ pr ls s t a b -> (a -> b) -> s -> t
over =
    Base.map


{-| The set function takes:

  - An accessor,
  - A value of the type `sub`,
  - A datastructure with type `super`
    and it returns the data structure, with the accessible field changed to be
    the set value.

```
set (foo << bar) "Hi!" myRecord
```

-}
set : An_Optic_ pr ls s t a b -> b -> s -> t
set =
    Base.set


{-| Use prism to reconstruct.
-}
new : A_Prism_ ls s t a b -> b -> t
new =
    Base.new


{-| This function gives the name of the function as a string...
-}
name : An_Optic_ pr ls s t a b -> String
name =
    Base.name



-- Common Accessors


{-| This accessor combinator lets you access values inside Maybe.

    import Accessors exposing (..)
    import Lens as L

    maybeRecord : { foo : Maybe { bar : Maybe {stuff : Maybe Int} }, qux : Maybe { bar : Maybe Int } }
    maybeRecord = { foo = Just { bar = Just { stuff = Just 2 } }
                  , qux = Nothing
                  }

    try (L.foo << just << L.bar << just << L.stuff) maybeRecord
    --> Just (Just 2 )

    try (L.qux << just << L.bar) maybeRecord
    --> Nothing

    map (L.foo << just << L.bar << just << L.stuff << just) ((+) 1) maybeRecord
    --> {foo = Just {bar = Just { stuff = Just 3 }}, qux = Nothing}

    map (L.qux << just << L.bar << just) ((+) 1) maybeRecord
    --> {foo = Just {bar = Just {stuff = Just 2}}, qux = Nothing}

-}
just : Prism_ pr (Maybe a) (Maybe b) a b x y
just =
    Maybe.just


{-| This accessor combinator lets you access values inside List.
alias for [`List.Accessors.each`](List-Accessors#each)
-}
each : Traversal_ (List a) (List b) a b x y
each =
    List.each


{-| This accessor lets you traverse a list including the index of each element
alias for [`List.Accessors.eachIdx`](List-Accessors#eachIdx)
-}
eachIdx : Traversal_ (List a) (List b) ( Int, a ) b x y
eachIdx =
    List.eachIdx


{-| at: Structure Preserving accessor over List members.
alias for [`List.Accessors.at`](List-Accessors#at)
-}
at : Int -> Traversal (List a) a x y
at =
    List.at


{-| This accessor combinator lets you access values inside Array.
alias for [`Array.Accessors.each`](Array-Accessors#each)

    import Accessors exposing (..)
    import Array exposing (Array)
    import Lens as L

    arrayRecord : {foo : Array {bar : Int}}
    arrayRecord =
        { foo =
            Array.fromList [{ bar = 2 }, { bar = 3 }, {bar = 4}]
        }

    all (L.foo << every << L.bar) arrayRecord
    --> [2, 3, 4]

    map (L.foo << every << L.bar) ((+) 1) arrayRecord
    --> {foo = Array.fromList [{bar = 3}, {bar = 4}, {bar = 5}]}

-}
every : Traversal_ (Array a) (Array b) a b x y
every =
    Array.each


{-| This accessor lets you traverse an Array including the index of each element
alias for [`Array.Accessors.eachIdx`](Array-Accessors#eachIdx)

    import Accessors exposing (..)
    import Lens as L
    import Array exposing (Array)

    arrayRecord : { foo : Array { bar : Int } }
    arrayRecord = { foo = [ {bar = 2}
                          , {bar = 3}
                          , {bar = 4}
                          ] |> Array.fromList
                  }

    multiplyIfGTOne : (Int, { bar : Int }) -> { bar : Int }
    multiplyIfGTOne ( idx, ({ bar } as rec) ) =
        if idx > 0 then
            { bar = bar * 10 }
        else
            rec


    all (L.foo << everyIdx) arrayRecord
    --> [(0, {bar = 2}), (1, {bar = 3}), (2, {bar = 4})]

    map (L.foo << everyIdx) multiplyIfGTOne arrayRecord
    --> {foo = [{bar = 2}, {bar = 30}, {bar = 40}] |> Array.fromList}

    all (L.foo << everyIdx << ixd L.bar) arrayRecord
    --> [2, 3, 4]

    map (L.foo << everyIdx << ixd L.bar) ((+) 1) arrayRecord
    --> {foo = [{bar = 3}, {bar = 4}, {bar = 5}] |> Array.fromList}

-}
everyIdx : Traversal_ (Array b) (Array c) ( Int, b ) c x y
everyIdx =
    Array.eachIdx


{-| alias for [`Array.Accessors.at`](Array-Accessors#at)

    import Accessors exposing (..)
    import Array exposing (Array)
    import Lens as L

    arr : Array { bar : String }
    arr = Array.fromList [{ bar = "Stuff" }, { bar =  "Things" }, { bar = "Woot" }]

    try (ix 1) arr
    --> Just { bar = "Things" }

    try (ix 9000) arr
    --> Nothing

    try (ix 0 << L.bar) arr
    --> Just "Stuff"

    set (ix 0 << L.bar) "Whatever" arr
    --> Array.fromList [{ bar = "Whatever" }, { bar =  "Things" }, { bar = "Woot" }]

    set (ix 9000 << L.bar) "Whatever" arr
    --> arr

-}
ix : Int -> Traversal (Array a) a x y
ix =
    Array.at


{-| This accessor lets you access values inside the Ok variant of a Result.
alias for [`Result.Accessors.onOk`](Result-Accessors#onOk)

    import Accessors exposing (..)
    import Lens as L

    maybeRecord : { foo : Result String { bar : Int }, qux : Result String { bar : Int } }
    maybeRecord = { foo = Ok { bar = 2 }
                  , qux = Err "Not an Int"
                  }

    try (L.foo << ok << L.bar) maybeRecord
    --> Just 2

    try (L.qux << ok << L.bar) maybeRecord
    --> Nothing

    map (L.foo << ok << L.bar) ((+) 1) maybeRecord
    --> { foo = Ok { bar = 3 }, qux = Err "Not an Int" }

    map (L.qux << ok << L.bar) ((+) 1) maybeRecord
    --> { foo = Ok { bar = 2 }, qux = Err "Not an Int" }

-}
ok : Prism_ pr (Result ignored a) (Result ignored b) a b x y
ok =
    Result.ok


{-| This accessor lets you access values inside the Err variant of a Result.
alias for [`Result.Accessors.onErr`](Result-Accessors#onErr)

    import Accessors exposing (..)
    import Lens as L

    maybeRecord : { foo : Result String { bar : Int }, qux : Result String { bar : Int } }
    maybeRecord = { foo = Ok { bar = 2 }
                  , qux = Err "Not an Int"
                  }

    try (L.foo << err) maybeRecord
    --> Nothing

    try (L.qux << err) maybeRecord
    --> Just "Not an Int"

    map (L.foo << err) String.toUpper maybeRecord
    --> { foo = Ok { bar = 2 }, qux = Err "Not an Int" }

    map (L.qux << err) String.toUpper maybeRecord
    --> { foo = Ok { bar = 2 }, qux = Err "NOT AN INT" }

-}
err : Prism_ pr (Result a ignored) (Result b ignored) a b x y
err =
    Result.err


{-| values: This accessor lets you traverse a Dict including the index of each element
alias for [`Dict.Accessors.each`](Dict-Accessors#each)

    import Accessors exposing (..)
    import Lens as L
    import Dict exposing (Dict)

    dictRecord : {foo : Dict String {bar : Int}}
    dictRecord = { foo = [ ("a", { bar = 2 })
                         , ("b", { bar = 3 })
                         , ("c", { bar = 4 })
                         ] |> Dict.fromList
                 }

    all (L.foo << values) dictRecord
    --> [{bar = 2}, {bar = 3}, {bar = 4}]

    map (L.foo << values << L.bar) ((*) 10) dictRecord
    --> {foo = [("a", {bar = 20}), ("b", {bar = 30}), ("c", {bar = 40})] |> Dict.fromList}

    all (L.foo << values << L.bar) dictRecord
    --> [2, 3, 4]

    map (L.foo << values << L.bar) ((+) 1) dictRecord
    --> {foo = [("a", {bar = 3}), ("b", {bar = 4}), ("c", {bar = 5})] |> Dict.fromList}

-}
values : Traversal_ (Dict key a) (Dict key b) a b x y
values =
    Dict.each


{-| keyed: This accessor lets you traverse a Dict including the index of each element
alias for [`Dict.Accessors.eachIdx`](Dict-Accessors#eachIdx)

    import Accessors exposing (..)
    import Lens as L
    import Dict exposing (Dict)

    dictRecord : {foo : Dict String {bar : Int}}
    dictRecord = { foo = [ ("a", { bar = 2 })
                         , ("b", { bar = 3 })
                         , ("c", { bar = 4 })
                         ] |> Dict.fromList
                 }

    multiplyIfA : (String, { bar : Int }) -> { bar : Int }
    multiplyIfA ( key, ({ bar } as rec) ) =
        if key == "a" then
            { bar = bar * 10 }
        else
            rec


    all (L.foo << keyed) dictRecord
    --> [("a", {bar = 2}), ("b", {bar = 3}), ("c", {bar = 4})]

    map (L.foo << keyed) multiplyIfA dictRecord
    --> {foo = [("a", {bar = 20}), ("b", {bar = 3}), ("c", {bar = 4})] |> Dict.fromList}

    all (L.foo << keyed << ixd L.bar) dictRecord
    --> [2, 3, 4]

    map (L.foo << keyed << ixd L.bar) ((+) 1) dictRecord
    --> {foo = [("a", {bar = 3}), ("b", {bar = 4}), ("c", {bar = 5})] |> Dict.fromList}

-}
keyed : Traversal_ (Dict key a) (Dict key b) ( key, a ) b x y
keyed =
    Dict.eachIdx


{-| key: NON-structure preserving accessor over Dict's
alias for [`Dict.Accessors.at`](Dict-Accessors#at)

In terms of accessors, think of Dicts as records where each field is a Maybe.

    import Dict exposing (Dict)
    import Accessors exposing (..)
    import Lens as L

    dict : Dict String {bar : Int}
    dict = Dict.fromList [("foo", {bar = 2})]

    get (key "foo") dict
    --> Just {bar = 2}

    get (key "baz") dict
    --> Nothing

    try (key "foo" << just << L.bar) dict
    --> Just 2

    set (key "foo") Nothing dict
    --> Dict.remove "foo" dict

    set (key "baz" << just << L.bar) 3 dict
    --> dict

-}
key : String -> Lens ls (Dict String a) (Maybe a) x y
key =
    Dict.at


{-| key: NON-structure preserving accessor over Dict's
alias for [`Dict.Accessors.id`](Dict-Accessors#id)

In terms of accessors, think of Dicts as records where each field is a Maybe.

    import Dict exposing (Dict)
    import Accessors exposing (..)
    import Lens as L

    dict : Dict Int {bar : Int}
    dict = Dict.fromList [(1, {bar = 2})]

    get (keyI 1) dict
    --> Just {bar = 2}

    get (keyI 0) dict
    --> Nothing

    try (keyI 1 << just << L.bar) dict
    --> Just 2

    set (keyI 1) Nothing dict
    --> Dict.remove 1 dict

    set (keyI 0 << just << L.bar) 3 dict
    --> dict

-}
keyI : Int -> Lens ls (Dict Int a) (Maybe a) x y
keyI =
    Dict.id


{-| `key_`: NON-structure preserving accessor over Dict's
alias for [`Dict.Accessors.at_`](Dict-Accessors#at_)

In terms of accessors, think of Dicts as records where each field is a Maybe.

    import Dict exposing (Dict)
    import Accessors exposing (..)
    import Lens as L

    dict : Dict Char {bar : Int}
    dict = Dict.fromList [('C', {bar = 2})]

    keyC : Char -> Lens ls (Dict Char { bar : Int })  (Maybe { bar : Int }) x y
    keyC =
        key_ String.fromChar

    get (keyC 'C') dict
    --> Just {bar = 2}

    get (keyC 'Z') dict
    --> Nothing

    try (keyC 'C' << just << L.bar) dict
    --> Just 2

    set (keyC 'C') Nothing dict
    --> Dict.remove 'C' dict

    set (keyC 'Z' << just << L.bar) 3 dict
    --> dict

-}
key_ : (comparable -> String) -> comparable -> Lens ls (Dict comparable a) (Maybe a) x y
key_ =
    Dict.at_


{-| Lens over the first component of a Tuple
alias for [`Tuple.Accessors.fst`](Tuple-Accessors#fst)

    import Accessors exposing (..)

    charging : (String, Int)
    charging = ("It's over", 1)

    get fst charging
    --> "It's over"

    set fst "It's over" charging
    --> ("It's over", 1)

    map fst (\s -> String.toUpper s ++ "!!!") charging
    --> ("IT'S OVER!!!", 1)

-}
fst : Lens_ ls ( a, two ) ( b, two ) a b x y
fst =
    Tuple.fst


{-| alias for [`Tuple.Accessors.snd`](Tuple-Accessors#snd)

    import Accessors exposing (..)

    meh : (String, Int)
    meh = ("It's over", 1)

    get snd meh
    --> 1

    set snd 1125 meh
    --> ("It's over", 1125)

    meh
        |> set snd 1125
        |> map fst (\s -> String.toUpper s ++ "!!!")
        |> map snd ((*) 8)
    --> ("IT'S OVER!!!", 9000)

-}
snd : Lens_ ls ( one, a ) ( one, b ) a b x y
snd =
    Tuple.snd


{-| solid: This accessor lets you convert between tesk9/palette TransparentColor && SolidColor

    import Color


    from transparent <| to transparent Color.red
    --> Color.red

-}
transparent : Iso pr ls Color TransparentColor x y
transparent =
    Palette.transparent


{-| solid: This accessor lets you convert between tesk9/palette TransparentColor && SolidColor

    import Color


    from (transparent << solid) <| to (transparent << solid) Color.red
    --> Color.red

-}
solid : Iso pr ls TransparentColor SolidColor x y
solid =
    Palette.solid


{-| hex: This accessor lets you convert between tesk9/palette TransparentColor && SolidColor

    import Color


    new (hexA << swap transparent) Color.red
    -->  "#CC0000"
    try (hexA << swap transparent) "#C00F" -- with alpha channel
    --> Just Color.red

-}
hexA : Base.Prism pr String TransparentColor x y
hexA =
    Hex.palette


{-| oklch: This accessor lets you convert between oklch & avh4/elm-color

    import Color
    import Color.Round as Round


    Color.blue |> to oklch |> from oklch |> Round.rgb
    --> Round.rgb Color.blue

-}
oklch : Iso pr ls Color Oklch x y
oklch =
    Oklch.color


{-| oklab: This accessor lets you convert between oklch & avh4/elm-color

    import Color
    import Color.Round as Round


     Color.blue |> to (oklch << oklab) |> from (oklch << oklab) |> Round.rgb
    --> Round.rgb Color.blue

-}
oklab : Iso pr ls Oklch Oklab x y
oklab =
    Oklch.oklab


{-| elmui: This accessor lets you convert between oklch & avh4/elm-color

    import Color


    from elmui <| to elmui Color.red
    --> Color.red

-}
elmui : Iso pr ls Color E.Color x y
elmui =
    ElmUI.color


{-| hsluv: This accessor lets you convert between oklch & avh4/elm-color

    import Color
    import Color.Round as Round


    Color.blue |> to hsluv |> from hsluv |> Round.rgb
    --> Round.rgb Color.blue

-}
hsluv : Iso pr ls Color HSLuv x y
hsluv =
    HSLuv.iso
