module SelectList.Accessors exposing (each, eachIdx, selected)

{-| This module exposes some helpers for "miyamoen/select-list"

@docs each, eachIdx, selected

-}

import Base exposing (Lens, Traversal)
import SelectList exposing (SelectList)


{-| This accessor combinator lets you access values inside List.

    import Accessors exposing (..)
    import SelectList.Accessors as SL
    import Lens as L
    import SelectList exposing (SelectList)

    listRecord : { foo : SelectList { bar : Int } }
    listRecord =
        { foo = SelectList.fromLists [{ bar = 1 }] { bar = 2 } [{ bar = 3 }, { bar = 4 }]
        }

    all (L.foo << SL.each << L.bar) listRecord
    --> [1, 2, 3, 4]

    map (L.foo << SL.each << L.bar) ((+) 1) listRecord
    --> { foo = SelectList.fromLists [{ bar = 2 }] { bar = 3 } [{ bar = 4 }, { bar = 5 }] }

-}
each : Traversal a b x y -> Traversal (SelectList a) (SelectList b) x y
each =
    Base.traversal ":[_]" SelectList.toList SelectList.map


{-| This accessor lets you traverse a list including the index of each element

    import Accessors exposing (..)
    import SelectList.Accessors as SL
    import Lens as L
    import SelectList exposing (SelectList)

    listRecord : { foo : SelectList { bar : Int } }
    listRecord =
        { foo = SelectList.fromLists [{ bar = 1 }] { bar = 2 } [{ bar = 3 }, { bar = 4 }]
        }

    multiplyIfGTOne : (Int, { bar : Int }) -> { bar : Int }
    multiplyIfGTOne ( idx, ({ bar } as rec) ) =
        if idx > 0 then
            { bar = bar * 10 }
        else
            rec


    all (L.foo << SL.eachIdx) listRecord
    --> [(0, {bar = 1}), (1, {bar = 2}), (2, {bar = 3}), (3, {bar = 4})]

    map (L.foo << SL.eachIdx) multiplyIfGTOne listRecord
    --> { foo = SelectList.fromLists [{ bar = 1 }] { bar = 20 } [{ bar = 30 }, { bar = 40 }] }

    all (L.foo << SL.eachIdx << ixd L.bar) listRecord
    --> [1, 2, 3, 4]

    map (L.foo << SL.eachIdx << ixd L.bar) ((+) 1) listRecord
    --> {foo = SelectList.fromLists [{bar = 2}] {bar = 3} [{bar = 4}, {bar = 5}]}

-}
eachIdx : Traversal ( Int, a ) c x y -> Traversal (SelectList a) (SelectList c) x y
eachIdx =
    Base.traversal "[#]"
        (SelectList.toList >> List.indexedMap Tuple.pair)
        (\fn ls ->
            let
                ( before, current, after ) =
                    SelectList.toTuple ls

                currentIdx : Int
                currentIdx =
                    SelectList.index ls
            in
            SelectList.fromLists (List.indexedMap (\idx -> Tuple.pair idx >> fn) before)
                (fn ( currentIdx, current ))
                (List.indexedMap (\idx -> Tuple.pair (idx + (currentIdx + 1)) >> fn) after)
        )


{-| This accessor lets you traverse a list including the index of each element

    import Accessors exposing (..)
    import SelectList.Accessors as SL
    import Lens as L
    import SelectList exposing (SelectList)

    listRecord : { foo : SelectList { bar : Int } }
    listRecord =
        { foo = SelectList.fromLists [{ bar = 1 }] { bar = 2 } [{ bar = 3 }, { bar = 4 }]
        }

    multiplyIfGTOne : (Int, { bar : Int }) -> (Int, { bar : Int })
    multiplyIfGTOne ( idx, ({ bar } as rec) ) =
        if idx > 0 then
            ( idx, { bar = bar * 10 } )
        else
            (idx, rec)

    get (L.foo << SL.selected << L.bar) listRecord
    --> 2

    set (L.foo << SL.selected << L.bar) 37 listRecord
    --> { foo = SelectList.fromLists [{ bar = 1 }] { bar = 37 } [{ bar = 3 }, { bar = 4 }] }

    map (L.foo << SL.selected << L.bar) ((*) 10) listRecord
    --> { foo = SelectList.fromLists [{ bar = 1 }] { bar = 20 } [{ bar = 3 }, { bar = 4 }] }

-}
selected : Lens ls b b x y -> Lens ls (SelectList b) (SelectList b) x y
selected =
    Base.lens "[^]" SelectList.selected (\rec new -> SelectList.updateSelected (\_ -> new) rec)
