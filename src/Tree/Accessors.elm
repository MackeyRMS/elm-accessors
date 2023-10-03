module Tree.Accessors exposing
    ( at, label_, path, at_, id
    , each
    )

{-| List.Accessors

@docs at, label_, path, at_, id
@docs each

-}

import Base exposing (Lens, Traversal, Traversal_)
import Tree exposing (Tree)
import Tree.Navigate
import Tree.Path exposing (TreePath)
import Tree.Zipper as Zipper


{-| This accessor combinator lets you access the label at a particular TreePath.

    import Accessors exposing (..)
    import Tree exposing (Tree, tree)
    import Tree.Accessors as Tree
    import Tree.Extra.Lue exposing (leaf)
    import Lens as L

    simpleTree : Tree String
    simpleTree =
        tree "root"
            [ tree "a" [ leaf "b"]
            , tree "x" [ leaf "y"]
            ]

    try (Tree.at [0]) simpleTree
    --> Just "a"

    map (Tree.at [0]) String.toUpper simpleTree
    --> tree "root" [ tree "A" [ leaf "b" ] , tree "x" [ leaf "y" ] ]

-}
at : TreePath -> Traversal (Tree a) a x y
at p =
    path p << label_


{-| This accessor combinator lets you access the label of a given Tree.

    import Accessors exposing (..)
    import Tree exposing (Tree, tree)
    import Tree.Accessors as Tree
    import Tree.Extra.Lue exposing (leaf)
    import Lens as L

    simpleTree : Tree String
    simpleTree =
        tree "root"
            [ tree "a" [ leaf "b"]
            , tree "x" [ leaf "y"]
            ]

    get (Tree.label_) simpleTree
    --> "root"

    set (Tree.label_) "top 'o the mornin' to yah!" simpleTree
    --> tree "top 'o the mornin' to yah!" [ tree "a" [ leaf "b" ] , tree "x" [ leaf "y" ] ]

    map (Tree.label_) String.toUpper simpleTree
    --> tree "ROOT" [ tree "a" [ leaf "b" ] , tree "x" [ leaf "y" ] ]

-}
label_ : Lens ls (Tree a) a x y
label_ =
    Base.lens "-label"
        Tree.label
        (\s b -> Tree.mapLabel (always b) s)


{-| This accessor combinator lets you access a sub-tree at a given TreePath.

    import Accessors exposing (..)
    import Tree exposing (Tree, tree)
    import Tree.Accessors as Tree
    import Tree.Extra.Lue exposing (leaf)
    import Lens as L

    simpleTree : Tree String
    simpleTree =
        tree "root"
            [ tree "a" [ leaf "b"]
            , tree "x" [ leaf "y"]
            ]

    try (Tree.path [0,0]) simpleTree
    --> Just (leaf "b")

    set (Tree.path [1,0]) (tree "hey!" [leaf "deeper"]) simpleTree
    --> tree "root" [ tree "a" [ leaf "b" ] , tree "x" [ tree "hey!" [leaf "deeper"] ] ]

    map (Tree.path [0]) (Tree.mapLabel (\s -> "gimme an " ++ String.toUpper s ++ "!")) simpleTree
    --> tree "root" [ tree "gimme an A!" [ leaf "b" ] , tree "x" [ leaf "y" ] ]

-}
path : TreePath -> Traversal (Tree a) (Tree a) x y
path p =
    Base.traversal
        ("<" ++ String.join ", " (List.map String.fromInt p) ++ ">")
        (Tree.Navigate.to p >> Maybe.map List.singleton >> Maybe.withDefault [])
        (Tree.Navigate.alter p)


{-| This accessor combinator lets you modify the type of each label of a Tree.

    import Accessors exposing (..)
    import Tree exposing (Tree, tree)
    import Tree.Accessors as Tree
    import Tree.Extra.Lue exposing (leaf)
    import Lens as L

    simpleTree : Tree String
    simpleTree =
        tree "r"
            [ tree "a" [ leaf "b"]
            , tree "x" [ leaf "y"]
            ]

    all Tree.each simpleTree
    --> ["r", "a", "b", "x", "y"]

    set Tree.each "1" simpleTree
    --> tree "1" [ tree "1" [ leaf "1" ] , tree "1" [ leaf "1" ] ]

    map Tree.each (\s -> "gimme an " ++ String.toUpper s ++ "!") simpleTree
    --> tree "gimme an R!" [ tree "gimme an A!" [ leaf "gimme an B!" ] , tree "gimme an X!" [ leaf "gimme an Y!" ] ]

-}
each : Traversal_ (Tree a) (Tree b) a b x y
each =
    Base.traversal "<>" Tree.flatten Tree.map


{-| -}
at_ : (a -> String) -> a -> Traversal (Tree { z | id : a }) (Tree { z | id : a }) x y
at_ toString id_ =
    Base.traversal
        ("<" ++ toString id_ ++ ">")
        (Zipper.fromTree
            >> Zipper.findFromRoot (\i -> i.id == id_)
            >> Maybe.map (Zipper.toTree >> List.singleton)
            >> Maybe.withDefault []
        )
        (\fn t ->
            t
                |> Zipper.fromTree
                |> Zipper.findFromRoot (\i -> i.id == id_)
                |> Maybe.map (Zipper.mapTree fn >> Zipper.toTree)
                |> Maybe.withDefault t
        )


{-| -}
id : String -> Traversal (Tree { z | id : String }) (Tree { z | id : String }) x y
id =
    at_ identity
