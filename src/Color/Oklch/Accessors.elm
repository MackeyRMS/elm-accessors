module Color.Oklch.Accessors exposing (color, oklab)

{-| Color.Oklch.Accessors

@docs color, oklab

-}

import Base exposing (Iso)
import Color exposing (Color)
import Color.Oklab exposing (Oklab)
import Color.Oklch
    exposing
        ( Oklch
        , fromColor
        , fromOklab
        , toColor
        , toOklab
        )


{-| color: This accessor lets you convert between oklch & avh4/elm-color

    import Accessors exposing (..)
    import Color.Oklch.Accessors exposing (..)
    import Color
    import Color.Round as Round


    Color.red |> to color |> from color |> Round.rgb
    --> Round.rgb Color.red

-}
color : Iso pr ls Color Oklch x y
color =
    Base.iso "color_oklch" fromColor toColor


{-| oklab: This accessor lets you convert between oklch & avh4/elm-color

    import Accessors exposing (to, from)
    import Color
    import Color.Round as Round


    Color.red |> to (color << oklab) |> from (color << oklab) |> Round.rgb
    --> Round.rgb Color.red

-}
oklab : Iso pr ls Oklch Oklab x y
oklab =
    Base.iso "oklch_oklab" toOklab fromOklab
