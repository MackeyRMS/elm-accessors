module Color.Hex.Accessors exposing (color, palette, ui)

{-| Color.Hex.Accessors

@docs color, palette, ui

-}

import Base exposing (Prism)
import Color exposing (Color)
import Color.Convert as Convert
import Element as E
import Hex
import Maybe.Extra exposing (andMap, orElse)
import TransparentColor exposing (TransparentColor)


{-| palette: This accessor lets you convert between tesk9/palette TransparentColor && SolidColor

    import Accessors exposing (swap, new, try)
    import Color
    import Color.Hex.Accessors as Hex
    import Color.Palette.Accessors as Palette


    new (Hex.palette << swap Palette.transparent) Color.red
    -->  "#CC0000"
    try (Hex.palette << swap Palette.transparent) "#C00F" -- with alpha channel
    --> Just Color.red

-}
palette : Prism pr String TransparentColor x y
palette =
    Base.prism "palette_hexa"
        TransparentColor.toHexA
        TransparentColor.fromHexA


{-| color: This accessor lets you convert between tesk9/palette TransparentColor && SolidColor

    import Accessors exposing (swap, new, try)
    import Color
    import Color.Hex.Accessors as Hex


    new (Hex.color) Color.red
    -->  "#cc0000"
    try (Hex.color) "#C00F" -- with alpha channel
    --> Just Color.red

-}
color : Prism pr String Color x y
color =
    Base.prism "color_hexa"
        Convert.colorToHex
        Convert.hexToColor


{-| ui: This accessor lets you convert between tesk9/palette TransparentColor && SolidColor

    import Accessors exposing (swap, new, try)
    import Color
    import Color.Hex.Accessors as Hex
    import Color.ElmUI.Accessors as UI


    new (Hex.ui << swap UI.color) Color.red
    -->  "#cc0000"
    try (Hex.ui << swap UI.color) "#C00F" -- with alpha channel
    --> Just Color.red

-}
ui : Prism pr String E.Color x y
ui =
    Base.prism "ui_hexa" uiToHex hexToUI


uiToHex : E.Color -> String
uiToHex c =
    E.toRgb c |> floatToInt |> toHex


hexToUI : String -> Result String E.Color
hexToUI s =
    fromHex s
        |> Result.fromMaybe s
        |> Result.map (intToFloat >> E.fromRgb255)


type alias RGBA =
    RGB Int


type alias MkRGBA =
    { red : Int, green : Int, blue : Int, alpha : Int }


type alias RGB a =
    { red : Int, green : Int, blue : Int, alpha : a }


intToFloat : RGB Int -> RGB Float
intToFloat { red, green, blue, alpha } =
    { red = red, green = green, blue = blue, alpha = toFloat alpha / 255 }


floatToInt : { red : Float, green : Float, blue : Float, alpha : Float } -> RGBA
floatToInt { red, green, blue, alpha } =
    { red = round (red * 255)
    , green = round (green * 255)
    , blue = round (blue * 255)
    , alpha = round (alpha * 255)
    }


toHex : RGBA -> String
toHex { red, green, blue, alpha } =
    "#"
        ++ toStr red
        ++ toStr green
        ++ toStr blue
        ++ (if alpha < 1 then
                toStr alpha

            else
                ""
           )


toStr : Int -> String
toStr c =
    String.padLeft 2 '0' (Hex.toString c)


fromHex : String -> Maybe RGBA
fromHex str =
    let
        maybeRGBA : Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe RGBA
        maybeRGBA r g b a =
            Just MkRGBA
                |> andMap r
                |> andMap g
                |> andMap b
                |> andMap (a |> orElse (Just 255))

        rgb : ( Char, Char ) -> ( Char, Char ) -> ( Char, Char ) -> Maybe ( Char, Char ) -> Maybe RGBA
        rgb r g b a =
            maybeRGBA (wrapToHex r) (wrapToHex g) (wrapToHex b) (Maybe.andThen wrapToHex a)
    in
    case String.toList str of
        '#' :: h1 :: h2 :: h3 :: h4 :: h5 :: h6 :: h7 :: h8 :: _ ->
            rgb ( h1, h2 ) ( h3, h4 ) ( h5, h6 ) (Just ( h7, h8 ))

        '#' :: h1 :: h2 :: h3 :: h4 :: h5 :: h6 :: _ ->
            rgb ( h1, h2 ) ( h3, h4 ) ( h5, h6 ) Nothing

        '#' :: h1 :: h2 :: h3 :: h4 :: _ ->
            rgb ( h1, h1 ) ( h2, h2 ) ( h3, h3 ) (Just ( h4, h4 ))

        '#' :: h1 :: h2 :: h3 :: _ ->
            rgb ( h1, h1 ) ( h2, h2 ) ( h3, h3 ) Nothing

        _ ->
            Nothing


wrapToHex : ( Char, Char ) -> Maybe number
wrapToHex ( tens, ones ) =
    Just (\ts os -> (ts * 16) + os)
        |> andMap (fromChar tens)
        |> andMap (fromChar ones)


fromChar : Char -> Maybe number
fromChar char =
    case Char.toUpper char of
        '0' ->
            Just 0x00

        '1' ->
            Just 0x01

        '2' ->
            Just 0x02

        '3' ->
            Just 0x03

        '4' ->
            Just 0x04

        '5' ->
            Just 0x05

        '6' ->
            Just 0x06

        '7' ->
            Just 0x07

        '8' ->
            Just 0x08

        '9' ->
            Just 0x09

        'A' ->
            Just 0x0A

        'B' ->
            Just 0x0B

        'C' ->
            Just 0x0C

        'D' ->
            Just 0x0D

        'E' ->
            Just 0x0E

        'F' ->
            Just 0x0F

        _ ->
            Nothing
