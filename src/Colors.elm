module Colors exposing
    ( systemBlack, black, verityBlack, offBlackForTab, bodyFontColor
    , dullWhite, white
    , red, lightCinder, yellow, green, brightGreen, blue, lightBlue
    , gray0, gray1, gray2, gray3, gray4
    , blend
    )

{-|


# Color most of the original module has been translated into elm-ui Element.Color's.

@docs systemBlack, black, verityBlack, offBlackForTab, bodyFontColor
@docs dullWhite, white
@docs red, lightCinder, yellow, green, brightGreen, blue, lightBlue
@docs gray0, gray1, gray2, gray3, gray4

-}

import Element



---------------------------------------------------------------
-- API --
---------------------------------------------------------------


verityBlack : Element.Color
verityBlack =
    Element.rgb255 16 24 32


systemBlack : Element.Color
systemBlack =
    noHue_ 0


black : Element.Color
black =
    noHue_ 17


offBlackForTab : Element.Color
offBlackForTab =
    Element.rgba255 18 18 18 0.8


green : Element.Color
green =
    Element.rgb255 85 155 28


brightGreen : Element.Color
brightGreen =
    Element.rgb255 112 213 156


bodyFontColor : Element.Color
bodyFontColor =
    Element.rgb255 78 68 74


blue : Element.Color
blue =
    Element.rgb255 70 153 229


lightBlue : Element.Color
lightBlue =
    Element.rgb255 35 105 210


white : Element.Color
white =
    noHue_ 255


dullWhite : Element.Color
dullWhite =
    blend 0.04 black white


red : Element.Color
red =
    Element.rgb255 244 15 126


lightCinder : Element.Color
lightCinder =
    Element.rgb255 244 235 235


yellow : Element.Color
yellow =
    Element.rgb255 255 255 136


gray0 : Element.Color
gray0 =
    noHue_ 103


gray1 : Element.Color
gray1 =
    noHue_ 144


gray2 : Element.Color
gray2 =
    noHue_ 196


gray3 : Element.Color
gray3 =
    noHue_ 217


gray4 : Element.Color
gray4 =
    noHue_ 238



---------------------------------------------------------------
-- Internal Helpers --
---------------------------------------------------------------


noHue_ : Int -> Element.Color
noHue_ i =
    Element.rgb255 i i i


blend : Float -> Element.Color -> Element.Color -> Element.Color
blend tintAlpha srcColor dstColor =
    let
        dst : { red : Float, green : Float, blue : Float, alpha : Float }
        dst =
            let
                d : { red : Float, green : Float, blue : Float, alpha : Float }
                d =
                    Element.toRgb dstColor
            in
            { red = d.red * 255, green = d.green * 255, blue = d.blue * 255, alpha = d.alpha }

        src : { red : Float, green : Float, blue : Float, alpha : Float }
        src =
            let
                s : { red : Float, green : Float, blue : Float, alpha : Float }
                s =
                    Element.toRgb srcColor
            in
            { red = s.red * 255, green = s.green * 255, blue = s.blue * 255, alpha = s.alpha }

        blendChannel : Float -> Float -> Float
        blendChannel channelS channelD =
            ((channelS * tintAlpha) + (channelD * dst.alpha * (1 - tintAlpha)))
                / aOut

        aOut : Float
        aOut =
            tintAlpha + dst.alpha * (1 - tintAlpha)
    in
    { red =
        blendChannel src.red dst.red
            |> round
            |> clamp 0 255
    , green =
        blendChannel src.green dst.green
            |> round
            |> clamp 0 255
    , blue =
        blendChannel src.blue dst.blue
            |> round
            |> clamp 0 255
    , alpha = aOut
    }
        |> Element.fromRgb255
