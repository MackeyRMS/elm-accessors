module Color.Palette.Accessors exposing (solid, transparent)

{-| Color.Palette.Accessors

@docs solid, transparent

-}

import Base exposing (Iso)
import Color exposing (Color)
import Color.Round as Round
import SolidColor exposing (SolidColor)
import TransparentColor exposing (TransparentColor)


{-| transparent: This accessor lets you convert between avh4/elm-color & tesk9/palette TransparentColor

    import Accessors exposing (to, from)
    import Color


    from transparent <| to transparent Color.red
    --> Color.red

-}
transparent : Iso pr ls Color TransparentColor x y
transparent =
    Base.iso "color_transparent"
        paletteFromColor
        (TransparentColor.toRGBA
            >> Round.mapAlpha TransparentColor.opacityToFloat
            >> Round.rgba255ToDecimal
            >> Color.fromRgba
        )


{-| solid: This accessor lets you convert between tesk9/palette TransparentColor && SolidColor

    import Accessors exposing (to, from)
    import Color


    from (transparent << solid) <| to (transparent << solid) Color.red
    --> Color.red

-}
solid : Iso pr ls TransparentColor SolidColor x y
solid =
    Base.iso "color_solid" TransparentColor.toColor (TransparentColor.fromColor TransparentColor.opaque)


paletteFromColor : Color -> TransparentColor
paletteFromColor =
    Color.toRgba
        >> Round.decimalTo255
        >> Round.mapAlpha TransparentColor.customOpacity
        >> TransparentColor.fromRGBA
