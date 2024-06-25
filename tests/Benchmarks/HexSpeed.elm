module Benchmarks.HexSpeed exposing (main)

import Accessors exposing (new, to, try)
import Benchmark as B exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Color
import Color.ElmUI.Accessors as UI
import Color.Hex.Accessors as Hex
import Color.Palette.Accessors as Palette
import Element as E
import TransparentColor exposing (TransparentColor)


main : BenchmarkProgram
main =
    program suite


paletteRed : TransparentColor
paletteRed =
    to Palette.transparent Color.red


uiRed : E.Color
uiRed =
    to UI.color Color.red


hexRed : String
hexRed =
    "#FF0000"


suite : Benchmark
suite =
    describe "Hex"
        [ describe "palette vs ui"
            [ B.compare "to"
                "palette"
                (\_ -> new Hex.palette paletteRed)
                "elm-ui"
                (\_ -> new Hex.ui uiRed)
            , B.compare "from"
                "palette"
                (\_ -> try Hex.palette hexRed)
                "elm-ui"
                (\_ -> try Hex.ui hexRed)
            ]
        , describe "color vs ui"
            [ B.compare "to"
                "elm-color"
                (\_ -> new Hex.color Color.red)
                "elm-ui"
                (\_ -> new Hex.ui uiRed)
            , B.compare "from"
                "elm-color"
                (\_ -> try Hex.color hexRed)
                "elm-ui"
                (\_ -> try Hex.ui hexRed)
            ]
        ]
