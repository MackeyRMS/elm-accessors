module Laws exposing (..)

import Accessors as A
    exposing
        ( A_Lens
        , A_Prism
        , An_Iso
        , An_Optic
        , Iso
        , elmui
        , from
        , hsluv
        , iso
        , new
        , oklch
        , swap
        , to
        , try
        )
import Array exposing (Array)
import Base exposing (A_Lens, Optic)
import Color exposing (Color)
import Color.Blending as ColorX
import Color.Oklch as Oklch exposing (Oklch)
import Colors exposing (..)
import Dict exposing (Dict)
import Element
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, int, string)
import HSLuv exposing (HSLuv)
import Lens as L
import Maybe exposing (Maybe)
import String
import Test exposing (..)
import Tuple.Extra as Tpl


eq : a -> a -> Expectation
eq =
    Expect.equal


type alias Person =
    { name : String
    , age : Int
    , email : Maybe String
    , stuff : List String
    , info : Dict String String
    , things : Array String
    }


suite : Test
suite =
    describe "Suite!"
        [ test "Name compositions output `jq` style String's" <|
            \() ->
                A.name (L.info << L.stuff << A.at 7 << L.name)
                    |> eq ".info.stuff[7]?.name"
        , describe "Laws Specs"
            [ isSetter (L.email << A.just) personFuzzer strFun string
            , isSetter (L.stuff << A.at 0) personFuzzer strFun string
            , isSetter (L.stuff << A.each) personFuzzer strFun string
            , isSetter (L.things << A.ix 0) personFuzzer strFun string
            , isSetter (L.things << A.every) personFuzzer strFun string
            , isLens L.name personFuzzer strFun string
            , isLens L.age personFuzzer intFun int
            , isLens (L.info << A.key "stuff") personFuzzer maybeStrFun (Fuzz.maybe string)
            , isPrism A.just (Fuzz.maybe string) string
            , isPrism A.ok (Fuzz.result int string) string
            , isPrism A.err (Fuzz.result int string) int
            , isIso intMaybe { s = Fuzz.maybe Fuzz.unit, a = Fuzz.bool, endo = boolFun }
            , isIso (swap elmui)
                { s = oneOfValues elmUIColors
                , a = oneOfValues colorColors
                , endo = colorFun
                }
            , isIso elmui
                { s = oneOfValues colorColors
                , a = oneOfValues elmUIColors
                , endo = elmUIFun
                }
            , isColorIso (swap hsluv << elmui)
                { s = oneOfValues hsluvColors
                , a = oneOfValues elmUIColors
                , endo = elmUIFun
                , aToColor = Element.toRgb >> Color.fromRgba
                , sToColor = HSLuv.toColor
                }
            , isColorIso (swap oklch << elmui)
                { s = oneOfValues oklchColors
                , a = oneOfValues elmUIColors
                , endo = elmUIFun
                , aToColor = Element.toRgb >> Color.fromRgba
                , sToColor = Oklch.toColor
                }
            , isColorIso (swap oklch)
                { s = oneOfValues oklchColors
                , a = oneOfValues colorColors
                , endo = colorFun
                , aToColor = identity
                , sToColor = Oklch.toColor
                }
            ]
        ]


oneOfValues : List a -> Fuzzer a
oneOfValues =
    Fuzz.oneOf << List.map Fuzz.constant


intMaybe : Iso pr ls (Maybe ()) Bool x y
intMaybe =
    iso "nulllyInt"
        (\maybeh ->
            case maybeh of
                Just () ->
                    True

                Nothing ->
                    False
        )
        (\true ->
            if true then
                Just ()

            else
                Nothing
        )


type alias Function a =
    a -> a


strFun : Fuzzer (Function String)
strFun =
    Fuzz.oneOf
        [ Fuzz.map String.append string
        , Fuzz.map (\s -> String.append s >> String.reverse) string
        , Fuzz.map (\s -> String.append s >> String.toUpper) string
        , Fuzz.map (\s -> String.append s >> String.toLower) string
        ]


intFun : Fuzzer (Function Int)
intFun =
    Fuzz.oneOf
        [ Fuzz.map (+) int
        , Fuzz.map (-) int
        , Fuzz.map (*) int
        , Fuzz.map (//) int
        ]


boolFun : Fuzzer (Function Bool)
boolFun =
    Fuzz.oneOf
        [ Fuzz.map (&&) Fuzz.bool
        , Fuzz.map (||) Fuzz.bool
        ]


elmUIFun : Fuzzer (Function Element.Color)
elmUIFun =
    Fuzz.oneOf
        [ Fuzz.map (blend 0.1) (oneOfValues elmUIColors)
        , Fuzz.map (blend 0.2) (oneOfValues elmUIColors)
        ]


colorFun : Fuzzer (Function Color)
colorFun =
    Fuzz.oneOf
        [ Fuzz.map ColorX.lighten (oneOfValues colorColors)
        , Fuzz.map ColorX.darken (oneOfValues colorColors)
        ]


maybeStrFun : Fuzzer (Function (Maybe String))
maybeStrFun =
    Fuzz.oneOf
        [ Fuzz.map
            (\_ ->
                Maybe.andThen String.toInt
                    >> Maybe.map String.fromInt
            )
            (Fuzz.maybe string)
        ]


personFuzzer : Fuzzer Person
personFuzzer =
    Fuzz.constant Person
        |> Fuzz.andMap string
        |> Fuzz.andMap int
        |> Fuzz.andMap (Fuzz.maybe string)
        |> Fuzz.andMap (Fuzz.list string)
        |> Fuzz.andMap (Fuzz.list (Fuzz.tuple ( string, string )) |> Fuzz.map Dict.fromList)
        |> Fuzz.andMap (Fuzz.list string |> Fuzz.map Array.fromList)


isSetter : Optic pr ls a a c c c c -> Fuzzer a -> Fuzzer (c -> c) -> Fuzzer c -> Test
isSetter l fzr fnFzr val =
    describe ("isSetable: " ++ A.name l)
        [ fuzz fzr
            "identity"
            (Expect.true "setter"
                << setter_id l
            )
        , fuzz (Fuzz.tuple3 ( fzr, fnFzr, fnFzr ))
            "composition"
            (\( s, f, g ) ->
                Expect.true "setter" <|
                    setter_composition l s f g
            )
        , fuzz (Fuzz.tuple3 ( fzr, val, val ))
            "set_set"
            (\( s, a, b ) ->
                Expect.true "setter" <|
                    setter_set_set l s a b
            )
        ]


isLens : A_Lens pr a b -> Fuzzer a -> Fuzzer (b -> b) -> Fuzzer b -> Test
isLens l fzr valFn val =
    describe ("isLens: " ++ A.name l)
        [ isSetter l fzr valFn val

        -- There's Traversal laws in here somewhere but not sure they're expressible in Elm.
        , fuzz fzr "lens_set_get" (lens_set_get l >> Expect.true "lens_set_get")
        , fuzz (Fuzz.tuple ( fzr, val ))
            "lens_get_set"
            (\( b, s ) ->
                lens_get_set l b s
                    |> Expect.true "lens_get_set"
            )
        ]


isPrism : A_Prism ls s a -> Fuzzer s -> Fuzzer a -> Test
isPrism pr fzrS fzrA =
    describe ("isPrism: " ++ A.name pr)
        [ fuzz (Fuzz.tuple ( fzrS, fzrA ))
            "yin"
            (\( s, a ) ->
                Expect.true "yin & yang"
                    (prism_yin pr a && prism_yang pr s)
            )

        -- , isTraversal
        ]


isIso :
    An_Iso s a
    ->
        { a : Fuzzer a
        , endo : Fuzzer (a -> a)
        , s : Fuzzer s
        }
    -> Test
isIso i fzrs =
    describe ("isIso: " ++ A.name i)
        [ isPrism i fzrs.s fzrs.a
        , isLens i fzrs.s fzrs.endo fzrs.a
        , fuzz fzrs.s "iso_hither" (Expect.true "hither" << iso_hither i)
        , fuzz fzrs.a "iso_yon" (Expect.true "yon" << iso_yon i)
        ]


isColorIso :
    An_Iso s a
    ->
        { a : Fuzzer a
        , endo : Fuzzer (a -> a)
        , s : Fuzzer s
        , aToColor : a -> Color
        , sToColor : s -> Color
        }
    -> Test
isColorIso i fzrs =
    describe ("isIso: " ++ A.name i)
        [ fuzz fzrs.s
            "iso_hither"
            (\fuzzed ->
                rounded fzrs.sToColor
                    fuzzed
                    (from i (to i fuzzed))
            )
        , fuzz fzrs.a
            "iso_yon"
            (\fuzzed ->
                rounded fzrs.aToColor
                    fuzzed
                    (to i (from i fuzzed))
            )
        , describe ("isLens: " ++ A.name i)
            [ describe ("isSetable: " ++ A.name i)
                [ fuzz fzrs.s
                    "identity"
                    (\fuzzed ->
                        rounded fzrs.sToColor fuzzed (A.map i identity fuzzed)
                    )
                , fuzz (Fuzz.tuple3 ( fzrs.s, fzrs.endo, fzrs.endo ))
                    "composition"
                    (\( s, f, g ) ->
                        rounded fzrs.sToColor
                            (A.map i f (A.map i g s))
                            (A.map i (f << g) s)
                    )
                , fuzz (Fuzz.tuple3 ( fzrs.s, fzrs.a, fzrs.a ))
                    "set_set"
                    (\( s, a, b ) ->
                        rounded fzrs.sToColor
                            (A.set i b (A.set i a s))
                            (A.set i b s)
                    )
                ]
            , fuzz fzrs.s
                "lens_set_get"
                (\fuzzed ->
                    rounded fzrs.sToColor
                        (A.set i (A.get i fuzzed) fuzzed)
                        fuzzed
                )
            , fuzz (Fuzz.tuple ( fzrs.s, fzrs.a ))
                "lens_get_set"
                (\( s, a ) ->
                    rounded fzrs.aToColor
                        (A.get i (A.set i a s))
                        a
                )
            ]
        , describe
            ("isPrism: " ++ A.name i)
            [ fuzz (Fuzz.tuple ( fzrs.s, fzrs.a ))
                "yin"
                (\( s, a ) ->
                    Expect.all
                        [ \() ->
                            rounded fzrs.sToColor
                                (Maybe.withDefault s <| Maybe.map (new i) (try i s))
                                s
                        , \() ->
                            rounded (Maybe.map fzrs.aToColor >> Maybe.withDefault Color.black)
                                (try i (new i a))
                                (Just a)
                        ]
                        ()
                )
            ]
        ]


rounded : (a -> Color) -> a -> a -> Expectation
rounded toClr a b =
    Expect.all
        [ within01 << Tpl.map .red
        , within01 << Tpl.map .green
        , within01 << Tpl.map .blue
        , within01 << Tpl.map .alpha
        ]
        (Tpl.map (Color.toRgba << toClr) ( a, b ))


within01 : ( Float, Float ) -> Expectation
within01 ( a, b ) =
    Expect.within (Absolute 0.01) a b


setter_id : An_Optic pr ls s a -> s -> Bool
setter_id l s =
    A.map l identity s == s


setter_composition : An_Optic pr ls s a -> s -> (a -> a) -> (a -> a) -> Bool
setter_composition l s f g =
    A.map l f (A.map l g s) == A.map l (f << g) s


setter_set_set : An_Optic pr ls s a -> s -> a -> a -> Bool
setter_set_set l s a b =
    A.set l b (A.set l a s) == A.set l b s


lens_set_get : A_Lens pr b a -> b -> Bool
lens_set_get l s =
    A.set l (A.get l s) s == s


lens_get_set : A_Lens pr s a -> s -> a -> Bool
lens_get_set l s a =
    A.get l (A.set l a s) == a


prism_yin : A_Prism ls s a -> a -> Bool
prism_yin l a =
    try l (new l a) == Just a


prism_yang : A_Prism ls s a -> s -> Bool
prism_yang l s =
    (Maybe.withDefault s <| Maybe.map (new l) (try l s)) == s


iso_hither : An_Iso s a -> s -> Bool
iso_hither l s =
    (from l <| to l s) == s


iso_yon : An_Iso s a -> a -> Bool
iso_yon l a =
    (to l <| from l a) == a



-- traverse_pure : LensLike' f s a -> s -> Bool
-- traverse_pure l s = l pure s == (pure s : f s)
-- traverse_pureMaybe : Eq s => LensLike' Maybe s a -> s -> Bool
-- traverse_pureMaybe = traverse_pure
-- traverse_pureList : Eq s => LensLike' [] s a -> s -> Bool
-- traverse_pureList = traverse_pure
-- traverse_compose : (Applicative f, Applicative g, Eq (f (g s)))
--                     => Traversal' s a -> (a -> g a) -> (a -> f a) -> s -> Bool
-- traverse_compose t f g s = (fmap (t f) . t g) s == (getCompose . t (Compose . fmap f . g)) s


elmUIColors : List Element.Color
elmUIColors =
    [ systemBlack
    , black
    , verityBlack
    , offBlackForTab
    , bodyFontColor
    , white
    , dullWhite
    , red
    , lightCinder
    , yellow
    , green
    , brightGreen
    , blue
    , lightBlue
    , gray0
    , gray1
    , gray2
    , gray3
    , gray4
    ]


colorColors : List Color
colorColors =
    List.map (Element.toRgb >> Color.fromRgba) elmUIColors


hsluvColors : List HSLuv
hsluvColors =
    List.map HSLuv.color colorColors


oklchColors : List Oklch
oklchColors =
    List.map Oklch.fromColor colorColors
