module Subintroduction exposing (..)

import Debug exposing (..)
import Html exposing (..)
import Html.Attributes as HtmlAttr exposing (..)
import Html.Events exposing (keyCode, onClick, onInput)
import Model exposing (CreativeZone, Model, alllevel, initModel)
import Msg exposing (Msg(..))
import Objects exposing (Model_State(..))
import String exposing (fromFloat, fromInt)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr


pixelHeight : Float
pixelHeight =
    400


pixelWidth_background : Float
pixelWidth_background =
    600


pixelHeight_background : Float
pixelHeight_background =
    900


view_shadow4 : Float -> Float -> Float -> Html Msg
view_shadow4 w h r =
    div
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "fixed"
        ]
        [ div
            [ HtmlAttr.style "width" "600px"
            , HtmlAttr.style "height" "873px"
            , HtmlAttr.style "position" "fixed"
            , HtmlAttr.style "left" (fromFloat ((w - pixelWidth_background * r) / 2) ++ "px")
            , HtmlAttr.style "top" (fromFloat ((h - pixelHeight_background * r) / 2 - 50) ++ "px")
            , HtmlAttr.style "transform-origin" "0 0"
            , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat r ++ ")")
            , HtmlAttr.style "background" "rgba(236, 240, 241, 0.85)"
            , HtmlAttr.style "opacity" "0.8"
            ]
            []
        , div
            [ HtmlAttr.style "width" "190px"
            , HtmlAttr.style "height" "820px"
            , HtmlAttr.style "position" "fixed"
            , HtmlAttr.style "left" (fromFloat ((w - pixelWidth_background * r) / 2) ++ "px")
            , HtmlAttr.style "top" (fromFloat ((h - pixelHeight_background * r) / 2 + 823) ++ "px")
            , HtmlAttr.style "transform-origin" "0 0"
            , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat r ++ ")")
            , HtmlAttr.style "background" "rgba(236, 240, 241, 0.85)"
            , HtmlAttr.style "opacity" "0.8"
            ]
            []
        , div
            [ HtmlAttr.style "width" "345px"
            , HtmlAttr.style "height" "820px"
            , HtmlAttr.style "position" "fixed"
            , HtmlAttr.style "left" (fromFloat ((w - pixelWidth_background * r) / 2 + 263) ++ "px")
            , HtmlAttr.style "top" (fromFloat ((h - pixelHeight_background * r) / 2 + 823) ++ "px")
            , HtmlAttr.style "transform-origin" "0 0"
            , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat r ++ ")")
            , HtmlAttr.style "background" "rgba(236, 240, 241, 0.85)"
            , HtmlAttr.style "opacity" "0.8"
            ]
            []
        ]


view_shadow3 : Float -> Float -> Float -> Html Msg
view_shadow3 w h r =
    div
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "fixed"
        ]
        [ div
            [ HtmlAttr.style "width" "600px"
            , HtmlAttr.style "height" "870px"
            , HtmlAttr.style "position" "fixed"
            , HtmlAttr.style "left" (fromFloat ((w - pixelWidth_background * r) / 2) ++ "px")
            , HtmlAttr.style "top" (fromFloat ((h - pixelHeight_background * r) / 2 - 50) ++ "px")
            , HtmlAttr.style "transform-origin" "0 0"
            , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat r ++ ")")
            , HtmlAttr.style "background" "rgba(236, 240, 241, 0.85)"
            , HtmlAttr.style "opacity" "0.8"
            ]
            []
        , div
            [ HtmlAttr.style "width" "190px"
            , HtmlAttr.style "height" "820px"
            , HtmlAttr.style "position" "fixed"
            , HtmlAttr.style "left" (fromFloat ((w - pixelWidth_background * r) / 2) ++ "px")
            , HtmlAttr.style "top" (fromFloat ((h - pixelHeight_background * r) / 2 + 820) ++ "px")
            , HtmlAttr.style "transform-origin" "0 0"
            , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat r ++ ")")
            , HtmlAttr.style "background" "rgba(236, 240, 241, 0.85)"
            , HtmlAttr.style "opacity" "0.8"
            ]
            []
        , div
            [ HtmlAttr.style "width" "375px"
            , HtmlAttr.style "height" "820px"
            , HtmlAttr.style "position" "fixed"
            , HtmlAttr.style "left" (fromFloat ((w - pixelWidth_background * r) / 2 + 225) ++ "px")
            , HtmlAttr.style "top" (fromFloat ((h - pixelHeight_background * r) / 2 + 820) ++ "px")
            , HtmlAttr.style "transform-origin" "0 0"
            , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat r ++ ")")
            , HtmlAttr.style "background" "rgba(236, 240, 241, 0.85)"
            , HtmlAttr.style "opacity" "0.8"
            ]
            []
        ]


view_shadow2 : Float -> Float -> Float -> Html Msg
view_shadow2 w h r =
    div
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "fixed"
        ]
        [ div
            [ HtmlAttr.style "width" "600px"
            , HtmlAttr.style "height" "870px"
            , HtmlAttr.style "position" "fixed"
            , HtmlAttr.style "left" (fromFloat ((w - pixelWidth_background * r) / 2) ++ "px")
            , HtmlAttr.style "top" (fromFloat ((h - pixelHeight_background * r) / 2 - 50) ++ "px")
            , HtmlAttr.style "transform-origin" "0 0"
            , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat r ++ ")")
            , HtmlAttr.style "background" "rgba(236, 240, 241, 0.85)"
            , HtmlAttr.style "opacity" "0.8"
            ]
            []
        , div
            [ HtmlAttr.style "width" "300px"
            , HtmlAttr.style "height" "820px"
            , HtmlAttr.style "position" "fixed"
            , HtmlAttr.style "left" (fromFloat ((w - pixelWidth_background * r) / 2) ++ "px")
            , HtmlAttr.style "top" (fromFloat ((h - pixelHeight_background * r) / 2 + 820) ++ "px")
            , HtmlAttr.style "transform-origin" "0 0"
            , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat r ++ ")")
            , HtmlAttr.style "background" "rgba(236, 240, 241, 0.85)"
            , HtmlAttr.style "opacity" "0.8"
            ]
            []
        , div
            [ HtmlAttr.style "width" "265px"
            , HtmlAttr.style "height" "820px"
            , HtmlAttr.style "position" "fixed"
            , HtmlAttr.style "left" (fromFloat ((w - pixelWidth_background * r) / 2 + 335) ++ "px")
            , HtmlAttr.style "top" (fromFloat ((h - pixelHeight_background * r) / 2 + 820) ++ "px")
            , HtmlAttr.style "transform-origin" "0 0"
            , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat r ++ ")")
            , HtmlAttr.style "background" "rgba(236, 240, 241, 0.85)"
            , HtmlAttr.style "opacity" "0.8"
            ]
            []
        ]


view_shadow1 : Float -> Float -> Float -> Html Msg
view_shadow1 w h r =
    div
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "fixed"
        ]
        [ div
            [ HtmlAttr.style "width" "600px"
            , HtmlAttr.style "height" "870px"
            , HtmlAttr.style "position" "fixed"
            , HtmlAttr.style "left" (fromFloat ((w - pixelWidth_background * r) / 2) ++ "px")
            , HtmlAttr.style "top" (fromFloat ((h - pixelHeight_background * r) / 2 - 50) ++ "px")
            , HtmlAttr.style "transform-origin" "0 0"
            , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat r ++ ")")
            , HtmlAttr.style "background" "rgba(236, 240, 241, 0.85)"
            , HtmlAttr.style "opacity" "0.8"
            ]
            []
        , div
            [ HtmlAttr.style "width" "415px"
            , HtmlAttr.style "height" "820px"
            , HtmlAttr.style "position" "fixed"
            , HtmlAttr.style "left" (fromFloat ((w - pixelWidth_background * r) / 2) ++ "px")
            , HtmlAttr.style "top" (fromFloat ((h - pixelHeight_background * r) / 2 + 820) ++ "px")
            , HtmlAttr.style "transform-origin" "0 0"
            , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat r ++ ")")
            , HtmlAttr.style "background" "rgba(236, 240, 241, 0.85)"
            , HtmlAttr.style "opacity" "0.8"
            ]
            []
        , div
            [ HtmlAttr.style "width" "150px"
            , HtmlAttr.style "height" "820px"
            , HtmlAttr.style "position" "fixed"
            , HtmlAttr.style "left" (fromFloat ((w - pixelWidth_background * r) / 2 + 450) ++ "px")
            , HtmlAttr.style "top" (fromFloat ((h - pixelHeight_background * r) / 2 + 820) ++ "px")
            , HtmlAttr.style "transform-origin" "0 0"
            , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat r ++ ")")
            , HtmlAttr.style "background" "rgba(236, 240, 241, 0.85)"
            , HtmlAttr.style "opacity" "0.8"
            ]
            []
        ]


view_scene2_5 : Model -> Float -> Float -> Float -> Float -> Html Msg
view_scene2_5 model w h r time =
    div
        [ HtmlAttr.style "width" "100%" --(fromFloat pixelWidth ++ "px")
        , HtmlAttr.style "height" "100%" --(fromFloat pixelHeight ++ "px")
        , HtmlAttr.style "position" "fixed"
        , HtmlAttr.style "background" "rgb(246,227,197)"
        ]
        [ Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "98.5%"
            ]
            [ Svg.image
                [ SvgAttr.xlinkHref "assets/test.png"
                , SvgAttr.x (fromFloat ((w - pixelWidth_background * r) / 2) ++ "px")
                , SvgAttr.y (fromFloat ((h - pixelHeight_background * r) / 2 - 3600 + 1620 + (5700 - 4900) * (4900 - 4000) * 2.5 / 500 + (4900 - 5700) * (5700 - 4900) / 550) ++ "px")
                , SvgAttr.width "600px"
                , SvgAttr.height "4500px"
                ]
                []
            , view_goal model w h r
            ]
        , view_scene2_corridor w h
        , div
            [ HtmlAttr.style "width" "100%"
            , HtmlAttr.style "width" "100%"
            , HtmlAttr.style "height" "100%"
            , HtmlAttr.style "position" "fixed"
            , HtmlAttr.style "left" (fromFloat (w / 2 - 80) ++ "px")
            , HtmlAttr.style "top" (fromFloat (h / 3) ++ "px")
            , HtmlAttr.style "font-size" "20px"
            , HtmlAttr.style "font-family" "Comic Sans MS"
            , HtmlAttr.style "color" "rgb(119,67,96)"
            ]
            [ text "HERE IS THE GOAL!" ]
        ]


view_scene2_4 : Model -> Float -> Float -> Float -> Float -> Html Msg
view_scene2_4 model w h r time =
    div
        [ HtmlAttr.style "width" "100%" --(fromFloat pixelWidth ++ "px")
        , HtmlAttr.style "height" "100%" --(fromFloat pixelHeight ++ "px")
        , HtmlAttr.style "position" "fixed"
        , HtmlAttr.style "background" "rgb(246,227,197)"
        ]
        [ Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "98.5%"
            ]
            [ Svg.image
                [ SvgAttr.xlinkHref "assets/test.png"
                , SvgAttr.x (fromFloat ((w - pixelWidth_background * r) / 2) ++ "px")
                , SvgAttr.y (fromFloat ((h - pixelHeight_background * r) / 2 - 3600 + 1620 + (5700 - 4900) * (4900 - 4000) * 2.5 / 500 + (4900 - 5700) * (5700 - 4900) / 550) ++ "px")
                , SvgAttr.width "600px"
                , SvgAttr.height "4500px"
                ]
                []
            , view_goal model w h r
            ]
        , view_scene2_corridor w h
        ]


view_scene2_3 : Model -> Float -> Float -> Float -> Float -> Html Msg
view_scene2_3 model w h r time =
    div
        [ HtmlAttr.style "width" "100%" --(fromFloat pixelWidth ++ "px")
        , HtmlAttr.style "height" "100%" --(fromFloat pixelHeight ++ "px")
        , HtmlAttr.style "position" "fixed"
        , HtmlAttr.style "background" "rgb(246,227,197)"
        ]
        [ Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "98.5%"
            ]
            [ Svg.image
                [ SvgAttr.xlinkHref "assets/test.png"
                , SvgAttr.x (fromFloat ((w - pixelWidth_background * r) / 2) ++ "px")
                , SvgAttr.y (fromFloat ((h - pixelHeight_background * r) / 2 - 3600 + 1620 + (time - 4900) * (4900 - 4000) * 2.5 / 500 + (4900 - time) * (time - 4900) / 550) ++ "px")
                , SvgAttr.width "600px"
                , SvgAttr.height "4500px"
                ]
                []
            ]
        , view_scene2_corridor w h
        , view_goal model w h r
        ]


view_scene2_2 : Model -> Float -> Float -> Float -> Float -> Html Msg
view_scene2_2 model w h r time =
    div
        [ HtmlAttr.style "width" "100%" --(fromFloat pixelWidth ++ "px")
        , HtmlAttr.style "height" "100%" --(fromFloat pixelHeight ++ "px")
        , HtmlAttr.style "position" "fixed"
        , HtmlAttr.style "background" "rgb(246,227,197)"
        ]
        [ Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "98.5%"
            ]
            [ Svg.image
                [ SvgAttr.xlinkHref "assets/test.png"
                , SvgAttr.x (fromFloat ((w - pixelWidth_background * r) / 2) ++ "px")
                , SvgAttr.y (fromFloat ((h - pixelHeight_background * r) / 2 - 3600 + (time - 4000) * (time - 4000) / 500) ++ "px")
                , SvgAttr.width "600px"
                , SvgAttr.height "4500px"
                ]
                []
            ]
        , view_scene2_corridor w h
        ]


view_scene2_1 : Model -> Float -> Float -> Float -> Html Msg
view_scene2_1 model w h r =
    div
        [ HtmlAttr.style "width" "100%" --(fromFloat pixelWidth ++ "px")
        , HtmlAttr.style "height" "100%" --(fromFloat pixelHeight ++ "px")
        , HtmlAttr.style "position" "fixed"
        , HtmlAttr.style "background" "rgb(246,227,197)"
        ]
        [ Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "98.5%"
            ]
            [ Svg.image
                [ SvgAttr.xlinkHref "assets/test.png"
                , SvgAttr.x (fromFloat ((w - pixelWidth_background * r) / 2) ++ "px")
                , SvgAttr.y (fromFloat ((h - pixelHeight_background * r) / 2 - 3600) ++ "px")
                , SvgAttr.width "600px"
                , SvgAttr.height "4500px"
                ]
                []
            ]
        , view_scene2_corridor w h
        ]


view_scene2_corridor : Float -> Float -> Html Msg
view_scene2_corridor w h =
    div
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "fixed"
        , HtmlAttr.style "left" (fromFloat (w / 6 - 20) ++ "px")
        , HtmlAttr.style "top" (fromFloat (h / 2 - 10) ++ "px")
        , HtmlAttr.style "font-size" "20px"
        , HtmlAttr.style "font-family" "Comic Sans MS"
        , HtmlAttr.style "color" "rgb(119,67,96)"
        ]
        [ text "THIS IS THE CORRIDOR!" ]


view_goal : Model -> Float -> Float -> Float -> Svg Msg
view_goal model w h r =
    let
        time =
            model.time
    in
    if time >= 5300 && time <= 5700 then
        Svg.image
            [ SvgAttr.xlinkHref "assets/goal.png"
            , SvgAttr.x (fromFloat ((w - pixelWidth_background * r) / 2 + 125) ++ "px")
            , SvgAttr.y (fromFloat ((h - pixelHeight_background * r) / 2 - 3600 + 1620 + (time - 4900) * (4900 - 4000) * 2.5 / 500 + (4900 - time) * (time - 4900) / 550 - 36) ++ "px")
            , SvgAttr.width "355px"
            ]
            []

    else
        Svg.image
            [ SvgAttr.xlinkHref "assets/goal.png"
            , SvgAttr.x (fromFloat ((w - pixelWidth_background * r) / 2 + 125) ++ "px")
            , SvgAttr.y (fromFloat ((h - pixelHeight_background * r) / 2 - 3600 + 1620 + (5700 - 4900) * (4900 - 4000) * 2.5 / 500 + (4900 - 5700) * (5700 - 4900) / 550 - 36) ++ "px")
            , SvgAttr.width "355px"
            ]
            []


view_text : Model -> Float -> Float -> Float -> Html Msg
view_text model w h r =
    let
        stage =
            model.playframe_0.stage

        state =
            model.state

        ( txt1, txt2 ) =
            case stage of
                1 ->
                    ( "MOVE TO YOUR FRIEND AND", "HE WILL STAND WITH YOU!" )

                2 ->
                    ( "MOVE TO THE PASSERBY AND", "HE WILL IGNORE YOU!" )

                3 ->
                    ( "MOVE TO THE BULLY AND", "HE WILL TAKE ONE OF YOU!" )

                4 ->
                    ( "LET'S SEE WHAT WILL HAAPPEN", "WHEN BALL REACHES PEOPLE!" )

                5 ->
                    case state of
                        Death ->
                            ( "WHEN BALL REACHES YOUR FRIEND", "HE WILL COME TO YOU!" )

                        _ ->
                            ( "", "" )

                6 ->
                    case state of
                        Death ->
                            ( "WHEN BALL REACHES THE PASSERBY", "HE WILL SIMPLY KICK BACK THE BALL AND RUN AWAY!" )

                        _ ->
                            ( "", "" )

                7 ->
                    case state of
                        Death ->
                            ( "WHEN BALL REACHES THE BULLY", "DEDUCTIONS WILL FOLLOW!" )

                        _ ->
                            ( "", "" )

                8 ->
                    case state of
                        Death ->
                            ( "NOW IT SEEMS THAT THE BULLY", "IS GOING TO GET THE BALL, IT'S TIME FOR LENGENDARY" )

                        _ ->
                            ( "", "" )

                9 ->
                    case state of
                        Death ->
                            ( "GOOD JOB!", "YOU DESTROY THE BULLY AND GET HUGE POINTS" )

                        _ ->
                            ( "", "" )

                _ ->
                    ( "", "" )
    in
    div
        [ HtmlAttr.style "width" "100%" --(fromFloat pixelWidth ++ "px")
        , HtmlAttr.style "height" "100%" --(fromFloat pixelHeight ++ "px")
        , HtmlAttr.style "position" "fixed"
        ]
        [ Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "100%"
            ]
            [ Svg.text_
                [ SvgAttr.x (fromFloat (w / 8 - 200) ++ "px")
                , SvgAttr.y (fromFloat (h / 3 - 20) ++ "px")
                , SvgAttr.fill "rgb(223,120,97)"
                , SvgAttr.fontSize "20px"
                , SvgAttr.fontFamily "Comic Sans MS"
                ]
                [ text txt1 ]
            , Svg.text_
                [ SvgAttr.x (fromFloat (w / 8 - 200) ++ "px")
                , SvgAttr.y (fromFloat (h / 3 + 20) ++ "px")
                , SvgAttr.fill "rgb(223,120,97)"
                , SvgAttr.fontSize "20px"
                , SvgAttr.fontFamily "Comic Sans MS"
                ]
                [ text txt2 ]
            ]
        ]


view_text_continue : Model -> Float -> Float -> Float -> Html Msg
view_text_continue model w h r =
    let
        stage =
            model.playframe_0.stage

        state =
            model.state

        txt =
            case stage of
                4 ->
                    case state of
                        Death ->
                            "PRESS 'N' TO CONTINUE"

                        _ ->
                            ""

                5 ->
                    case state of
                        Death ->
                            "PRESS 'N' TO CONTINUE"

                        _ ->
                            ""

                6 ->
                    case state of
                        Death ->
                            "PRESS 'N' TO CONTINUE"

                        _ ->
                            ""

                7 ->
                    case state of
                        Death ->
                            "PRESS 'N' TO CONTINUE"

                        _ ->
                            ""

                8 ->
                    case state of
                        Death ->
                            "PRESS 'A' TO RELEASE POWER"

                        _ ->
                            ""

                9 ->
                    case state of
                        Death ->
                            "PRESS 'N' TO CONTINUE"

                        _ ->
                            ""

                _ ->
                    ""
    in
    div
        [ HtmlAttr.style "width" "100%" --(fromFloat pixelWidth ++ "px")
        , HtmlAttr.style "height" "100%" --(fromFloat pixelHeight ++ "px")
        , HtmlAttr.style "position" "fixed"
        ]
        [ Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "100%"
            ]
            [ Svg.text_
                [ SvgAttr.x (fromFloat (w / 8 - 150) ++ "px")
                , SvgAttr.y (fromFloat (h / 2 + 10) ++ "px")
                , SvgAttr.fill "rgb(119,67,96)"
                , SvgAttr.fontSize "20px"
                , SvgAttr.fontFamily "Comic Sans MS"
                ]
                [ text txt ]
            ]
        ]
