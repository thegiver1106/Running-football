module Introduction exposing (updateintro, updatemoveIntro, view_scene, view_scene1)

import Blocks exposing (RecSize, Rectangle)
import Debug exposing (..)
import General exposing (ConstructStatus(..), Dir(..), Identity(..), LegendStatus(..), LiveStatus(..), Point, Vec, getxval, vecNormalize)
import Html exposing (..)
import Html.Attributes as HtmlAttr exposing (..)
import Html.Events exposing (keyCode)
import Interaction exposing (..)
import Json.Decode as Decode
import Model exposing (CreativeZone, Model, alllevel, initModel)
import Msg exposing (Msg(..))
import Objects exposing (Ball, CrashDir(..), Door, Existence(..), Key_state(..), Model_State(..), Paddle, PaddleStatus(..), Page, Pagetype(..), Person, Playframe_0)
import String exposing (fromFloat, fromInt)
import Subintroduction exposing (..)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import UI exposing (viewUI)


pixelWidth : Float
pixelWidth =
    400


pixelHeight : Float
pixelHeight =
    400


pixelWidth_background : Float
pixelWidth_background =
    600


pixelHeight_background : Float
pixelHeight_background =
    900


updatemoveIntro : Msg -> Model -> ( Model, Cmd Msg )
updatemoveIntro msg model =
    let
        playframe_0 =
            model.playframe_0
    in
    case msg of
        Tick elapsed ->
            let
                npaddle =
                    generatePaddle model.playframe_0.paddle elapsed
            in
            ( { model
                | playframe_0 =
                    { playframe_0 | paddle = npaddle }
              }
                |> judge_at_place
            , Cmd.none
            )

        Way dir ->
            case dir of
                Right ->
                    ( { model | playframe_0 = { playframe_0 | paddle = directedPaddle playframe_0.paddle dir }, key_right = Key_down }, Cmd.none )

                Left ->
                    ( { model | playframe_0 = { playframe_0 | paddle = directedPaddle playframe_0.paddle dir }, key_left = Key_down }, Cmd.none )

        Stop dir ->
            case dir of
                Right ->
                    case model.key_left of
                        Key_up ->
                            ( { model | playframe_0 = { playframe_0 | paddle = staticPaddle playframe_0.paddle }, key_right = Key_up }, Cmd.none )

                        Key_down ->
                            ( { model | key_right = Key_up }, Cmd.none )

                Left ->
                    case model.key_right of
                        Key_up ->
                            ( { model | playframe_0 = { playframe_0 | paddle = staticPaddle playframe_0.paddle }, key_left = Key_up }, Cmd.none )

                        Key_down ->
                            ( { model | key_left = Key_up }, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateintro : Model -> Msg -> ( Model, Cmd Msg )
updateintro model msg =
    let
        page =
            model.page

        playframe_0 =
            model.playframe_0
    in
    case msg of
        Skip ->
            ( { model | page = { page | state = Turn 0 }, state = Pass } |> setzeroTime, Cmd.none )

        --remove may
        _ ->
            if model.time <= 8000 then
                case msg of
                    Tick elapsed ->
                        ( { model
                            | time = model.time + elapsed
                          }
                        , Cmd.none
                        )

                    _ ->
                        ( model, Cmd.none )

            else if model.playframe_0.stage <= 10 then
                --intro part stage
                case model.state of
                    Pass ->
                        updatemoveIntro msg model

                    Init ->
                        updatemoveIntro msg model

                    Paused ->
                        case msg of
                            Tick elapsed ->
                                let
                                    movedball =
                                        updateBall elapsed model.playframe_0.ball

                                    npaddle =
                                        model.playframe_0.paddle

                                    ( paddledBall, newPaddle ) =
                                        checkPaddleBallCrash movedball npaddle

                                    ( finalBall, finalList, finalPaddle ) =
                                        updateBricksByBall paddledBall model.playframe_0.allPersonList newPaddle
                                in
                                ( { model
                                    | playframe_0 =
                                        { playframe_0 | ball = finalBall }
                                    , time = model.time + elapsed
                                  }
                                    |> judge_at_place
                                , Cmd.none
                                )

                            _ ->
                                ( model, Cmd.none )

                    Death ->
                        case msg of
                            Tick elapsed ->
                                ( { model
                                    | time = model.time + elapsed
                                  }
                                , Cmd.none
                                )

                            Press_N ->
                                let
                                    stage =
                                        model.playframe_0.stage
                                in
                                if stage == 8 then
                                    ( model, Cmd.none )

                                else
                                    ( { model | state = Paused }, Cmd.none )

                            StartLegend ->
                                let
                                    stage =
                                        model.playframe_0.stage
                                in
                                if stage == 8 then
                                    ( { model | state = Paused }, Cmd.none )

                                else
                                    ( model, Cmd.none )

                            _ ->
                                ( model, Cmd.none )

                    _ ->
                        ( model, Cmd.none )

            else
                ( { model | page = { page | state = Turn 0 }, state = Pass }
                    |> setzeroTime
                , Cmd.none
                )


view_scene : Model -> Float -> Float -> Float -> Html Msg
view_scene model w h r =
    let
        time =
            model.time
    in
    if time <= 2000 then
        view_scene1 model w h r

    else if time <= 8000 then
        view_scene2 model w h r

    else
        view_paddle_people_back model w h r


view_paddle_people_back : Model -> Float -> Float -> Float -> Html Msg
view_paddle_people_back model w h r =
    div
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "fixed"
        , HtmlAttr.style "background" "rgb(246,227,197)"
        ]
        [ view_Paddle model w h r
        , view_People_ball model w h r
        , view_text model w h r
        , view_shadow model w h r
        , view_score model w h r
        , view_power_0 model w h r
        , view_text_continue model w h r
        , Svg.svg
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
        ]


view_Rec : Float -> Float -> Float -> Model -> { a | anchor : Point, size : RecSize, identity : Identity } -> Svg Msg
view_Rec w h r model rectangle =
    let
        x =
            fromFloat ((rectangle.anchor.x - rectangle.size.halflength) * 600 / 1024 + ((w - pixelWidth_background * r) / 2))

        y =
            fromFloat ((rectangle.anchor.y - rectangle.size.halfwidth) * 900 / 1536 + ((h - pixelHeight_background * r) / 2))

        wid =
            fromFloat (2 * rectangle.size.halflength * 600 / 1024)

        len =
            fromFloat (2 * rectangle.size.halfwidth * 900 / 1536)

        picture_url =
            case rectangle.identity of
                Enemy ->
                    "assets/bullier_1.png"

                Friend ->
                    "assets/buddy.png"

                Passerby ->
                    "assets/crowd_1.png"

                Me ->
                    "assets/main_character.png"
    in
    Svg.image
        [ SvgAttr.xlinkHref picture_url
        , SvgAttr.x (x ++ "px")
        , SvgAttr.y (y ++ "px")
        , SvgAttr.width (wid ++ "px")
        , SvgAttr.height (len ++ "px")
        , SvgAttr.scale "10"
        ]
        []


view_score : Model -> Float -> Float -> Float -> Html Msg
view_score model w h r =
    let
        score =
            case model.playframe_0.stage of
                1 ->
                    "0"

                2 ->
                    "0"

                3 ->
                    "0"

                4 ->
                    "0"

                5 ->
                    "10"

                6 ->
                    "15"

                7 ->
                    "5"

                8 ->
                    "5"

                _ ->
                    "25"
    in
    div
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "fixed"
        , HtmlAttr.style "left" (fromFloat (w / 1.3) ++ "px")
        , HtmlAttr.style "top" (fromFloat (h / 5) ++ "px")
        , HtmlAttr.style "font-size" "20px"
        , HtmlAttr.style "font-family" "Comic Sans MS"
        , HtmlAttr.style "color" "rgb(119,67,96)"
        ]
        [ text ("score: " ++ score)
        ]


view_power_0 : Model -> Float -> Float -> Float -> Html Msg
view_power_0 model w h r =
    let
        length =
            model.playframe_0.power * 5
    in
    div
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "fixed"
        , HtmlAttr.style "left" "0"
        , HtmlAttr.style "top" "0"
        ]
        [ Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "100%"
            ]
            [ Svg.rect
                [ SvgAttr.x (fromFloat (w / 1.3) ++ "px")
                , SvgAttr.y (fromFloat (h / 3) ++ "px")
                , SvgAttr.width (fromInt length ++ "px")
                , SvgAttr.height "50px"
                , SvgAttr.rx "20px"
                , SvgAttr.ry "20px"
                , SvgAttr.fill "red"
                ]
                []
            ]
        , div
            [ HtmlAttr.style "width" "100%"
            , HtmlAttr.style "height" "100%"
            , HtmlAttr.style "position" "fixed"
            , HtmlAttr.style "left" (fromFloat (w / 1.3) ++ "px")
            , HtmlAttr.style "top" (fromFloat (h / 3 - 15) ++ "px")
            , HtmlAttr.style "font-size" "20px"
            , HtmlAttr.style "font-family" "Comic Sans MS"
            , HtmlAttr.style "color" "rgb(119,67,96)"
            ]
            [ text "POWER" ]
        ]


view_Ball : Float -> Float -> Float -> Ball -> Html Msg
view_Ball w h r ball =
    let
        anchor =
            ball.anchor

        radius =
            ball.radius

        sx =
            fromFloat ((anchor.x - radius) * 600 / 1024 + ((w - pixelWidth_background * r) / 2))

        sy =
            fromFloat ((anchor.y - radius) * 900 / 1536 + ((h - pixelHeight_background * r) / 2))
    in
    Svg.image
        [ SvgAttr.xlinkHref "assets/soccer_1616.png"
        , SvgAttr.x (sx ++ "px")
        , SvgAttr.y (sy ++ "px")
        ]
        []


view_shadow : Model -> Float -> Float -> Float -> Html Msg
view_shadow model w h r =
    let
        stage =
            model.playframe_0.stage

        state =
            model.state
    in
    if stage == 1 then
        view_shadow1 w h r

    else if stage == 2 then
        view_shadow2 w h r

    else if stage == 3 then
        view_shadow3 w h r

    else if stage >= 5 && stage <= 7 && state == Death then
        view_shadow4 w h r

    else
        div [] []


view_People_ball : Model -> Float -> Float -> Float -> Html Msg
view_People_ball model w h r =
    div
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "fixed"
        ]
        [ Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "100%"
            ]
            (List.append (List.map (view_Rec w h r model) model.playframe_0.allPersonList) [ view_Ball w h r model.playframe_0.ball ])
        ]


view_Paddle : Model -> Float -> Float -> Float -> Html Msg
view_Paddle model w h r =
    if model.playframe_0.stage == 2 || model.playframe_0.stage == 3 || model.playframe_0.stage == 5 || model.playframe_0.stage == 6 then
        div
            [ HtmlAttr.style "width" "100%"
            , HtmlAttr.style "height" "100%"
            , HtmlAttr.style "position" "fixed"
            ]
            [ view_maincharacter model w h r
            , div
                [ HtmlAttr.style "width" (fromFloat (2 * model.playframe_0.paddle.size.halflength * 600 / 1024) ++ "px")
                , HtmlAttr.style "height" (fromFloat (2 * model.playframe_0.paddle.size.halfwidth * 900 / 1536) ++ "px")
                , HtmlAttr.style "position" "fixed"
                , HtmlAttr.style "left" (fromFloat ((model.playframe_0.paddle.anchor.x - model.playframe_0.paddle.size.halflength + 64) * 600 / 1024 + ((w - pixelWidth_background * r) / 2)) ++ "px") --"500px"
                , HtmlAttr.style "top" (fromFloat ((model.playframe_0.paddle.anchor.y - model.playframe_0.paddle.size.halfwidth) * 900 / 1536 + ((h - pixelHeight_background * r) / 2)) ++ "px") --"80px"
                , HtmlAttr.style "transform-origin" "0 0"
                , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat r ++ ")")
                ]
                [ Html.img
                    [ HtmlAttr.src "assets/buddy.png"
                    , HtmlAttr.style "width" "100%"
                    , HtmlAttr.style "height" "100%"
                    ]
                    []
                ]
            ]

    else
        view_maincharacter model w h r


view_maincharacter : Model -> Float -> Float -> Float -> Html Msg
view_maincharacter model w h r =
    div
        [ HtmlAttr.style "width" (fromFloat (2 * model.playframe_0.paddle.size.halflength * 600 / 1024) ++ "px")
        , HtmlAttr.style "height" (fromFloat (2 * model.playframe_0.paddle.size.halfwidth * 900 / 1536) ++ "px")
        , HtmlAttr.style "position" "fixed"
        , HtmlAttr.style "left" (fromFloat ((model.playframe_0.paddle.anchor.x - model.playframe_0.paddle.size.halflength) * 600 / 1024 + ((w - pixelWidth_background * r) / 2)) ++ "px") --"500px"
        , HtmlAttr.style "top" (fromFloat ((model.playframe_0.paddle.anchor.y - model.playframe_0.paddle.size.halfwidth) * 900 / 1536 + ((h - pixelHeight_background * r) / 2)) ++ "px") --"80px"
        , HtmlAttr.style "transform-origin" "0 0"
        , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat r ++ ")")
        ]
        [ Html.img
            [ HtmlAttr.src "assets/main_character.png"
            , HtmlAttr.style "width" "100%"
            , HtmlAttr.style "height" "100%"
            ]
            []
        ]


view_scene2 : Model -> Float -> Float -> Float -> Html Msg
view_scene2 model w h r =
    let
        time =
            model.time
    in
    if time <= 4000 then
        view_scene2_1 model w h r

    else if time <= 4900 then
        view_scene2_2 model w h r time

    else if time <= 5700 then
        view_scene2_3 model w h r time

    else if time <= 6800 then
        view_scene2_4 model w h r time

    else
        view_scene2_5 model w h r time


view_scene1 : Model -> Float -> Float -> Float -> Html Msg
view_scene1 model w h r =
    div
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "left" "0"
        , HtmlAttr.style "top" "0"
        , HtmlAttr.style "margin" "auto"
        , HtmlAttr.style "background" "rgb(246,227,197)"
        ]
        [ Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "100%"
            ]
            [ Svg.image
                [ SvgAttr.xlinkHref "assets/main_character.png"
                , SvgAttr.x (fromFloat ((w - pixelWidth * r) / 2) ++ "px")
                , SvgAttr.y (fromFloat ((h - pixelHeight * r) / 2) ++ "px")
                , SvgAttr.width "400px"
                , SvgAttr.height "400px"
                ]
                []
            ]
        , div
            [ HtmlAttr.style "width" "100%"
            , HtmlAttr.style "height" "100%"
            , HtmlAttr.style "position" "fixed"
            , HtmlAttr.style "left" (fromFloat (w / 2 - 70) ++ "px")
            , HtmlAttr.style "top" (fromFloat (h / 4 - 10) ++ "px")
            , HtmlAttr.style "font-size" "20px"
            , HtmlAttr.style "font-family" "Comic Sans MS"
            , HtmlAttr.style "color" "rgb(119,67,96)"
            ]
            [ text "THIS IS YOU!"
            ]
        ]
