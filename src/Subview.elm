module Subview exposing (..)

import Blocks exposing (RecSize, Rectangle)
import Color exposing (Color)
import Debug exposing (..)
import General exposing (Identity(..), LiveStatus(..), Point, mode)
import Html exposing (..)
import Html.Attributes as HtmlAttr exposing (..)
import Html.Events exposing (keyCode, onClick, onInput)
import Introduction exposing (view_scene, view_scene1)
import Json.Decode as Decode
import Model exposing (Model)
import Msg exposing (Msg(..))
import Objects exposing (Ball, Levelframe, Model_State(..), Paddle)
import String exposing (fromFloat, fromInt)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import UI exposing (viewUI)


pixelWidth : Float
pixelWidth =
    600


pixelHeight : Float
pixelHeight =
    900


viewRec : Float -> Float -> Float -> Model -> { a | anchor : Point, size : RecSize, identity : Identity } -> Svg Msg
viewRec w h r model rectangle =
    let
        x =
            fromFloat ((rectangle.anchor.x - rectangle.size.halflength) * 600 / 1024 + ((w - pixelWidth * r) / 2))

        y =
            fromFloat ((rectangle.anchor.y - rectangle.size.halfwidth) * 900 / 1536 + ((h - pixelHeight * r) / 2))

        --798 here should be 800, just some modification for the photo issue
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


viewBall : Float -> Float -> Float -> Model -> Ball -> Html Msg
viewBall w h r model ball =
    let
        anchor =
            ball.anchor

        radius =
            ball.radius

        sx =
            fromFloat ((anchor.x - radius) * 600 / 1024 + ((w - pixelWidth * r) / 2))

        sy =
            fromFloat ((anchor.y - radius) * 900 / 1536 + ((h - pixelHeight * r) / 2))
    in
    Svg.image
        [ SvgAttr.xlinkHref "assets/soccer_1616.png"
        , SvgAttr.x (sx ++ "px")
        , SvgAttr.y (sy ++ "px")
        ]
        []


viewBackButton : Html Msg
viewBackButton =
    Html.button
        (List.append
            [ Html.Events.onClick Back
            , style "background" "rgb(248,241,228)"
            , style "border" "0"
            , style "bottom" "50%"
            , style "color" "rgb(248,116,116)"
            , style "cursor" "pointer"
            , style "font-family" "Helvetica, Arial, sans-serif"
            , style "display" "block"
            , style "font-size" "18px"
            ]
            [ style "font-weight" "300"
            , style "height" "60px"
            , style "left" "25%"
            , style "top" "60%"
            , style "line-height" "60px"
            , style "outline" "none"
            , style "position" "absolute"
            , style "width" "120px"
            , style "border-radius" "10%"
            ]
        )
        [ text "Back to Start" ]


viewHiddenButton : Html Msg
viewHiddenButton =
    Html.button
        (List.append
            [ Html.Events.onClick HiddenLevel
            , style "background" "rgb(248,241,228)"
            , style "border" "0"
            , style "bottom" "20%"
            , style "color" "rgb(248,116,116)"
            , style "cursor" "pointer"
            , style "font-family" "Helvetica, Arial, sans-serif"
            , style "display" "block"
            , style "font-size" "18px"
            ]
            [ style "font-weight" "300"
            , style "height" "60px"
            , style "left" "65%"
            , style "top" "60%"
            , style "line-height" "60px"
            , style "outline" "none"
            , style "position" "absolute"
            , style "width" "120px"
            , style "border-radius" "10%"
            ]
        )
        [ text "Hidden Level" ]


viewPaddle : Model -> Float -> Float -> Float -> Html Msg
viewPaddle model w h r =
    if model.playframe.paddle.length >= 1 then
        div
            [ HtmlAttr.style "width" (fromFloat (2 * 32 * 600 / 1024) ++ "px")
            , HtmlAttr.style "height" (fromFloat (2 * 48 * 900 / 1536) ++ "px")
            , HtmlAttr.style "position" "fixed"
            , HtmlAttr.style "left" (fromFloat ((model.playframe.paddle.anchor.x - 32 * model.playframe.paddle.length) * 600 / 1024 + ((w - pixelWidth * r) / 2)) ++ "px") --"500px"
            , HtmlAttr.style "top" (fromFloat ((model.playframe.paddle.anchor.y - 48) * 900 / 1536 + ((h - pixelHeight * r) / 2)) ++ "px") --"80px"
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

    else
        div
            []
            []


changeInttoFloat : Float -> Float -> Int -> Float
changeInttoFloat add mul ori =
    (mul * toFloat ori) + add


changextoRec : Float -> Float -> { anchor : Point, size : RecSize }
changextoRec y x =
    let
        anchor =
            Point x y

        size =
            RecSize 32 48
    in
    { anchor = anchor, size = size }


viewCreateIntro : Html Msg
viewCreateIntro =
    div
        [ HtmlAttr.style "position" "fixed"
        , HtmlAttr.style "top" "150px"
        , HtmlAttr.style "left" "500px"
        , HtmlAttr.style "color" "red"
        , HtmlAttr.style "font-family" "Comic Sans MS"
        , HtmlAttr.style "display" "block"
        , HtmlAttr.style "font-size" "35px"
        ]
        [ text "Welcome to Creative Zone!" ]


getText : Float -> String
getText order =
    if order == 0 then
        "change friend probability here"

    else if order == 1 then
        "change passerby probability here"

    else if order == 2 then
        "change enemy probability here"

    else
        "change the interval of generating people here"


view_cover : Model -> Float -> Float -> Float -> Html Msg
view_cover model w h r =
    let
        time =
            model.time

        --798 here should be 800, just some modification for the photo issue
    in
    div
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "margin" "auto"
        , HtmlAttr.style "background" "rgb(246,227,197)"
        ]
        [ Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "100%"
            ]
            [ Svg.image
                [ SvgAttr.xlinkHref "assets/background_bully.png"
                , SvgAttr.imageRendering "pixelated"
                , SvgAttr.width "300px"
                , SvgAttr.height "600px"
                , SvgAttr.x (fromFloat ((w / 2) - 450 - time / 5))
                , SvgAttr.y "200"
                ]
                []
            , Svg.image
                [ SvgAttr.xlinkHref "assets/background_character.png"
                , SvgAttr.imageRendering "pixelated"
                , SvgAttr.width "300px"
                , SvgAttr.height "600px"
                , SvgAttr.x (fromFloat ((w / 2) + 150 + time / 5))
                , SvgAttr.y "200"
                ]
                []
            , Svg.text_
                [ SvgAttr.fontFamily "Comix Sans MS"
                , SvgAttr.x (fromFloat ((w / 2) - 130))
                , SvgAttr.y (fromFloat (h / 1.1))
                , SvgAttr.fill "rgb(119,67,96)"
                , SvgAttr.fontSize "20px"
                ]
                [ text "PRESS 'Enter' TO START" ]
            ]
        ]


tellNext : Int -> Html Msg
tellNext level =
    if level == 3 || level == 5 then
        div
            []
            []

    else
        div
            [ HtmlAttr.style "postion" "absolute"
            , HtmlAttr.style "top" "800px"
            , HtmlAttr.style "left" "900px"
            , HtmlAttr.style "font-size" "30px"
            , HtmlAttr.style "font-family" "Comic Sans MS"
            , HtmlAttr.style "color" "rgb(119,67,96)"
            ]
            [ text "press N to continue ->" ]


getPuretext : Float -> Int -> String
getPuretext order level =
    if level == 0 then
        if order == 1 then
            "Now Start to Play~"

        else if order == 2 then
            "Enjoy the game!"

        else
            ""

    else if level == 1 then
        if order == 1 then
            "Great Attempt! You pass your first obstacle!"

        else if order == 2 then
            "Now let's try another!"

        else
            ""

    else if level == 2 then
        if order == 1 then
            "What a great job you've done!"

        else if order == 2 then
            "But harder things hide.."

        else
            ""

    else if level == 3 then
        if order == 1 then
            "Congratulations! You've passed the game!"

        else if order == 2 then
            "feel free to explore more!"

        else
            ""

    else if level == 4 then
        if order == 1 then
            "In next infinite level, you can set your own data"

        else
            "simply press pause in next level"

    else if level == 5 then
        if order == 1 then
            "Thanks for playing our game"

        else
            "Life is one thing must be experienced"

    else
        ""


judge_text_url : Float -> ( String, String, String )
judge_text_url time =
    if time <= 10000 then
        ( "The class for today is over!", "", "assets/standard_classroom_2.png" )

    else if time <= 17000 then
        ( "As a student who loves soccer very much", "you are going to play soccer with your buddies now", "assets/buddy.png" )

    else if time <= 24000 then
        ( "However, when you are packing your books, soccer, and pens into your backpack", "the backdoor of the classroom is cracked by the bully", "assets/background_bully.png" )

    else if time <= 31000 then
        ( "'Give me your tiny ball!' He shouts", "with some of his pawns stepping towards you", "assets/bullier_1.png" )

    else if time <= 38000 then
        ( "You have to do something to escape and protect your ball!", "", "assets/soccer_1616.png" )

    else if time <= 45000 then
        ( " You grab your backpack with your right hand,  chips the ball over the bully's head", "pass through two pawns when the flying soccer attracts all their attention", "" )

    else if time <= 52000 then
        ( "and rush out the classroom from the front door......", "", "" )

    else
        ( "", "", "" )


view_skip : Html Msg
view_skip =
    Html.button
        (List.append
            [ Html.Events.onClick Skip
            , style "background" "rgb(248,241,228)"
            , style "border" "0"
            , style "bottom" "20%"
            , style "color" "rgb(248,116,116)"
            , style "cursor" "pointer"
            , style "font-family" "Comic Sans MS"
            , style "display" "block"
            , style "font-size" "18px"
            ]
            [ style "font-weight" "300"
            , style "height" "60px"
            , style "left" "85%"
            , style "line-height" "60px"
            , style "outline" "none"
            , style "position" "absolute"
            , style "width" "120px"
            , style "border-radius" "10%"
            ]
        )
        [ text "Skip" ]


view_back : Model -> Html Msg
view_back model =
    let
        txt =
            case model.page.state of
                Objects.HE int ->
                    "Replay"

                _ ->
                    "Back"
    in
    Html.button
        (List.append
            [ Html.Events.onClick Back
            , style "background" "rgb(248,241,228)"
            , style "border" "0"
            , style "bottom" "80%"
            , style "color" "rgb(248,116,116)"
            , style "cursor" "pointer"
            , style "font-family" "Comic Sans MS"
            , style "display" "block"
            , style "font-size" "18px"
            ]
            [ style "font-weight" "300"
            , style "height" "60px"
            , style "left" "3%"
            , style "top" "20%"
            , style "line-height" "60px"
            , style "outline" "none"
            , style "position" "absolute"
            , style "width" "120px"
            , style "border-radius" "10%"
            ]
        )
        [ text txt ]


view_logo : Model -> Float -> Float -> Float -> Html Msg
view_logo model w h r =
    let
        time =
            model.time

        opacity =
            if time <= 1500 then
                time * time / 2250000

            else
                (time - 3000) * (time - 3000) / 2250000
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
            [ Svg.rect
                [ SvgAttr.x "0"
                , SvgAttr.y "0"
                , SvgAttr.width (fromFloat w ++ "px")
                , SvgAttr.height (fromFloat h ++ "px")
                , SvgAttr.fill "rgb(248,241,228)"
                ]
                []
            , Svg.image
                [ SvgAttr.xlinkHref "assets/Logo.png"
                , SvgAttr.x (fromFloat ((w - 600 * r) / 2) ++ "px")
                , SvgAttr.y (fromFloat ((h - 600 * r) / 2) ++ "px")
                , SvgAttr.width "600px"
                , SvgAttr.height "600px"
                , SvgAttr.opacity (fromFloat opacity)
                ]
                []
            ]
        ]


view_Goal : Float -> Float -> Float -> Float -> Int -> Html Msg
view_Goal w h r flow level =
    if level <= 3 then
        div
            [ HtmlAttr.style "width" "100%"
            , HtmlAttr.style "height" "100%"
            , HtmlAttr.style "position" "fixed"
            ]
            [ Svg.svg
                [ SvgAttr.width "100%"
                , SvgAttr.height "100%"
                ]
                [ Svg.image
                    [ SvgAttr.xlinkHref "assets/goal.png"
                    , SvgAttr.x (fromFloat ((w - pixelWidth * r) / 2 + 122) ++ "px")
                    , if level <= 3 then
                        if flow <= 3600 then
                            SvgAttr.y (fromFloat ((h - pixelHeight * r) / 2 - 3600 + flow) ++ "px")

                        else
                            SvgAttr.y (fromFloat ((h - pixelHeight * r) / 2) ++ "px")

                      else
                        SvgAttr.y (fromFloat ((h - pixelHeight * r) / 2 - 3600 + mode flow 2700 + toFloat (floor (flow / 2700)) * 900) ++ "px")
                    , SvgAttr.width "360px"
                    ]
                    []
                ]
            ]

    else
        div [] []


view_points_power : Model -> Float -> Float -> Float -> Html Msg
view_points_power model w h r =
    let
        score =
            model.playframe.ball.score

        legendp =
            model.playframe.ball.legendStore
    in
    div
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "fixed"
        ]
        [ Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "100%"
            ]
            [ Svg.text_
                [ SvgAttr.x (fromFloat (3 * w / 4 - 10) ++ "px")
                , SvgAttr.y (fromFloat (h / 5 - 100) ++ "px")
                , SvgAttr.fill "rgb(223,120,97)"
                , SvgAttr.fontSize "50px"
                , SvgAttr.fontFamily "Comic Sans MS"
                ]
                [ text "SCORE:" ]
            , Svg.text_
                [ SvgAttr.x (fromFloat (3 * w / 4 + 40) ++ "px")
                , SvgAttr.y (fromFloat (h / 5) ++ "px")
                , SvgAttr.fill "rgb(223,120,97)"
                , SvgAttr.fontSize "50px"
                , SvgAttr.fontFamily "Comic Sans MS"
                ]
                [ text (fromFloat score) ]
            , Svg.text_
                [ SvgAttr.x (fromFloat (1 * w / 6 - 10) ++ "px")
                , SvgAttr.y (fromFloat (h / 5 - 100) ++ "px")
                , SvgAttr.fill "rgb(223,120,97)"
                , SvgAttr.fontSize "50px"
                , SvgAttr.fontFamily "Comic Sans MS"
                ]
                [ text "POWER:" ]
            , Svg.rect
                [ SvgAttr.x (fromFloat (1 * w / 6 + 60) ++ "px")
                , SvgAttr.y (fromFloat (h / 5 - 50) ++ "px")
                , SvgAttr.width (fromFloat model.playframe.ball.legendStore ++ "px") --need to implement the value of power
                , SvgAttr.height "50px"
                , SvgAttr.rx "20px"
                , SvgAttr.ry "20px"
                , SvgAttr.fill ("rgb(" ++ fromFloat legendp ++ ", 120,97)") --here it changes the color with time, but it can be set as changed with the value of power
                ]
                []
            ]
        ]


viewEnd_button : Html Msg
viewEnd_button =
    Html.button
        (List.append
            [ Html.Events.onClick Start_Restart
            , style "background" "rgb(248,241,228)"
            , style "border" "0"
            , style "bottom" "20%"
            , style "color" "rgb(248,116,116)"
            , style "cursor" "pointer"
            , style "font-family" "Comic Sans MS"
            , style "display" "block"
            , style "font-size" "18px"
            ]
            [ style "font-weight" "300"
            , style "height" "60px"
            , style "left" "85%"
            , style "line-height" "60px"
            , style "outline" "none"
            , style "position" "absolute"
            , style "width" "120px"
            , style "border-radius" "10%"
            ]
        )
        [ text "Restart" ]


view_backpeople : Float -> Float -> Float -> Html Msg
view_backpeople w h r =
    div
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "margin" "auto"
        ]
        [ Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "100%"
            ]
            [ Svg.image
                [ SvgAttr.xlinkHref "assets/background_bully.png"
                , SvgAttr.imageRendering "pixelated"
                , SvgAttr.width "300px"
                , SvgAttr.height "600px"
                , SvgAttr.x (fromFloat ((w / 2) - 450 - 1500 / 5))
                , SvgAttr.y "200"
                ]
                []
            , Svg.image
                [ SvgAttr.xlinkHref "assets/background_character.png"
                , SvgAttr.imageRendering "pixelated"
                , SvgAttr.width "300px"
                , SvgAttr.height "600px"
                , SvgAttr.x (fromFloat ((w / 2) + 150 + 1500 / 5))
                , SvgAttr.y "200"
                ]
                []
            ]
        ]
