module View exposing (pixelHeight, pixelWidth, view)

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
import Subview exposing (..)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import UI exposing (viewUI)


pixelWidth : Float
pixelWidth =
    600


pixelHeight : Float
pixelHeight =
    900


view : Model -> Html Msg
view model =
    let
        ( w, h ) =
            model.size

        r =
            if w / h > pixelWidth / pixelHeight then
                Basics.min 1 (h / pixelHeight)

            else
                Basics.min 1 (w / pixelWidth)
    in
    case model.page.state of
        Objects.Logo ->
            view_start model w h r

        Objects.Start flag ->
            view_cover model w h r

        Objects.Intro ->
            div []
                [ view_scene model w h r
                , view_skip
                ]

        Objects.Turn int ->
            case int of
                0 ->
                    view_turn_level model

                _ ->
                    view_turn_level model

        Objects.Play ->
            view_play model w h r

        Objects.Win stage ->
            view_finish_level model

        Objects.End stage ->
            if stage /= 4 then
                viewEnd w h r model stage

            else
                view_play model w h r

        Objects.HE int ->
            viewHE model

        Objects.Edit ->
            viewChange model

        _ ->
            div [] []


viewHE : Model -> Html Msg
viewHE model =
    if model.score >= 300 then
        div
            [ HtmlAttr.style "width" "100%"
            , HtmlAttr.style "height" "100%"
            , HtmlAttr.style "position" "fixed"
            , HtmlAttr.style "background" "rgb(246,227,197)"
            ]
            [ div
                [ HtmlAttr.style "postion" "fixed"
                , HtmlAttr.style "top" "600px"
                , HtmlAttr.style "left" "900px"
                , HtmlAttr.style "font-size" "30px"
                , HtmlAttr.style "font-family" "Comic Sans MS"
                , HtmlAttr.style "color" "rgb(119,67,96)"
                , HtmlAttr.style "background" "rgb(246,227,197)"
                ]
                [ text "yeeeeee!!! And you got really much score!"
                ]
            , div
                [ HtmlAttr.style "postion" "fixed"
                , HtmlAttr.style "top" "800px"
                , HtmlAttr.style "left" "900px"
                , HtmlAttr.style "font-size" "30px"
                , HtmlAttr.style "font-family" "Comic Sans MS"
                , HtmlAttr.style "color" "rgb(119,67,96)"
                , HtmlAttr.style "background" "rgb(246,227,197)"
                ]
                [ text "go find more pleasure in the hidden level ."
                ]
            , viewHiddenButton
            , viewBackButton
            ]

    else
        view_turn_level model


view_background : Float -> Float -> Float -> Float -> Int -> Html Msg
view_background w h r flow level =
    let
        floow =
            flow - toFloat (floor (flow / 3600)) * 3600.0
    in
    div
        [ HtmlAttr.style "width" "100%" --(fromFloat pixelWidth ++ "px")
        , HtmlAttr.style "height" "100%" --(fromFloat pixelHeight ++ "px")
        , HtmlAttr.style "position" "fixed"
        ]
        [ Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "98.5%"
            ]
            [ Svg.image
                [ SvgAttr.xlinkHref "assets/test.png"
                , SvgAttr.x (fromFloat ((w - pixelWidth * r) / 2) ++ "px")
                , if level <= 3 then
                    if flow <= 3600 then
                        SvgAttr.y (fromFloat ((h - pixelHeight * r) / 2 - 3600 + floow) ++ "px")

                    else
                        SvgAttr.y (fromFloat ((h - pixelHeight * r) / 2) ++ "px")

                  else
                    SvgAttr.y (fromFloat ((h - pixelHeight * r) / 2 - 3600 + mode floow 2700 + toFloat (floor (floow / 2700)) * 900) ++ "px")
                , SvgAttr.width "600px"
                , SvgAttr.height "4500px"
                ]
                []
            ]
        ]


viewsubfriend : Float -> Float -> Float -> { a | anchor : Point, size : RecSize } -> Svg Msg
viewsubfriend w h r rectangle =
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
            "assets/buddy.png"
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


viewsubfriends : Float -> Float -> Float -> Paddle -> List (Svg Msg)
viewsubfriends w h r paddle =
    let
        num =
            paddle.length - 1

        startx =
            paddle.anchor.x - paddle.size.halflength + 32

        y =
            paddle.anchor.y

        friendRec =
            getFriendRect num startx y
    in
    List.map (viewsubfriend w h r) friendRec


getFriendRect : Float -> Float -> Float -> List { anchor : Point, size : RecSize }
getFriendRect num startx y =
    if num <= 0 then
        []

    else
        List.range 1 (round num)
            |> List.map (changeInttoFloat startx 64)
            |> List.map (changextoRec y)


viewChange : Model -> Html Msg
viewChange model =
    if model.state == Paused then
        div [ HtmlAttr.style "z-index" "10" ]
            [ viewInput 0 "text" (fromFloat model.creativeZone.friendProb) (ChangeId Friend)
            , viewInput 1 "text" (fromFloat model.creativeZone.passerbyProb) (ChangeId Passerby)
            , viewInput 2 "text" (fromFloat model.creativeZone.enemyProb) (ChangeId Enemy)
            , viewInput 3 "text" (fromFloat model.creativeZone.geneElapsed) ChangeTime
            , viewResumeButton
            , viewCreateIntro
            ]

    else
        div
            []
            []


viewResumeButton : Html Msg
viewResumeButton =
    div
        (List.append
            [ Html.Events.onClick Resume
            , style "background" "rgb(248,241,228)"
            , style "border" "0"
            , style "bottom" "20%"
            , style "color" "rgb(248,116,116)"
            , style "cursor" "pointer"
            , style "font-family" "Comic Sans MS"
            , style "display" "block"
            ]
            [ style "font-size" "18px"
            , style "font-weight" "300"
            , style "height" "60px"
            , style "left" "85%"
            , style "line-height" "60px"
            , style "outline" "none"
            , style "position" "absolute"
            , style "width" "120px"
            , style "border-radius" "10%"
            ]
        )
        [ text "Resume" ]


viewInput : Float -> String -> String -> (String -> Msg) -> Html Msg
viewInput order t v toMsg =
    div
        [ HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "left" "700px"
        , HtmlAttr.style "top" (fromFloat (50 * order + 400) ++ "px")
        ]
        [ input [ type_ t, value v, onInput toMsg ] []
        , viewIntroWord order
        ]


viewIntroWord : Float -> Html Msg
viewIntroWord order =
    div
        [ HtmlAttr.style "position" "relative"
        , HtmlAttr.style "top" "1px"
        , HtmlAttr.style "color" "blue"
        ]
        [ text (getText order) ]


view_turn_level : Model -> Html Msg
view_turn_level model =
    let
        level =
            model.levelframe.level
    in
    div
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "fixed"
        , HtmlAttr.style "background" "rgb(246,227,197)"
        ]
        [ viewText level
        , tellNext level
        , view_back model
        ]


viewText : Int -> Html Msg
viewText level =
    div
        [ HtmlAttr.style "position" "relative"
        , HtmlAttr.style "top" "400px"
        , HtmlAttr.style "left" "400px"
        ]
        [ viewsubText 1 level
        , viewsubText 2 level
        ]


viewsubText : Float -> Int -> Html Msg
viewsubText order level =
    div
        [ HtmlAttr.style "position" "relative"
        , HtmlAttr.style "top" (fromFloat (order * 50 - 100) ++ "px")
        , HtmlAttr.style "left" "20%"
        , HtmlAttr.style "font-size" "30px"
        , HtmlAttr.style "font-family" "Comic Sans MS"
        , HtmlAttr.style "color" "rgb(119,67,96)"
        ]
        [ text (getPuretext order level) ]


view_start : Model -> Float -> Float -> Float -> Html Msg
view_start model w h r =
    if model.time <= 3000 then
        view_logo model w h r

    else if model.time <= 51000 then
        view_context model w h r

    else
        div [] []


view_context : Model -> Float -> Float -> Float -> Html Msg
view_context model w h r =
    let
        time =
            model.time

        timing =
            toFloat (modBy 7000 (floor (model.time - 3000)))

        opacity =
            if timing <= 2000 then
                (timing + 1000) * (timing + 1000) / 9000000

            else if time <= 5000 then
                1

            else
                (timing - 7000) * (timing - 7000) / 4000000

        ( tet1, tet2, url ) =
            judge_text_url time
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
            , Svg.text_
                [ SvgAttr.x (fromFloat (w / 8) ++ "px")
                , SvgAttr.y (fromFloat (h / 3 - 100) ++ "px")
                , SvgAttr.fill "rgb(119,67,96)"
                , SvgAttr.fontSize "30px"
                , SvgAttr.opacity (fromFloat opacity)
                , SvgAttr.fontFamily "Comic Sans MS"
                ]
                [ text tet1 ]
            , Svg.text_
                [ SvgAttr.x (fromFloat (w / 8) ++ "px")
                , SvgAttr.y (fromFloat (h / 3) ++ "px")
                , SvgAttr.fill "rgb(119,67,96)"
                , SvgAttr.fontSize "30px"
                , SvgAttr.opacity (fromFloat opacity)
                , SvgAttr.fontFamily "Comic Sans MS"
                ]
                [ text tet2 ]
            , Svg.image
                [ SvgAttr.xlinkHref url
                , SvgAttr.imageRendering "pixelated"
                , SvgAttr.x (fromFloat (w / 10) ++ "px")
                , SvgAttr.y (fromFloat (2 * h / 3 - 80) ++ "px")
                , SvgAttr.width "200px"
                , SvgAttr.height "200px"
                , SvgAttr.opacity (fromFloat opacity)
                ]
                []
            ]
        , view_skip
        ]


view_play : Model -> Float -> Float -> Float -> Html Msg
view_play model w h r =
    div
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "left" "0"
        , HtmlAttr.style "top" "0"
        , HtmlAttr.style "margin" "auto"
        , HtmlAttr.style "background" "rgb(246,227,197)"
        ]
        [ view_background w h r (model.time * 0.2) model.levelframe.level
        , viewPaddle model w h r
        , view_points_power model w h r
        , view_Goal w h r (model.time * 0.2) model.levelframe.level
        , view_backpeople w h r
        , div
            [ HtmlAttr.style "width" "100%"
            , HtmlAttr.style "height" "100%"
            , HtmlAttr.style "position" "fixed"
            ]
            [ Svg.svg
                [ SvgAttr.width "100%"
                , SvgAttr.height "100%"
                ]
                (List.append (List.map (viewRec w h r model) model.playframe.allPersonList) [ viewBall w h r model model.playframe.ball ]
                    |> List.append (viewsubfriends w h r model.playframe.paddle)
                )
            ]
        , viewUI model
        ]


viewEnd : Float -> Float -> Float -> Model -> Int -> Html Msg
viewEnd w h r model stage =
    let
        len =
            fromFloat (256 * 2 * 600 / 1024)

        wid =
            fromFloat (256 * 2 * 900 / 1536)

        x =
            fromInt 200

        y =
            fromInt 150

        picture_url =
            case stage of
                1 ->
                    "assets/1.png"

                2 ->
                    "assets/2.png"

                3 ->
                    "assets/3.png"

                5 ->
                    "assets/5.png"

                _ ->
                    ""

        ( text_ending_1, text_ending_2, text_ending_3 ) =
            case stage of
                1 ->
                    ( "In our way towards the goal, no matter the goal of in the court or the goal in the real life,", "we may frequently be defended tightly, knocked down heavily, or even fouled flagrantly .", "" )

                2 ->
                    ( "The sense of pain is so heartfelt, while the goal seems so far away that we want to quit from time to time,", "questioning our aim, which we have taken for granted since we started.", "But, a voice deep from our heart becomes louder and louder: 'You can lose, but pleaze don't give up!'" )

                3 ->
                    ( "And,", " we are fortunate to have our deariest friends to give us a hand when we are down.", "" )

                5 ->
                    ( "Then, please be patient to wait for the crowd to make the great noise as they do after every brilliant finish. This, is why we play.  ", "", "" )

                _ ->
                    ( "", "", "" )
    in
    viewEnd_text ( text_ending_1, text_ending_2, text_ending_3 ) model picture_url len wid x y


viewEnd_text : ( String, String, String ) -> Model -> String -> String -> String -> String -> String -> Html Msg
viewEnd_text ( text_ending_1, text_ending_2, text_ending_3 ) model picture_url len wid x y =
    div
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "fixed"
        , HtmlAttr.style "left" "0"
        , HtmlAttr.style "top" "0"
        , HtmlAttr.style "font-size" "20px"
        , HtmlAttr.style "font-family" "Comic Sans MS"
        , HtmlAttr.style "color" "rgb(119,67,96)"
        , HtmlAttr.style "background" "rgb(246,227,197)"
        ]
        [ Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "100%"
            ]
            [ Svg.image
                [ SvgAttr.xlinkHref picture_url
                , SvgAttr.x (x ++ "px")
                , SvgAttr.y (y ++ "px")
                , SvgAttr.width (wid ++ "px")
                , SvgAttr.height (len ++ "px")
                , SvgAttr.scale "10"
                , SvgAttr.imageRendering "pixelated"
                ]
                [ view_finish_level model ]
            ]
        , div
            [ HtmlAttr.style "width" "100%"
            , HtmlAttr.style "height" "100%"
            , HtmlAttr.style "position" "fixed"
            , HtmlAttr.style "left" (fromFloat 200.0 ++ "px")
            , HtmlAttr.style "top" (fromFloat 500 ++ "px")
            ]
            [ text text_ending_1
            ]
        , div
            [ HtmlAttr.style "width" "100%"
            , HtmlAttr.style "height" "100%"
            , HtmlAttr.style "position" "fixed"
            , HtmlAttr.style "left" (fromFloat 200.0 ++ "px")
            , HtmlAttr.style "top" (fromFloat 600 ++ "px")
            ]
            [ text text_ending_2
            ]
        , div
            [ HtmlAttr.style "width" "100%"
            , HtmlAttr.style "height" "100%"
            , HtmlAttr.style "position" "fixed"
            , HtmlAttr.style "left" (fromFloat 200.0 ++ "px")
            , HtmlAttr.style "top" (fromFloat 700 ++ "px")
            ]
            [ text text_ending_3
            ]
        , viewEnd_button
        ]


view_finish_level : Model -> Svg Msg
view_finish_level model =
    let
        len =
            fromFloat (256 * 4 * 600 / 1024)

        wid =
            fromFloat (256 * 4 * 900 / 1536)

        x =
            fromInt 550

        y =
            fromInt 300

        picture_url =
            "assets/finish_level.png "
    in
    div
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "fixed"
        , HtmlAttr.style "left" "0"
        , HtmlAttr.style "top" "0"
        , HtmlAttr.style "background" "rgb(246,227,197)"
        ]
        [ Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "100%"
            ]
            [ Svg.image
                [ SvgAttr.xlinkHref picture_url
                , SvgAttr.x (x ++ "px")
                , SvgAttr.y (y ++ "px")
                , SvgAttr.width (wid ++ "px")
                , SvgAttr.height (len ++ "px")
                , SvgAttr.scale "10"
                , SvgAttr.imageRendering "pixelated"
                ]
                []
            ]
        ]
