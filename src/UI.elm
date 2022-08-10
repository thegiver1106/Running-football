module UI exposing (viewUI)

import Browser
import Debug exposing (toString)
import Html exposing (Html, button, div, text)
import Html.Attributes as HtmlAttr exposing (..)
import Html.Events exposing (onClick)
import Model exposing (Model)
import Msg exposing (Msg(..))
import Objects exposing (..)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr


viewUI : Model -> Html Msg
viewUI model =
    div []
        [ view_Pause_Resume model
        , view_Start_reStart model

        --, view_gradual_change model
        ]


view_Pause_Resume : Model -> Html Msg
view_Pause_Resume model =
    let
        ( txt, msg ) =
            case model.state of
                Paused ->
                    ( "Resume", Resume )

                _ ->
                    ( "Pause", Pause )
    in
    div []
        [ button
            (List.append
                [ onClick msg
                , style "background" "rgba(236, 240, 241, 0.85)"
                , style "border" "0"
                , style "bottom" "33%"
                , style "color" "#34495f"
                , style "cursor" "pointer"
                , style "font-family" "Helvetica, Arial, sans-serif"
                , style "display" "block"
                , style "font-size" "18px"
                ]
                [ style "font-weight" "300"
                , style "height" "60px"
                , style "left" "27%"
                , style "line-height" "60px"
                , style "outline" "none"
                , style "position" "absolute"
                , style "width" "120px"
                , style "border-radius" "10%"
                , style "shadow" "5 5 10 rgb(50,0,0)"
                ]
            )
            [ text txt ]
        ]


view_Start_reStart : Model -> Html Msg
view_Start_reStart model =
    let
        txt =
            case model.state of
                Init ->
                    "Start"

                _ ->
                    "Restart"
    in
    div []
        [ button
            (List.append
                [ onClick Start_Restart
                , style "background" "rgba(236, 240, 241, 0.85)"
                , style "border" "0"
                , style "bottom" "20%"
                , style "color" "#34495f"
                , style "cursor" "pointer"
                , style "font-family" "Helvetica, Arial, sans-serif"
                , style "display" "block"
                , style "font-size" "18px"
                ]
                [ style "font-weight" "300"
                , style "height" "60px"
                , style "left" "27%"
                , style "line-height" "60px"
                , style "outline" "none"
                , style "position" "absolute"
                , style "width" "120px"
                , style "border-radius" "10%"
                ]
            )
            [ text txt ]
        ]
