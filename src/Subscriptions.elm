module Subscriptions exposing (subscriptions)

import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp, onResize,onKeyPress)
import General exposing (Dir(..))
import Html.Attributes exposing (dir)
import Html.Events exposing (keyCode)
import Json.Decode as Decode
import Model exposing (Model,alllevel)
import Msg exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onAnimationFrameDelta Tick
        , onKeyUp (Decode.map keystop keyCode)
        , onKeyDown (Decode.map key keyCode)
        , onResize Resize
        , onKeyPress (Decode.map (keypage) keyCode)
        ]


key : Int -> Msg
key keycode =
    case keycode of
        --38 ->
        --    Way Up
        --
        --40 ->
        --    Way Down
        37 ->
            Way Left

        39 ->
            Way Right
        65 -> 
            StartLegend
        68 ->
            EndLegend
        78 ->
            Press_N

        _ ->
            Key_None


keystop : Int -> Msg
keystop keycode =
    case keycode of
        --38 ->
        --    Stop
        --
        --40 ->
        --    Stop
        37 ->
            Stop Left

        39 ->
            Stop Right

        _ ->
            Key_None

keypage :  Int -> Msg
keypage  keycode =
    case keycode of
        13 ->
            TurnPage
        _ ->
            Key_None
