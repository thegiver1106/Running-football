module Main exposing (main)

import Browser
import Browser.Dom exposing (getViewport)
import Model exposing (Model, initModel)
import Objects exposing (Model_State)
import Objects exposing (Model_State)
import Msg exposing (Msg)
import Subscriptions exposing (subscriptions)
import Task
import Update exposing (update)
import View exposing (view)
import Time 
import Msg exposing (Msg(..))


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel 0 |> (\a ->  {a | state = Objects.Pass }), Cmd.batch [Task.perform Msg.GetViewport getViewport,Task.perform NewTime Time.now])


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view}
