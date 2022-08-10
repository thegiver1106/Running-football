module Msg exposing (Msg(..))

import Browser.Dom exposing (Viewport)
import General exposing (Dir(..), Identity(..), Point)
import Html.Events exposing (on)
import Svg exposing (stop)
import Time


type Msg
    = Key_None
    | Way Dir
    | Place_Person Identity
    | Tick Float
    | Generate_Person ( Identity, Point )
    | Stop Dir
    | Pause
    | Resume
    | Start_Restart
    | GetViewport Viewport
    | Resize Int Int
    | TurnPage
    | NewTime Time.Posix
    | StartLegend
    | EndLegend
    | Press_N
    | ChangeId Identity String
    | ChangeTime String
    | Skip
    | HiddenLevel
    | Back
    | Noop
