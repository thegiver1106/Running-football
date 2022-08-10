module Model exposing (CreativeZone, Model, alllevel, initModel)

import Blocks exposing (RecSize, Rectangle, getInitBlocks, getInitPaddle)
import General exposing (ConstructStatus(..), Identity(..), LegendStatus(..), LiveStatus(..), Point, Vec)
import Objects exposing (..)
import Random
import Task exposing (Task)
import Time exposing (now)


type alias Model =
    { levelframe : Levelframe --1: basic information
    , playframe : Playframe --2: detail in different level
    , playframe_0 : Playframe_0 -- for the introduction part
    , page : Page
    , state : Model_State --UI
    , time : Float --one level total time set zero
    , key_left : Key_state
    , key_right : Key_state
    , size : ( Float, Float )
    , current_flow : Float --delta t
    , seed : Random.Seed
    , durable : Float --control random set zero
    , score : Float -- all score add
    , creativeZone : CreativeZone --changeable x, propability + time value, no change!
    }


type alias CreativeZone =
    { friendProb : Float
    , passerbyProb : Float
    , enemyProb : Float
    , geneElapsed : Float
    }


alllevel : Int
alllevel =
    2


initModel : Int -> Model
initModel level =
    let
        modela =
            Model (initlevelframe level) (initplayframe level)

        modelb =
            modela initplayframe_0 initpage Init 0

        modelc =
            modelb Key_up Key_up ( 0, 0 ) 0
    in
    modelc (Random.initialSeed 1) 0 0 (initCreativeZone level)


initCreativeZone : Int -> CreativeZone
initCreativeZone level =
    let
        ( friendProb, passerbyProb, enemyProb ) =
            initPersonProb level

        timeGuard =
            inittimeGuard level
    in
    CreativeZone friendProb passerbyProb enemyProb timeGuard


initPersonProb : Int -> ( Float, Float, Float )
initPersonProb level =
    case level of
        1 ->
            ( 50, 40, 10 )

        2 ->
            ( 40, 40, 20 )

        3 ->
            ( 30, 50, 20 )

        _ ->
            ( 40, 40, 20 )


inittimeGuard : Int -> Float
inittimeGuard level =
    800 - 80 * toFloat level


initlevelframe : Int -> Levelframe
initlevelframe level =
    Levelframe level


initplayframe : Int -> Playframe
initplayframe level =
    Playframe (initallPerson level) initBall initPaddle initdoor


initplayframe_0 : Playframe_0
initplayframe_0 =
    Playframe_0 [ Person (Point 736 1472) (RecSize 32 64) Friend (Destroyable 1) ]
        (Ball (Point 760 8) 8 0.5 (Vec 0.6 0.8) Common Dead 0 0 None)
        (Paddle (Point 288 1488)
            (RecSize 32 48)
            Static
            0.5
            Me
            1
            Exist
        )
        initdoor
        1
        0


initdoor : Door
initdoor =
    Door (Point 512 96) (RecSize 512 64)


initpage : Page
initpage =
    Page Logo


initallPerson : Int -> List Person
initallPerson level =
    case level of
        1 ->
            [ Person (Point 736 192) (RecSize 32 64) Friend (Destroyable 1), Person (Point 480 576) (RecSize 32 64) Passerby (Destroyable 1) ]

        2 ->
            [ Person (Point 736 192) (RecSize 32 64) Friend (Destroyable 1), Person (Point 480 576) (RecSize 32 64) Passerby (Destroyable 1) ]

        3 ->
            [ Person (Point 736 192) (RecSize 32 64) Friend (Destroyable 1), Person (Point 480 576) (RecSize 32 64) Passerby (Destroyable 1) ]

        4 ->
            [ Person (Point 736 192) (RecSize 32 64) Friend (Destroyable 1), Person (Point 480 576) (RecSize 32 64) Passerby (Destroyable 1) ]

        5 ->
            [ Person (Point 736 192) (RecSize 32 64) Friend (Destroyable 1), Person (Point 480 576) (RecSize 32 64) Passerby (Destroyable 1) ]

        _ ->
            []


initBall : Ball
initBall =
    Ball (Point 760 8) 8 0.5 (Vec 0.6 0.8) Common Alive 0 0 None


initPaddle : Paddle
initPaddle =
    Paddle (Point 288 1488) (RecSize 32 48) Static 0.5 Me 1 Exist
