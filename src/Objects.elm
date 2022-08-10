module Objects exposing (..)

import Blocks exposing (..)
import General exposing (..)
import List exposing (append)
import Random
import List exposing (length)


type Key_state
    = Key_up
    | Key_down


type Model_State
    = Init
    | Paused -- inLevel Pass 
    | Playing
    | Death
    | Pass 


type PaddleStatus
    = Static
    | Moving Dir Int

type Existence 
        = Exist
        | NotExist
type Axis = XAxis
            | YAxis
type CrashDir 
        = None 
        | Crash Axis

type alias Paddle =
    { anchor : Point
    , size : RecSize
    , status : PaddleStatus
    , velocity : Float
    , identity : Identity
    , length : Float
    , exist : Existence
    }


type alias Person =
    { anchor : Point
    , size : RecSize
    , identity : Identity
    , construct : ConstructStatus
    }


type alias Ball =
    { anchor : Point
    , radius : Float
    , speed : Float
    , dir : Vec
    , legendstatus : LegendStatus
    , live : LiveStatus
    , score : Float -- one level score  --restart set zero
    , legendStore : Float --set zero 
    , crashDir : CrashDir
    }

type alias Door =
    { anchor : Point
    , size : RecSize
    }



type alias Levelframe =
    { level : Int -- T
    }

type alias Playframe =
    { allPersonList : List Person
    , ball : Ball
    , paddle : Paddle
    , door : Door
    }

type alias Playframe_0 =
    --designed for the intro part
    { allPersonList : List Person
    , ball : Ball
    , paddle : Paddle
    , door : Door
    , stage : Int
    , power : Int
    }

type alias Page =
    { state : Pagetype
    }



type Pagetype 
    = Logo
    | Intro
    | Start Bool -- cover
    | Turn Int  -- 1 - 2 == turn 1
    | Play --  When playing 
    | Win Int
    | End Int  -- Lose @ level : End 1 == die in first level  (bad ending)
    | HE Bool 
    | Edit 
    | AllEnd -- win happy ending

    -- score -- true ending --> ?

--functions


getDetailLocation : { a | anchor : Point } -> ( Float, Float )
getDetailLocation figure =
    let
        anc =
            figure.anchor
    in
    ( anc.x, anc.y )


judgeIn : Float -> Float -> Float -> Float -> Bool
judgeIn ballX personX ballradius halflen =
    abs (ballX - personX) <= ballradius + halflen




checkCrash : Ball -> { a | anchor : Point, size : RecSize } -> CrashDir
checkCrash ball person =
    let
        ( ballX, ballY ) =
            getDetailLocation ball

        ( personX, personY ) =
            getDetailLocation person

        ( halflen, halfwid ) =
            retRecSize person

        ballRadius =
            retRadius ball
    in
        if judgeCrashIn ballX personX halflen then
            if judgeIn ballY personY ballRadius halfwid then
                Crash XAxis
            else
                None
        else if judgeCrashIn ballY personY halfwid then
            if judgeIn ballX personX ballRadius halflen then
                Crash YAxis
            else 
                None
        else 
            None

  
judgeCrashIn : Float -> Float -> Float -> Bool 
judgeCrashIn ball person len =
    abs (ball - person) <= len


retCrash : Ball -> List Person -> ( Maybe Person, List Person, CrashDir )
retCrash ball personList =
    let
        (xhitted, xothers ) =
            List.partition (\x -> (checkCrash ball x == (Crash XAxis))) personList
    in
    case xhitted of
        [] ->
           let 
                (yhitted, yothers) =
                    List.partition (\x -> (checkCrash ball x == (Crash YAxis))) personList
            in
                case yhitted of 
                    [] ->
                        (Nothing, personList, None)
                    aPerson:: rest ->
                        (Just aPerson, List.append rest yothers, Crash YAxis)

        aPerson :: rest ->
            ( Just aPerson, List.append rest xothers, Crash XAxis )


randomidentity : Random.Generator Identity
randomidentity =
    Random.weighted ( 60, Passerby ) [ ( 30, Friend ), ( 10, Enemy ) ]


randomlocate : Random.Generator Point
randomlocate =
    Random.map2 Point (Random.float 0 500) (Random.float 0 500)



--door
judgeXInDoor : Float -> Float -> Float -> Float -> Bool
judgeXInDoor ballX personX ballradius halflen =
    abs (ballX - personX) <= halflen- ballradius 


judgeYInDoor : Float -> Float -> Float -> Float -> Bool
judgeYInDoor ballY personY ballRadius halfwid =
    abs (ballY - personY) <=  halfwid - ballRadius


checkDoor : Ball -> { a | anchor : Point, size : RecSize } -> Bool
checkDoor ball door =
    let
        ( ballX, ballY ) =
            getDetailLocation ball

        ( doorX, doorY ) =
            getDetailLocation door

        ( halflen, halfwid ) =
            retRecSize door

        ballRadius =
            retRadius ball
    in
    if judgeXInDoor ballX doorX ballRadius halflen then
        judgeYInDoor ballY doorY ballRadius halfwid

    else
        False

checkPaddleCrash : Paddle -> { a | anchor : Point, size : RecSize } -> Bool
checkPaddleCrash paddle person =
    let
        ( ballX, ballY ) =
            getDetailLocation paddle

        ( personX, personY ) =
            getDetailLocation person

        ( halflen, halfwid ) =
            retRecSize person

        (phalflen,phalfwid) =
            retRecSize paddle
        
    in
    if judgeIn ballX personX phalflen halflen then
        judgeIn ballY personY phalfwid halfwid

    else
        False


