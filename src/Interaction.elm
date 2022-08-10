module Interaction exposing (..)

import Blocks exposing (RecSize)
import General exposing (ConstructStatus(..), Dir(..), Identity(..), LegendStatus(..), LiveStatus(..), Point, Vec, getxval, vecNormalize)
import Model exposing (Model)
import Objects exposing (..)


mapsize : ( Float, Float )
mapsize =
    ( 1024, 1536 )


upLimit : Float
upLimit =
    1 / 4 * Tuple.second mapsize


legendGuard : Float
legendGuard =
    300


crashTimeGuard : Float
crashTimeGuard =
    100


derativeTime : Float
derativeTime =
    20


staticPaddle : Paddle -> Paddle
staticPaddle paddle =
    { paddle | status = Static }


directedPaddle : Paddle -> Dir -> Paddle
directedPaddle paddle dir =
    case paddle.status of
        Moving fdir acc ->
            if fdir == dir then
                let
                    nacc =
                        min (acc + 1) 4
                in
                { paddle | status = Moving dir nacc }

            else
                { paddle | status = Moving dir 1 }

        Static ->
            { paddle | status = Moving dir 1 }


generatePaddle : Paddle -> Float -> Paddle
generatePaddle paddle time =
    let
        prePaddleLocate =
            paddle.anchor

        state =
            paddle.status

        halflen =
            paddle.size.halflength

        halfwid =
            paddle.size.halfwidth

        velocity =
            paddle.velocity

        distance =
            velocity * time

        nlocate =
            tryPaddleForward prePaddleLocate distance state
    in
    if nlocate.x - halflen < Tuple.first mapsize * 1 / 4 then
        { paddle | status = Static }
            |> giveNewAnchor (Tuple.first mapsize * 1 / 4 + halflen) nlocate.y

    else if nlocate.x + halflen > Tuple.first mapsize * 3 / 4 then
        { paddle | status = Static }
            |> giveNewAnchor (Tuple.first mapsize * 3 / 4 - halflen) nlocate.y

    else if nlocate.y - halfwid < Tuple.second mapsize - upLimit then
        { paddle | status = Static }
            |> giveNewAnchor nlocate.x (Tuple.second mapsize - upLimit + halfwid)

    else if nlocate.y + halfwid > Tuple.second mapsize then
        { paddle | status = Static }
            |> giveNewAnchor nlocate.x (Tuple.second mapsize - halfwid)

    else
        paddle
            |> giveNewAnchor nlocate.x nlocate.y


tryPaddleForward : Point -> Float -> PaddleStatus -> Point
tryPaddleForward point distance state =
    let
        fx =
            point.x

        fy =
            point.y
    in
    case state of
        --  Moving Up _ ->
        --      Point fx (fy - distance)
        --  Moving Down _ ->
        --      Point fx (fy + distance)
        Moving Left _ ->
            Point (fx - distance) fy

        Moving Right _ ->
            Point (fx + distance) fy

        Static ->
            Point fx fy


giveNewAnchor : Float -> Float -> { a | anchor : Point } -> { a | anchor : Point }
giveNewAnchor nx ny obj =
    { obj | anchor = Point nx ny }


setzeroTime : Model -> Model
setzeroTime model =
    { model | time = 0 }


updateBall : Float -> Ball -> Ball
updateBall time ball =
    let
        vec =
            ball.dir

        velocity =
            ball.speed

        legendstatus =
            ball.legendstatus

        dx =
            velocity * vec.x * time

        dy =
            velocity * vec.y * time

        nx =
            ball.anchor.x + dx

        ny =
            ball.anchor.y + dy
    in
    checkValidBallMove { ball | anchor = Point nx ny }


checkValidBallMove : Ball -> Ball
checkValidBallMove ball =
    let
        radius =
            ball.radius

        vec =
            ball.dir

        point =
            ball.anchor

        leftex =
            point.x - (radius + Tuple.first mapsize / 4)

        rightex =
            Tuple.first mapsize * 3 / 4 - (point.x + radius)

        upex =
            point.y - radius

        downex =
            Tuple.second mapsize - (point.y + radius)
    in
    if leftex <= 0 then
        if vec.x <= 0 then
            { ball | dir = Vec (0 - vec.x) vec.y }
                |> checkUpDownValidMove upex downex

        else
            checkUpDownValidMove upex downex ball

    else if rightex <= 0 then
        if vec.x >= 0 then
            { ball | dir = Vec (0 - vec.x) vec.y }
                |> checkUpDownValidMove upex downex

        else
            checkUpDownValidMove upex downex ball

    else
        ball
            |> checkUpDownValidMove upex downex


checkUpDownValidMove : Float -> Float -> Ball -> Ball
checkUpDownValidMove upex downex ball =
    if upex <= 0 then
        if ball.dir.y <= 0 then
            { ball | dir = Vec ball.dir.x (0 - ball.dir.y) }

        else
            ball

    else if downex <= 0 then
        { ball | live = Dead }

    else
        ball


checkPaddleBallCrash : Ball -> Paddle -> ( Ball, Paddle )
checkPaddleBallCrash ball paddle =
    case checkCrash ball paddle of
        Crash axis ->
            let
                newVec =
                    updateBallByPaddle ball.dir paddle.status axis
                        |> vecNormalize
            in
            ( { ball | dir = newVec }, { paddle | status = Static } )

        None ->
            ( ball, paddle )


updateBallByPaddle : Vec -> PaddleStatus -> Axis -> Vec
updateBallByPaddle oldVec pstatus axis =
    case axis of
        XAxis ->
            Vec oldVec.x (0 - abs oldVec.y)
                |> changeByAcc pstatus

        YAxis ->
            Vec (0 - oldVec.x) oldVec.y
                |> changeByAcc pstatus


changeByAcc : PaddleStatus -> Vec -> Vec
changeByAcc pstatus oldVec =
    case pstatus of
        Static ->
            oldVec

        Moving dir time ->
            case dir of
                Left ->
                    Vec (oldVec.x - 0.2 * toFloat time) oldVec.y

                Right ->
                    Vec (oldVec.x + 0.2 * toFloat time) oldVec.y


updateBricksByBall : Ball -> List Person -> Paddle -> ( Ball, List Person, Paddle )
updateBricksByBall ball personlist paddle =
    let
        ( ret, remain, crashDir ) =
            retCrash ball personlist
    in
    case ret of
        Nothing ->
            ( { ball | crashDir = None }, remain, paddle )

        Just aPerson ->
            interactBallPerson { ball | crashDir = crashDir } aPerson remain paddle


interactBallPerson : Ball -> Person -> List Person -> Paddle -> ( Ball, List Person, Paddle )
interactBallPerson ball aPerson remain paddle =
    if checkValidCrash ball aPerson == True then
        let
            identity =
                aPerson.identity

            ( nPerson, nball, npaddle ) =
                reduceHP aPerson ball paddle
        in
        case identity of
            Friend ->
                ( { nball | dir = reverseVec nball.dir nball.crashDir, score = ball.score + 20 }, List.append nPerson remain, npaddle )

            Enemy ->
                ( { nball | dir = Vec (0 - nball.dir.x) (0 - nball.dir.y) }, List.append nPerson remain, npaddle )

            Passerby ->
                ( { nball | dir = reverseVec nball.dir nball.crashDir }, List.append nPerson remain, npaddle )

            _ ->
                ( nball, List.append [ aPerson ] remain, paddle )

    else
        ( ball, List.append [ aPerson ] remain, paddle )


checkValidCrash : Ball -> Person -> Bool
checkValidCrash ball person =
    let
        crashdir =
            ball.crashDir
    in
    case crashdir of
        Crash XAxis ->
            if person.anchor.y - ball.anchor.y < 0 then
                if ball.dir.y > 0 then
                    False

                else
                    True

            else
                True

        _ ->
            True


reduceHP : Person -> Ball -> Paddle -> ( List Person, Ball, Paddle )
reduceHP person ball paddle =
    let
        status =
            person.construct

        identity =
            person.identity

        ballstatus =
            ball.legendstatus
    in
    case identity of
        Enemy ->
            case ballstatus of
                Common ->
                    ( [ person ], ball, changePaddlebyPerson identity paddle )

                Legendary ->
                    ( [], giveMark ball identity, paddle )

        Friend ->
            case status of
                Undestroyable ->
                    ( [ person ], giveMark ball identity, changePaddlebyPerson identity paddle )

                Destroyable time ->
                    if time == 1 then
                        ( [], giveMark ball identity, changePaddlebyPerson identity paddle )

                    else
                        ( [ person ], giveMark ball identity, changePaddlebyPerson identity paddle )

        Passerby ->
            case status of
                Undestroyable ->
                    ( [ person ], giveMark ball identity, changePaddlebyPerson identity paddle )

                Destroyable time ->
                    if time == 1 then
                        ( [], giveMark ball identity, changePaddlebyPerson identity paddle )

                    else
                        ( [ person ], giveMark ball identity, changePaddlebyPerson identity paddle )

        _ ->
            ( [], ball, paddle )


reverseVec : Vec -> CrashDir -> Vec
reverseVec dir crashdir =
    case crashdir of
        Crash XAxis ->
            Vec dir.x (0 - dir.y)

        Crash YAxis ->
            Vec (0 - dir.x) dir.y

        None ->
            dir


changePaddlebyPerson : Identity -> Paddle -> Paddle
changePaddlebyPerson id paddle =
    let
        para =
            changePaddlebyPersonSignal id paddle.length paddle.exist

        halflen =
            paddle.size.halflength + (32 * para)

        halfwid =
            paddle.size.halfwidth

        xlocate =
            paddle.anchor.x + 32 * para

        length =
            paddle.length
    in
    { paddle | length = paddle.length + para, anchor = Point xlocate paddle.anchor.y, size = RecSize halflen halfwid }
        |> checkPaddleZeroLength


giveMark : Ball -> Identity -> Ball
giveMark ball identity =
    case identity of
        Enemy ->
            { ball | score = ball.score + 40, legendStore = min (ball.legendStore + 40) 256 }

        Friend ->
            { ball | score = ball.score + 20, legendStore = min (ball.legendStore + 20) 256 }

        Passerby ->
            { ball | score = ball.score + 10, legendStore = min (ball.legendStore + 10) 256 }

        Me ->
            ball


changePaddlebyPersonSignal : Identity -> Float -> Existence -> Float
changePaddlebyPersonSignal id length exist =
    case id of
        Friend ->
            if length < 4 then
                if exist == Exist then
                    1.0

                else
                    0

            else
                0

        Enemy ->
            -1.0

        _ ->
            0


checkPaddleZeroLength : Paddle -> Paddle
checkPaddleZeroLength paddle =
    if paddle.length <= 0 then
        { paddle | exist = NotExist }

    else
        paddle


setzeroScore : Model -> Model
setzeroScore model =
    let
        ball =
            model.playframe.ball

        playframe =
            model.playframe
    in
    { model | playframe = { playframe | ball = { ball | score = 0 } } }


judge_at_place : Model -> Model
judge_at_place model =
    let
        stage =
            model.playframe_0.stage

        paddle_x =
            model.playframe_0.paddle.anchor.x

        ball_x =
            model.playframe_0.ball.anchor.x

        ball_y =
            model.playframe_0.ball.anchor.y

        page =
            model.page
    in
    case stage of
        1 ->
            if paddle_x == 736 then
                model |> resume_introduction

            else
                model

        2 ->
            if paddle_x <= 544 then
                model |> resume_introduction

            else
                model

        3 ->
            if paddle_x <= 352 then
                model
                    |> resume_introduction

            else
                model

        4 ->
            if ball_y >= 212 then
                model
                    |> resume_introduction

            else
                model

        5 ->
            if ball_y >= 480 then
                model
                    |> resume_introduction

            else
                model

        6 ->
            if ball_y >= 760 then
                model
                    |> resume_introduction

            else
                model

        7 ->
            if ball_y <= 645 then
                model
                    |> resume_introduction

            else
                model

        8 ->
            if ball_y <= 485 then
                model
                    |> resume_introduction

            else
                model

        9 ->
            { model | state = Pass, page = { page | state = Turn 0 } } |> setzeroTime |> setzeroScore

        _ ->
            model


resume_introduction : Model -> Model
resume_introduction model =
    let
        current_stage =
            model.playframe_0.stage + 1

        current_frame =
            model.playframe_0
    in
    if current_stage <= 3 then
        { model | state = Init, playframe_0 = switch_playframe_0 current_stage current_frame }

    else if current_stage == 4 then
        { model | state = Death, playframe_0 = switch_playframe_0 current_stage current_frame, time = 10000 }

    else if current_stage >= 5 then
        { model | state = Death, playframe_0 = switch_playframe_0 current_stage current_frame, time = 10000 }

    else
        { model | playframe_0 = switch_playframe_0 current_stage current_frame }


switch_playframe_0 : Int -> Playframe_0 -> Playframe_0
switch_playframe_0 number frame =
    let
        paddle =
            frame.paddle
    in
    case number of
        2 ->
            { frame | allPersonList = [ Person (Point 544 1472) (RecSize 32 64) Passerby (Destroyable 1) ], stage = 2, power = 10 }

        -- Playframe_0 [ (Ball (Point 760 8) 8 0.5 (Vec 0.6 0.8) Common Dead 0) (Paddle (Point 288 1488) (RecSize 32 48) Static 0.5 Me) initdoor 0
        3 ->
            { frame | allPersonList = [ Person (Point 352 1472) (RecSize 32 64) Enemy Undestroyable ], stage = 3, power = 15 }

        --Playframe_0 [Person (Point 352 1376) (RecSize 32 64) Enemy Undestroyable] (Ball (Point 760 8) 8 0.5 (Vec 0.6 0.8) Common Dead 0) (Paddle (Point 288 1488) (RecSize 32 48) Static 0.5 Me) initdoor 0
        4 ->
            --{ frame | stage = 4 }
            { frame | allPersonList = [ Person (Point 608 220) (RecSize 32 64) Friend (Destroyable 1) ], stage = 4, power = 10 }

        --Playframe_0 [Person (Point 480 576 ) (RecSize 32 64) Friend (Destroyable 1)] (Ball (Point 760 8) 8 0.5 (Vec 0.6 0.8) Common Dead 0) (Paddle (Point 288 1488) (RecSize 32 48) Static 0.5 Me) initdoor 0
        5 ->
            { frame | allPersonList = [ Person (Point 608 488) (RecSize 32 64) Passerby (Destroyable 1) ], stage = 5, power = 20 }

        --Playframe_0 [Person (Point 736 192 ) (RecSize 32 64) Passerby (Destroyable 1)] (Ball (Point 760 8) 8 0.5 (Vec 0.6 0.8) Common Dead 0) (Paddle (Point 288 1488) (RecSize 32 48) Static 0.5 Me) initdoor 0
        6 ->
            { frame | allPersonList = [ Person (Point 608 806) (RecSize 32 64) Enemy Undestroyable ], stage = 6, power = 15 }

        --Playframe_0 [Person (Point 288 100 ) (RecSize 32 64) Friend (Destroyable 1)] (Ball (Point 760 8) 8 0.5 (Vec 0.6 0.8) Common Dead 0) (Paddle (Point 288 1488) (RecSize 32 48) Static 0.5 Me) initdoor 0
        7 ->
            { frame | allPersonList = [ Person (Point 608 408) (RecSize 32 64) Enemy (Destroyable 1), Person (Point 608 806) (RecSize 32 64) Enemy Undestroyable ], stage = 7, power = 20 }

        8 ->
            { frame | allPersonList = [ Person (Point 608 408) (RecSize 32 64) Enemy (Destroyable 1), Person (Point 608 806) (RecSize 32 64) Enemy Undestroyable ], stage = 8, power = 20 }

        9 ->
            { frame | allPersonList = [ Person (Point 608 806) (RecSize 32 64) Enemy Undestroyable ], stage = 9, power = 0 }

        _ ->
            Playframe_0 [ Person (Point 736 192) (RecSize 32 64) Friend (Destroyable 1) ]
                (Ball (Point 760 8) 8 0.5 (Vec 0.6 0.8) Common Dead 0 0 None)
                (Paddle (Point 288 1488) (RecSize 32 48) Static 0.5 Me 1 Exist)
                (Door (Point 512 -7250) (RecSize 512 32))
                0
                0
