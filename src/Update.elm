module Update exposing (..)

import Blocks exposing (RecSize)
import Browser.Dom exposing (getViewport)
import General exposing (ConstructStatus(..), Dir(..), Identity(..), LegendStatus(..), LiveStatus(..), Point, Vec, getxval, vecNormalize)
import Html exposing (time)
import Html.Attributes exposing (download)
import Interaction exposing (..)
import Introduction exposing (updateintro)
import Json.Decode exposing (Value)
import Level exposing (getLevelPerson)
import Model exposing (CreativeZone, Model, alllevel, initModel)
import Msg exposing (Msg(..))
import Objects exposing (..)
import Process exposing (Id)
import Random
import Svg.Attributes exposing (fy)
import Task
import Time
import View



--global static parameters, but editable in programming


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    updateall msg model
        |> updateView msg


updateView : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updateView msg ( model, cmd ) =
    case msg of
        Resize width height ->
            --resize
            ( { model | size = ( toFloat width, toFloat height ) }
            , cmd
            )

        GetViewport { viewport } ->
            --get size
            ( { model
                | size =
                    ( viewport.width
                    , viewport.height
                    )
              }
            , Cmd.batch [ cmd, Task.perform GetViewport getViewport ]
            )

        Back ->
            backtostart msg model

        _ ->
            ( model, cmd )


updateall : Msg -> Model -> ( Model, Cmd Msg )
updateall msg model =
    case model.page.state of
        Intro ->
            updateintro model msg

        _ ->
            (case model.state of
                Init ->
                    updateInit msg model

                Playing ->
                    updatePlaying msg model

                Paused ->
                    case msg of
                        Resume ->
                            if model.page.state == Edit then
                                updategamestate msg model

                            else
                                ( { model | state = Playing }, Cmd.none )

                        Start_Restart ->
                            ( initModel model.levelframe.level |> (\a -> { a | state = Playing, score = model.score }), Task.perform NewTime Time.now )

                        ChangeId id value ->
                            ( { model | creativeZone = changeModelProbByInput model id value }, Cmd.none )

                        ChangeTime value ->
                            ( { model | creativeZone = changeModelGeneByInput model value }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                Death ->
                    case msg of
                        Start_Restart ->
                            ( initModel model.levelframe.level |> (\a -> { a | state = Playing, score = model.score }), Task.perform NewTime Time.now )

                        _ ->
                            updategamestate msg model

                Pass ->
                    --updateTime model msg
                    updategamestate msg model
            )
                |> checkDeath
                |> updatePass


updateInit : Msg -> Model -> ( Model, Cmd Msg )
updateInit msg model =
    let
        playframe =
            model.playframe

        ball =
            playframe.ball
    in
    case msg of
        Start_Restart ->
            ( { model | state = Playing, playframe = { playframe | ball = { ball | score = 0 } } }
            , Cmd.none
            )

        NewTime time ->
            ( { model | seed = Random.initialSeed (Time.posixToMillis time) }, Cmd.none )

        _ ->
            ( model, Cmd.none )


updatePlaying : Msg -> Model -> ( Model, Cmd Msg )
updatePlaying msg model =
    let
        playframe =
            model.playframe

        ball =
            playframe.ball

        level =
            model.levelframe.level

        page =
            model.page
    in
    case msg of
        Way dir ->
            updateWay dir model

        NewTime time ->
            ( { model | seed = Random.initialSeed (Time.posixToMillis time) }, Cmd.none )

        Stop dir ->
            updateStop dir model

        StartLegend ->
            ( { model | playframe = { playframe | ball = { ball | legendstatus = Legendary } } }, Cmd.none )

        EndLegend ->
            ( { model | playframe = { playframe | ball = { ball | legendstatus = Common } } }, Cmd.none )

        Tick elapsed ->
            updateTick model elapsed

        Pause ->
            if model.levelframe.level == 5 then
                updategamestate msg { model | state = Paused }

            else
                ( { model | state = Paused }, Cmd.none )

        Start_Restart ->
            ( initModel model.levelframe.level |> (\a -> { a | state = Playing, score = model.score }), Task.perform NewTime Time.now )

        ChangeId id value ->
            ( { model | creativeZone = changeModelProbByInput model id value }, Cmd.none )

        ChangeTime value ->
            ( { model | creativeZone = changeModelGeneByInput model value }, Cmd.none )

        Press_N ->
            ( { model | page = { page | state = Turn level }, state = Pass }, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateWay : Dir -> Model -> ( Model, Cmd Msg )
updateWay dir model =
    let
        playframe =
            model.playframe
    in
    case dir of
        Right ->
            ( { model | playframe = { playframe | paddle = directedPaddle playframe.paddle dir }, key_right = Key_down }, Cmd.none )

        Left ->
            ( { model | playframe = { playframe | paddle = directedPaddle playframe.paddle dir }, key_left = Key_down }, Cmd.none )


updateStop : Dir -> Model -> ( Model, Cmd Msg )
updateStop dir model =
    let
        playframe =
            model.playframe
    in
    case dir of
        Right ->
            case model.key_left of
                Key_up ->
                    ( { model | playframe = { playframe | paddle = staticPaddle playframe.paddle }, key_right = Key_up }, Cmd.none )

                Key_down ->
                    ( { model | key_right = Key_up }, Cmd.none )

        Left ->
            case model.key_right of
                Key_up ->
                    ( { model | playframe = { playframe | paddle = staticPaddle playframe.paddle }, key_left = Key_up }, Cmd.none )

                Key_down ->
                    ( { model | key_left = Key_up }, Cmd.none )


updateTick : Model -> Float -> ( Model, Cmd Msg )
updateTick model elapsed =
    let
        page =
            model.page

        playframe =
            model.playframe

        checkedLegendBall =
            checkLegendBall model.playframe.ball elapsed

        movedball =
            updateBall elapsed checkedLegendBall

        npaddle =
            generatePaddle model.playframe.paddle elapsed

        ( paddledBall, newPaddle ) =
            checkPaddleBallCrash movedball npaddle

        ( personedpaddle, persons ) =
            changePaddlebyCrush newPaddle model.playframe.allPersonList

        ( finalBall, finalList, finalPaddle ) =
            updateBricksByBall paddledBall persons personedpaddle
    in
    ( { model
        | playframe =
            { playframe | paddle = finalPaddle, ball = finalBall, allPersonList = finalList }
        , time = model.time + elapsed
        , page = { page | state = Play }
        , current_flow = elapsed
        , durable = model.durable + elapsed
      }
        |> getLevelPerson
        |> update_allPersonlist_byflow
    , Cmd.none
    )


updategamestate : Msg -> Model -> ( Model, Cmd Msg )
updategamestate msg model =
    let
        page =
            model.page
    in
    if model.state == Paused then
        changePausedModel model msg

    else
        case model.page.state of
            Logo ->
                updateStateLogo msg model

            Start flag ->
                updateStateStart msg model flag

            Intro ->
                ( { model | state = Init }, Cmd.none )

            Turn int ->
                updateStateTurn msg model int

            Play ->
                --none
                ( model, Cmd.none )

            Win int ->
                (if model.time < 1500 then
                    --time for logo
                    model

                 else
                    { model | page = { page | state = Turn int } } |> setzeroTime
                )
                    |> updateTime msg

            End int ->
                case int of
                    4 ->
                        ( { model | page = { page | state = Turn 4 }, state = Pass }, Cmd.none )

                    _ ->
                        case msg of
                            Start_Restart ->
                                ( initModel model.levelframe.level |> (\a -> { a | state = Playing }), Task.perform NewTime Time.now )

                            _ ->
                                ( model, Cmd.none )

            HE flag ->
                if flag == False then
                    if model.score >= 300 then
                        case msg of
                            HiddenLevel ->
                                ( { model | page = { page | state = HE True } }, Cmd.none )

                            Back ->
                                ( { model | page = { page | state = Start False }, score = 0 }, Cmd.none )

                            _ ->
                                ( model, Cmd.none )

                    else
                        case msg of
                            Back ->
                                ( { model | page = { page | state = Start False }, score = 0 }, Cmd.none )

                            _ ->
                                ( model, Cmd.none )

                else
                    ( updateTurn msg model, Cmd.none )

            --button
            --|> updateHE
            Edit ->
                case msg of
                    Resume ->
                        ( { model | page = { page | state = Play }, state = Playing }, Cmd.none )

                    _ ->
                        ( model, Cmd.none )

            AllEnd ->
                ( model, Cmd.none )


updateStateTurn : Msg -> Model -> Int -> ( Model, Cmd Msg )
updateStateTurn msg model int =
    let
        page =
            model.page
    in
    if int == 3 then
        ( { model | state = Pass, page = { page | state = HE False } }, Cmd.none )

    else if int == 5 then
        ( model, Cmd.none )

    else
        ( updateTurn msg model, Cmd.none )


updateStateStart : Msg -> Model -> Bool -> ( Model, Cmd Msg )
updateStateStart msg model flag =
    let
        page =
            model.page
    in
    if flag == False then
        case msg of
            TurnPage ->
                ( { model | page = { page | state = Start True } } |> setzeroTime, Cmd.none )

            _ ->
                ( model, Cmd.none )

    else
        (if model.time < 1500 then
            --time for logo
            model

         else
            { model | page = { page | state = Intro } } |> setzeroTime
        )
            |> updateTime msg


updateStateLogo : Msg -> Model -> ( Model, Cmd Msg )
updateStateLogo msg model =
    let
        page =
            model.page
    in
    if model.time < 45000 then
        --time for logo
        if model.time > 3000 then
            case msg of
                Skip ->
                    ( { model | page = { page | state = Start False } } |> setzeroTime, Cmd.none )

                _ ->
                    model |> updateTime msg

        else
            model |> updateTime msg

    else
        ( { model | page = { page | state = Start False } } |> setzeroTime, Cmd.none )


backtostart : Msg -> Model -> ( Model, Cmd Msg )
backtostart msg model =
    let
        page =
            model.page

        playframe =
            model.playframe

        ball =
            playframe.ball
    in
    case msg of
        Back ->
            ( initModel 1
                |> (\a ->
                        { a
                            | state = Init
                            , page = { page | state = Play }
                            , levelframe = Levelframe 1
                            , score = model.score + playframe.ball.score
                        }
                   )
                |> setzeroTime
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


changePausedModel : Model -> Msg -> ( Model, Cmd Msg )
changePausedModel model msg =
    let
        page =
            model.page
    in
    if model.page.state == Play then
        ( { model | page = { page | state = Edit } }, Cmd.none )

    else if model.page.state == Edit then
        case msg of
            Resume ->
                ( { model | page = { page | state = Play }, state = Playing }, Cmd.none )

            _ ->
                ( model, Cmd.none )

    else
        ( model, Cmd.none )


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


updateTurn : Msg -> Model -> Model
updateTurn msg model =
    let
        page =
            model.page

        levelframe =
            model.levelframe

        playframe =
            model.playframe

        nscore =
            playframe.ball.score + model.score

        ball =
            playframe.ball
    in
    if levelframe.level == 3 then
        if nscore >= 300 then
            case msg of
                Press_N ->
                    initModel (levelframe.level + 1)
                        |> (\a ->
                                { a
                                    | state = Init
                                    , page = { page | state = Play }
                                    , levelframe = { levelframe | level = levelframe.level + 1 }
                                    , score = nscore
                                    , playframe = { playframe | ball = { ball | score = 0 } }
                                }
                           )
                        |> setzeroTime

                HiddenLevel ->
                    initModel (levelframe.level + 1)
                        |> (\a ->
                                { a
                                    | state = Init
                                    , page = { page | state = Play }
                                    , levelframe = { levelframe | level = levelframe.level + 1 }
                                    , score = nscore
                                    , playframe = { playframe | ball = { ball | score = 0 } }
                                }
                           )
                        |> setzeroTime

                _ ->
                    { model | score = nscore, playframe = { playframe | ball = { ball | score = 0 } } }

        else
            { model | score = nscore, playframe = { playframe | ball = { ball | score = 0 } } }

    else
        case levelframe.level of
            0 ->
                case msg of
                    Press_N ->
                        initModel 1
                            |> (\a -> { a | state = Init, page = { page | state = Play }, levelframe = { levelframe | level = 1 } })

                    _ ->
                        model

            _ ->
                case msg of
                    Press_N ->
                        initModel (levelframe.level + 1)
                            |> (\a ->
                                    { a
                                        | state = Init
                                        , page = { page | state = Play }
                                        , levelframe = { levelframe | level = levelframe.level + 1 }
                                        , score = model.score + playframe.ball.score
                                    }
                               )
                            |> setzeroTime

                    _ ->
                        model


updateTime : Msg -> Model -> ( Model, Cmd Msg )
updateTime msg model =
    case msg of
        Tick elapsed ->
            ( { model | time = model.time + elapsed }, Cmd.none )

        _ ->
            ( model, Cmd.none )


changeModelProbByInput : Model -> Identity -> String -> CreativeZone
changeModelProbByInput model identity value =
    let
        oriZone =
            model.creativeZone

        oriFriend =
            oriZone.friendProb

        oriPasserBy =
            oriZone.passerbyProb

        oriEnemy =
            oriZone.enemyProb
    in
    case identity of
        Friend ->
            { oriZone | friendProb = Maybe.withDefault oriFriend (String.toFloat value) }

        Passerby ->
            { oriZone | passerbyProb = Maybe.withDefault oriPasserBy (String.toFloat value) }

        Enemy ->
            { oriZone | enemyProb = Maybe.withDefault oriEnemy (String.toFloat value) }

        Me ->
            oriZone


changeModelGeneByInput : Model -> String -> CreativeZone
changeModelGeneByInput model value =
    let
        oriZone =
            model.creativeZone

        oriGuard =
            oriZone.geneElapsed

        potential =
            Maybe.withDefault oriGuard (String.toFloat value)
    in
    if potential <= 0 then
        oriZone

    else
        { oriZone | geneElapsed = potential }


checkLegendBall : Ball -> Float -> Ball
checkLegendBall ball elapsed =
    case ball.legendstatus of
        Common ->
            ball

        Legendary ->
            let
                updatedstore =
                    ball.legendStore - 0.01 * elapsed
            in
            if updatedstore <= 0 then
                { ball | legendstatus = Common, legendStore = 0 }

            else
                { ball | legendStore = updatedstore }


update_allPersonlist_byflow : Model -> Model
update_allPersonlist_byflow model =
    let
        allpeople =
            model.playframe.allPersonList

        elapsed =
            model.current_flow

        new_people =
            List.map (\x -> update_y elapsed x) allpeople
                |> checkPeopleExistYLimit

        playframe =
            model.playframe
    in
    { model | playframe = { playframe | allPersonList = new_people } }


checkPeopleExistYLimit : List Person -> List Person
checkPeopleExistYLimit list =
    List.filter (\{ anchor } -> anchor.y <= 1488) list


update_y : Float -> Person -> Person
update_y elapsed person =
    let
        anchor =
            person.anchor
    in
    { person | anchor = { anchor | y = anchor.y + 0.22 * elapsed } }


checkDeath : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
checkDeath ( model, cmd ) =
    let
        page =
            --case level panduan
            model.page
    in
    case model.playframe.ball.live of
        Alive ->
            ( model, cmd )

        Dead ->
            { model | state = Death, page = { page | state = End model.levelframe.level } } |> updategamestate Noop



--state page


updatePass : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updatePass ( model, cmd ) =
    let
        levelframe =
            model.levelframe

        level =
            levelframe.level

        page =
            model.page
    in
    if model.time > 18600 && checkDoor model.playframe.ball model.playframe.door then
        if level > 0 then
            ( initModel level
                |> (\a ->
                        { a
                            | page = { page | state = Win level }
                            , score = model.score + model.playframe.ball.score
                            , state = Pass
                        }
                   )
            , cmd
            )

        else
            ( model, cmd )

    else
        ( model, cmd )


changePaddlebyCrush : Paddle -> List Person -> ( Paddle, List Person )
changePaddlebyCrush paddle list =
    let
        ( crashed, remain ) =
            List.partition (\x -> checkPaddleCrash paddle x) list
    in
    ( List.foldl changePaddlebyPerson paddle (List.map getIdentity crashed)
        |> checkPaddleZeroLength
    , remain
    )


getIdentity : { a | identity : Identity } -> Identity
getIdentity person =
    person.identity
