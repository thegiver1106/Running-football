module General exposing (ConstructStatus(..), Dir(..), Identity(..), LegendStatus(..), LiveStatus(..), Point, Unzipper, Vec, backtolist, changeUnzipExpose, fixxmapy, getIntervalNumber, getfirstx, getpoint, getsomeele, getziplocate, getzipperele, turnToUnzipper, vecNormalize,getxval,mode)
{-|  This file defines basic types and function may be used later.
-}

import Json.Encode as Encode
import List exposing (unzip)


type ConstructStatus
    = Destroyable Int
    | Undestroyable


type Identity
    = Passerby
    | Friend
    | Enemy
    | Me


type LegendStatus
    = Common
    | Legendary 


type LiveStatus
    = Alive
    | Dead


type Dir
    = Left
    | Right


type alias Point =
    { x : Float
    , y : Float
    }


type alias Unzipper a =
    ( List a, List a )



-- The idea of unzipper is gotten from VG100 TA Runze Xue from VG100 piazza,
-- I thanks him very much for giving such an inspiring idea.


type alias Vec =
    { x : Float
    , y : Float
    }


vecNormalize : Vec -> Vec
vecNormalize vec =
    let
        x =
            vec.x

        y =
            vec.y

        magnitude =
            sqrt (x * x + y * y)
    in
    Vec (x / magnitude) (y / magnitude)


getIntervalNumber : Float -> Float -> Int -> Int -> List Float -> List Float



-- A function that gets some cycling number
--eg.
-- getIntervalNumber  1 3 4 10 [] =
-- [1,4,7,10,1,4,7,10,1,4]


getIntervalNumber ini inte linenum totalnum list =
    if totalnum <= linenum then
        getintervalnumbersub ini inte totalnum list

    else
        getIntervalNumber ini inte linenum (totalnum - linenum) (getintervalnumbersub ini inte linenum list)


getpoint : ( Float, Float ) -> Point
getpoint ( x, y ) =
    Point x y


getintervalnumbersub : Float -> Float -> Int -> List Float -> List Float



--local funtion, sub to getintervelnumber


getintervalnumbersub ini inte num list =
    if num <= 0 then
        list

    else
        getintervalnumbersub (ini + inte) inte (num - 1) (List.append list [ ini ])


getsomeele : List a -> Int -> Int -> List a



-- return some elements of a list as a list
-- eg. list = [1,2,3,4,5]
-- getsomeele list 2 4 =
-- [2,3,4]


getsomeele list start end =
    if start > end then
        []

    else if start <= 0 then
        []

    else if end > List.length list then
        getsomeele list start (List.length list)

    else if start > 1 then
        case List.tail list of
            Nothing ->
                []

            Just alist ->
                getsomeele alist (start - 1) (end - 1)

    else
        getfirstx list end


getfirstx : List a -> Int -> List a
getfirstx list num =
    if num <= 0 then
        []

    else if num < List.length list then
        let
            newlist =
                list
                    |> List.reverse
                    |> List.tail
                    |> Maybe.withDefault []
                    |> List.reverse
        in
        getfirstx newlist num

    else
        list


fixxmapy : Float -> (Float -> ( Float, Float ))
fixxmapy x =
    \y -> Tuple.pair x y


fixymapx : Float -> (Float -> ( Float, Float ))
fixymapx y =
    \x -> Tuple.pair x y


turnToUnzipper : List a -> Unzipper a
turnToUnzipper list =
    ( [], list )


gettail : List a -> List a
gettail list =
    case List.tail list of
        Just atail ->
            atail

        Nothing ->
            []


getunzipperlength : Unzipper a -> Int
getunzipperlength unzipper =
    List.append (Tuple.first unzipper) (Tuple.second unzipper)
        |> List.length


getzipperele : Unzipper a -> Maybe a
getzipperele unzip =
    let
        sec =
            Tuple.second unzip
    in
    List.head sec


getziplocate : Unzipper a -> Int
getziplocate unzipper =
    List.length (Tuple.first unzipper)


backtolist : Unzipper a -> List a
backtolist azip =
    let
        forward =
            Tuple.first azip

        back =
            Tuple.second azip
    in
    List.append forward back


changeUnzipExpose : Unzipper a -> Int -> Unzipper a
changeUnzipExpose unzipper locate =
    let
        forward =
            Tuple.first unzipper

        back =
            Tuple.second unzipper
    in
    if List.length forward + List.length back < locate then
        ( forward, back )

    else if List.length forward == locate then
        ( forward, back )

    else if locate > List.length forward then
        case List.head back of
            Just element ->
                changeUnzipExpose ( List.append forward [ element ], gettail back ) locate

            Nothing ->
                ( forward, back )

    else if locate < List.length forward then
        let
            revforward =
                List.reverse forward
        in
        case List.head revforward of
            Just element ->
                changeUnzipExpose ( gettail forward, List.append [ element ] back ) locate

            Nothing ->
                ( forward, back )

    else
        ( forward, back )

getxval : Int -> List String -> String
getxval position list =
    if position > 0 then 
        case position of 
            1 ->
                case list of 
                    [] -> ""
                    (x::xs) -> x
            _ -> getxval (position - 1) (List.drop 1 list) -- -- cut front element
    else
        ""
mode : Float -> Float -> Float
mode targ var =
    if targ < var then
        targ
    else
        targ - toFloat(floor(targ/var)) * var
