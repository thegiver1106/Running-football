module Blocks exposing (RecSize,Circle, Rectangle, getInitBlocks, getInitPaddle,retRecSize,retRadius)

import Color exposing (Color, getcolor)
import General exposing (Point, Unzipper, backtolist, changeUnzipExpose, getIntervalNumber, getpoint, getziplocate, getzipperele, turnToUnzipper)


type alias RecSize =
    { halflength : Float
    , halfwidth : Float
    }


type alias Rectangle =
    { anchor : Point
    , color : Color
    , size : RecSize
   
    }

type alias Circle = 
    {
        anchor: Point
        , radius : Float 
    }


getRecsize : Float -> Float -> RecSize
getRecsize hl hw =
    RecSize hl hw


constructSingleRec : Point -> Color -> RecSize -> Rectangle
constructSingleRec apoint acolor asize  =
    Rectangle apoint acolor asize 


constructInitSingleBlock : Point -> Rectangle
constructInitSingleBlock point =
    constructSingleRec point (getcolor 255 0 0) (getRecsize 32 16)


getBlack : Color
getBlack =
    getcolor 0 0 0


getInitBlockLocate : Unzipper ( Float, Float )
getInitBlockLocate =
    let
        xlist =
            getIntervalNumber 4 72 6 24 []
                |> turnToUnzipper
    in
    xMapToPoint xlist 6 ( [], [] )


getInitBlocks : List Rectangle
getInitBlocks =
    let
        points =
            backtolist getInitBlockLocate
    in
    List.map constructInitSingleBlock (List.map getpoint points)


getInitPaddle : Rectangle
getInitPaddle =
    constructSingleRec (getpoint ( 144, 400 )) getBlack (getRecsize 48 12) 

xMapToPoint : Unzipper Float -> Int -> Unzipper ( Float, Float ) -> Unzipper ( Float, Float )
xMapToPoint unzipper time targ =
    if List.length (Tuple.second unzipper) == 0 then
        targ

    else
        let
            locate =
                getziplocate unzipper

            origin =
                Tuple.second targ
        in
        case getzipperele unzipper of
            Just ele ->
                xMapToPoint (changeUnzipExpose unzipper (locate + 1)) time ( [], List.append origin [ Tuple.pair ele (36 * toFloat (locate // time + 1)) ] )

            Nothing ->
                xMapToPoint (changeUnzipExpose unzipper (locate + 1)) time targ


retRecSize : {a|size: RecSize} ->(Float,Float)
retRecSize rectangle =
    (rectangle.size.halflength,rectangle.size.halfwidth)

retRadius : {a|radius: Float}-> Float
retRadius {radius} = 
    radius