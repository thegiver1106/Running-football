module Color exposing (Color, getcolor)
{-| This file defines the Color type and help get color.
  It makes color design easier.
-}

type alias Color =
    { r : Int
    , g : Int
    , b : Int
    }


getcolor : Int -> Int -> Int -> Color
getcolor red green blue =
    Color red green blue
