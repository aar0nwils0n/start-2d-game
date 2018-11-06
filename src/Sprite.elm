module Sprite exposing (DynamicSprite, GenericSprite, MovingObject, Sprite, TRBL, accelerateSprite, collides, collidesAny, createRect, spriteToTRBL)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes as Attr


type alias GenericSprite a =
    { a
        | x : Float
        , y : Float
        , width : Float
        , height : Float
    }


type alias Sprite =
    GenericSprite {}


type alias MovingObject a =
    { a
        | velocityX : Float
        , velocityY : Float
    }


type alias DynamicSprite =
    MovingObject Sprite


type alias TRBL =
    { top : Float
    , right : Float
    , bottom : Float
    , left : Float
    }


spriteToTRBL : GenericSprite a -> TRBL
spriteToTRBL { x, y, width, height } =
    { top = y + height
    , right = x + width
    , bottom = y
    , left = x
    }


createRect : GenericSprite a -> Html msg
createRect { width, height, x, y } =
    rect
        [ Attr.width <| String.fromFloat width
        , Attr.height <| String.fromFloat height
        , Attr.x <| String.fromFloat x
        , Attr.y <| String.fromFloat y
        , Attr.fill "#000000"
        ]
        []


accelerateSprite : DynamicSprite -> DynamicSprite
accelerateSprite object =
    { object | velocityY = object.velocityY + 0.2, y = object.y + object.velocityY }


collides a b =
    let
        rectA =
            spriteToTRBL a

        rectB =
            spriteToTRBL b
    in
    rectA.left
        < rectB.right
        && rectA.right
        > rectB.left
        && rectA.top
        > rectB.bottom
        && rectA.bottom
        < rectB.top



-- accelerateSprite : List (GenericSprite a) -> GenericSprite b -> Bool


collidesAny sprites spriteToCheck =
    List.foldl (\cur agg -> agg || collides cur spriteToCheck) False sprites
