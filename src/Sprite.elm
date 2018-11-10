module Sprite exposing (DynamicSprite, GenericSprite, MovingObject, Sprite, TRBL, accelerateSprite, accelerateSpriteWithFloor, collides, collidesAnyBottomToTop, collidesBottomToTop, collidesWith, createRect, setSpriteVelocityX, setSpriteVelocityY, spriteToTRBL, transformSprite)

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
    { top = y
    , right = x + width
    , bottom = y + height
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
    { object
        | velocityY = object.velocityY + 0.2
        , y = object.y + object.velocityY
        , x = object.x + object.velocityX
    }


accelerateSpriteWithFloor : DynamicSprite -> List Sprite -> DynamicSprite
accelerateSpriteWithFloor object collidingSprites =
    let
        collidingSprite =
            List.head collidingSprites

        yFromBottom =
            Maybe.map (spriteToTRBL >> .top >> (\top -> top - object.height)) collidingSprite
                |> Maybe.withDefault object.y

        ( newY, newVelocityY ) =
            if object.velocityY > 0 then
                ( yFromBottom, 0 )

            else
                ( object.y + object.velocityY, object.velocityY )
    in
    { object
        | y = newY
        , velocityY = newVelocityY
        , x = object.x + object.velocityX
    }


setSpriteVelocityX : Float -> DynamicSprite -> DynamicSprite
setSpriteVelocityX velocity sprite =
    { sprite
        | velocityX = velocity
    }


setSpriteVelocityY : Float -> DynamicSprite -> DynamicSprite
setSpriteVelocityY velocity sprite =
    { sprite
        | velocityY = velocity
    }


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
        && rectA.bottom
        > rectB.top
        && rectA.top
        < rectB.bottom


transformSprite staticSprites sprite =
    let
        collidingSprites =
            collidesWith staticSprites sprite
    in
    if collidingSprites |> List.length |> (<) 0 then
        accelerateSpriteWithFloor sprite collidingSprites

    else
        accelerateSprite sprite



-- accelerateSprite : List (GenericSprite a) -> GenericSprite b -> Bool


collidesWith sprites spriteToCheck =
    List.filter (\sprite -> collides sprite spriteToCheck) sprites


collidesAnyBottomToTop sprites spriteToCheck =
    List.foldl (\cur agg -> agg || collidesBottomToTop cur spriteToCheck) False sprites


collidesBottomToTop a b =
    let
        rectA =
            spriteToTRBL a

        rectB =
            spriteToTRBL b
    in
    rectB.bottom
        >= rectA.top
        && rectB.top
        < rectA.top
        && rectB.left
        < rectA.right
        && rectB.right
        > rectA.left



-- todo change to ==
