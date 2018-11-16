module Sprite exposing (DynamicSprite, GenericSprite, MovingObject, Sprite, TRBL, accelerateSprite, accelerateSpriteWithFloor, collides, collidesAnyFloor, collidesWith, createRect, hitsFloor, setSpriteVelocityX, setSpriteVelocityY, spriteToTRBL, transformSprite)

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


type VelocityType
    = Some
    | None
    | Negative


floatToVelocityType velocity =
    if velocity > 0 then
        Some

    else if velocity < 0 then
        Negative

    else
        None


accelerateSpriteWithFloor : DynamicSprite -> List Sprite -> DynamicSprite
accelerateSpriteWithFloor object collidingSprites =
    let
        floorSprite =
            collidesAnyFloor collidingSprites object |> List.head

        roofSprite =
            collidesAnyRoof collidingSprites object |> List.head

        velocityType =
            floatToVelocityType object.velocityY

        ( newY, newVelocityY ) =
            case ( velocityType, floorSprite, roofSprite ) of
                ( Some, Just collidingSprite, _ ) ->
                    let
                        yFromBottom =
                            collidingSprite |> spriteToTRBL |> .top |> (\top -> top - object.height)
                    in
                    ( yFromBottom, 0 )

                ( Negative, _, Just collidingSprite ) ->
                    let
                        yFromTop =
                            collidingSprite |> spriteToTRBL |> .bottom
                    in
                    ( yFromTop, 0 )

                _ ->
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


collidesAnyFloor =
    collidesAny hitsFloor


collidesAnyRoof =
    collidesAny hitsRoof


collidesAny checker sprites spriteToCheck =
    List.filter (checker spriteToCheck) sprites


hitsRoof a b =
    let
        rectA =
            spriteToTRBL a

        rectB =
            spriteToTRBL b
    in
    rectA.top
        <= rectB.bottom
        && rectA.bottom
        > rectB.bottom
        && rectA.left
        < rectB.right
        && rectA.right
        > rectB.left


hitsFloor a b =
    let
        rectA =
            spriteToTRBL a

        rectB =
            spriteToTRBL b
    in
    rectA.bottom
        >= rectB.top
        && rectA.top
        < rectB.top
        && rectA.left
        < rectB.right
        && rectA.right
        > rectB.left



-- todo change to ==
