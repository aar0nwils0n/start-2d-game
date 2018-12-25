module Sprite exposing (DynamicSprite, GenericSprite, MovingObject, Sprite, TRBL, accelerateSprite, collides, collidesAnyFloor, collidesWith, createRect, hitsFloor, setSpriteVelocityX, setSpriteVelocityY, spriteToTRBL, transformSprite)

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


calculateSpriteX : List Sprite -> DynamicSprite -> DynamicSprite
calculateSpriteX collidingSprites object =
    let
        velocityTypeX =
            floatToVelocityType object.velocityX

        leftSprite =
            collidesAnyLeft collidingSprites object |> List.head

        rightSprite =
            collidesAnyRight collidingSprites object |> List.head

        ( newX, newVelocityX ) =
            case ( velocityTypeX, leftSprite, rightSprite ) of
                ( Negative, Just collidingSprite, _ ) ->
                    let
                        xFromLeft =
                            collidingSprite |> spriteToTRBL |> .right
                    in
                    ( xFromLeft, 0 )

                ( Some, _, Just collidingSprite ) ->
                    let
                        xFromRight =
                            collidingSprite |> spriteToTRBL |> .left |> (\left -> left - object.width)
                    in
                    ( xFromRight, 0 )

                _ ->
                    ( object.x + object.velocityX, object.velocityX )
    in
    { object
        | velocityX = newVelocityX
        , x = newX
    }


calculateSpriteY : List Sprite -> DynamicSprite -> DynamicSprite
calculateSpriteY collidingSprites sprite =
    let
        floorSprite =
            collidesAnyFloor collidingSprites sprite |> List.head

        roofSprite =
            collidesAnyRoof collidingSprites sprite |> List.head

        velocityTypeY =
            floatToVelocityType sprite.velocityY
    in
    case ( floorSprite, roofSprite ) of
        ( Nothing, Nothing ) ->
            accelerateSprite sprite

        _ ->
            let
                ( newY, newVelocityY ) =
                    case ( velocityTypeY, floorSprite, roofSprite ) of
                        ( Some, Just collidingSprite, _ ) ->
                            let
                                yFromBottom =
                                    collidingSprite |> spriteToTRBL |> .top |> (\top -> top - sprite.height)
                            in
                            ( yFromBottom, 0 )

                        ( Negative, _, Just collidingSprite ) ->
                            let
                                yFromTop =
                                    collidingSprite |> spriteToTRBL |> .bottom
                            in
                            ( yFromTop, 0 )

                        _ ->
                            ( sprite.y + sprite.velocityY, sprite.velocityY )
            in
            { sprite
                | y = newY
                , velocityY = newVelocityY
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
        calculateSpriteY collidingSprites sprite
            |> calculateSpriteX collidingSprites

    else
        accelerateSprite sprite



-- accelerateSprite : List (GenericSprite a) -> GenericSprite b -> Bool


collidesWith sprites spriteToCheck =
    List.filter (\sprite -> collides sprite spriteToCheck) sprites


collidesAnyFloor =
    collidesAny hitsFloor


collidesAnyRoof =
    collidesAny hitsRoof


collidesAnyLeft =
    collidesAny hitsLeft


collidesAnyRight =
    collidesAny hitsRight


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


hitsLeft a b =
    let
        rectA =
            spriteToTRBL a

        rectB =
            spriteToTRBL b
    in
    rectB.right
        > rectA.left
        && rectB.left
        < rectA.left
        && rectB.bottom
        > rectA.top
        && rectB.top
        < rectA.bottom


hitsRight a b =
    let
        rectA =
            spriteToTRBL a

        rectB =
            spriteToTRBL b
    in
    rectB.left
        < rectA.right
        && rectB.right
        > rectA.right
        && rectB.bottom
        > rectA.top
        && rectB.top
        < rectA.bottom
