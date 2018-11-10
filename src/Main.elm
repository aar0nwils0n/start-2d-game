module Main exposing (main)

import Browser
import Browser.Events as Events
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Lazy
import Input
import Json.Decode as Decode
import Sprite exposing (Sprite)
import Svg exposing (..)
import Svg.Attributes exposing (height, viewBox, width)
import Time


type Msg
    = Tick
    | OnKeyDown String
    | OnKeyUp String


type alias Model =
    { playerSprite : Sprite.DynamicSprite
    , staticSprites : List Sprite
    , keysState : Dict String Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { playerSprite =
            { x = 0
            , y = 0
            , width = 20
            , height = 20
            , velocityX = 0
            , velocityY = 0
            }
      , keysState = Dict.fromList []
      , staticSprites =
            [ { x = 0
              , y = 390
              , width = 400
              , height = 10
              }
            , { x = 0
              , y = 320
              , width = 100
              , height = 10
              }
            , { x = 100
              , y = 280
              , width = 100
              , height = 10
              }
            ]
      }
    , Cmd.none
    )


transformSprite staticSprites sprite =
    if Sprite.collidesWith staticSprites sprite |> List.length |> (<) 0 then
        Sprite.accelerateSpriteWithFloor sprite

    else
        Sprite.accelerateSprite sprite


setLeftRightVelocity model =
    let
        sprite =
            case ( Dict.get "ArrowLeft" model.keysState, Dict.get "ArrowRight" model.keysState ) of
                ( Just True, Just True ) ->
                    Sprite.setSpriteVelocityX 0 model.playerSprite

                ( Just True, _ ) ->
                    Sprite.setSpriteVelocityX -1.5 model.playerSprite

                ( _, Just True ) ->
                    Sprite.setSpriteVelocityX 1.5 model.playerSprite

                _ ->
                    Sprite.setSpriteVelocityX 0 model.playerSprite
    in
    { model | playerSprite = sprite }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            ( { model
                | playerSprite =
                    transformSprite model.staticSprites
                        model.playerSprite
              }
                |> setLeftRightVelocity
            , Cmd.none
            )

        OnKeyDown key ->
            let
                updatedKeysState =
                    { model
                        | keysState = Dict.insert key True model.keysState
                    }
            in
            ( case key of
                "ArrowUp" ->
                    { updatedKeysState
                        | playerSprite =
                            if Sprite.collidesAnyBottomToTop updatedKeysState.staticSprites updatedKeysState.playerSprite then
                                Sprite.setSpriteVelocityY -5 updatedKeysState.playerSprite

                            else
                                updatedKeysState.playerSprite
                    }

                _ ->
                    updatedKeysState
            , Cmd.none
            )

        OnKeyUp key ->
            let
                updatedKeysState =
                    { model
                        | keysState = Dict.insert key False model.keysState
                    }
            in
            ( updatedKeysState, Cmd.none )


view model =
    { title = "foo"
    , body =
        [ svg
            [ width "500"
            , height "400"
            , viewBox "0 500 400"
            ]
            ([ Sprite.createRect model.playerSprite ]
                ++ [ Html.Lazy.lazy (\playerSprite -> g [] <| List.map Sprite.createRect playerSprite) model.staticSprites
                   ]
            )
        ]
    }


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions =
            always <|
                Sub.batch
                    [ Time.every 20 <| always Tick
                    , Events.onKeyDown (Decode.at [ "key" ] Decode.string |> Decode.map OnKeyDown)
                    , Events.onKeyUp (Decode.at [ "key" ] Decode.string |> Decode.map OnKeyUp)
                    ]
        }
