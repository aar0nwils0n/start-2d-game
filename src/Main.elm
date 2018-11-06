module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Lazy
import Sprite exposing (Sprite)
import Svg exposing (..)
import Svg.Attributes exposing (height, viewBox, width)
import Time


type Msg
    = Tick


type alias Model =
    { dynamicSprites : List Sprite.DynamicSprite
    , staticSprites : List Sprite
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { dynamicSprites =
            [ { x = 0
              , y = 0
              , width = 20
              , height = 20
              , velocityX = 0
              , velocityY = 0
              }
            ]
      , staticSprites =
            [ { x = 0
              , y = 390
              , width = 400
              , height = 10
              }
            ]
      }
    , Cmd.none
    )


transformSprite staticSprites sprite =
    if Sprite.collidesAny staticSprites sprite then
        sprite

    else
        Sprite.accelerateSprite sprite


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            ( { model
                | dynamicSprites =
                    List.map
                        (transformSprite model.staticSprites)
                        model.dynamicSprites
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    svg
        [ width "500"
        , height "400"
        , viewBox "0 500 400"
        ]
        (List.map Sprite.createRect model.staticSprites
            ++ [ Html.Lazy.lazy (\dynamicSprites -> g [] <| List.map Sprite.createRect dynamicSprites) model.dynamicSprites
               ]
        )


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions =
            always <|
                Sub.batch
                    [ Time.every 20 <| always Tick
                    ]
        }
