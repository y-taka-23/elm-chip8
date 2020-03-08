module Main exposing (main)

import Browser
import Browser.Events as Browser
import Control exposing (Control)
import Display exposing (Display)
import Html exposing (Html, div)
import Html.Attributes exposing (id)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type Msg
    = Run
    | Pause
    | Step


type alias Model =
    { control : Control
    , display : Display
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { control = Control.init
      , display = Display.init
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Run ->
            ( { model | control = Control.run model.control }, Cmd.none )

        Pause ->
            ( { model | control = Control.pause model.control }, Cmd.none )

        Step ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    if Control.isRunning model.control then
        Browser.onAnimationFrame <| always Step

    else
        Sub.none


view : Model -> Html Msg
view model =
    div [ id "main-container" ]
        [ Control.view Run Pause Step model.control
        , Display.view model.display
        ]
