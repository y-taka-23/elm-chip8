module Main exposing (main)

import Browser
import Display exposing (Display)
import Html exposing (Html, div)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type Msg
    = NoOp


type alias Model =
    { display : Display }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { display = Display.init }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ Display.view model.display
        ]
