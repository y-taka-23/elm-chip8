module Control exposing (Control, init, isRunning, pause, run, view)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


type Control
    = Paused
    | Running


init : Control
init =
    Paused


run : Control -> Control
run _ =
    Running


pause : Control -> Control
pause _ =
    Paused


isRunning : Control -> Bool
isRunning ctrl =
    case ctrl of
        Paused ->
            False

        Running ->
            True


view : msg -> msg -> msg -> msg -> Control -> Html msg
view onSelect onRun onPause onStep ctrl =
    div [ class "pane" ]
        [ div [ class "button", onClick onSelect ] [ text "Load ROM" ]
        , case ctrl of
            Paused ->
                div [ class "button", onClick onRun ] [ text "Run" ]

            Running ->
                div [ class "button", onClick onPause ] [ text "Pause" ]
        , div [ class "button", onClick onStep ] [ text "Step" ]
        ]
