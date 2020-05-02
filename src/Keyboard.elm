module Keyboard exposing (view)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick, onMouseDown, onMouseUp)
import Memory.Word exposing (Nibble(..))


view : (Nibble -> msg) -> (Nibble -> msg) -> (Nibble -> msg) -> Html msg
view onPressKey onDownKey onUpKey =
    let
        key label n =
            div
                [ class "key"
                , onClick (onPressKey n)
                , onMouseDown (onDownKey n)
                , onMouseUp (onUpKey n)
                ]
                [ text label ]
    in
    div [ id "keyboard", class "pane" ]
        [ div [ class "row" ]
            [ key "1" (Nibble 0x01)
            , key "2" (Nibble 0x02)
            , key "3" (Nibble 0x03)
            , key "C" (Nibble 0x0C)
            ]
        , div [ class "row" ]
            [ key "4" (Nibble 0x04)
            , key "5" (Nibble 0x05)
            , key "6" (Nibble 0x06)
            , key "D" (Nibble 0x0D)
            ]
        , div [ class "row" ]
            [ key "7" (Nibble 0x07)
            , key "8" (Nibble 0x08)
            , key "9" (Nibble 0x09)
            , key "E" (Nibble 0x0E)
            ]
        , div [ class "row" ]
            [ key "A" (Nibble 0x0A)
            , key "0" (Nibble 0x00)
            , key "B" (Nibble 0x0B)
            , key "F" (Nibble 0x0F)
            ]
        ]
