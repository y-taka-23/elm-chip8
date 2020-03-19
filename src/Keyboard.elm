module Keyboard exposing (view)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Memory.Word exposing (Nibble(..))


view : (Nibble -> msg) -> Html msg
view onPressKey =
    div [ id "keyboard", class "pane" ]
        [ div [ class "row" ]
            [ key "1" <| onPressKey (Nibble 0x01)
            , key "2" <| onPressKey (Nibble 0x02)
            , key "3" <| onPressKey (Nibble 0x03)
            , key "C" <| onPressKey (Nibble 0x0C)
            ]
        , div [ class "row" ]
            [ key "4" <| onPressKey (Nibble 0x04)
            , key "5" <| onPressKey (Nibble 0x05)
            , key "6" <| onPressKey (Nibble 0x06)
            , key "D" <| onPressKey (Nibble 0x0D)
            ]
        , div [ class "row" ]
            [ key "7" <| onPressKey (Nibble 0x07)
            , key "8" <| onPressKey (Nibble 0x08)
            , key "9" <| onPressKey (Nibble 0x09)
            , key "E" <| onPressKey (Nibble 0x0E)
            ]
        , div [ class "row" ]
            [ key "A" <| onPressKey (Nibble 0x0A)
            , key "0" <| onPressKey (Nibble 0x00)
            , key "B" <| onPressKey (Nibble 0x0B)
            , key "F" <| onPressKey (Nibble 0x0F)
            ]
        ]


key : String -> msg -> Html msg
key label msg =
    div [ class "key", onClick msg ] [ text label ]
