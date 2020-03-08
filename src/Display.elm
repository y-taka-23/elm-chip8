module Display exposing (Display, init, view)

import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, classList, id)


type Display
    = Display (Dict Int Bool)


width : Int
width =
    64


height =
    32


init : Display
init =
    Display <|
        Dict.fromList <|
            List.map (\i -> ( i, modBy 3 i == 0 )) <|
                List.range 0 (width * height - 1)


view : Display -> Html msg
view (Display dots) =
    div [ id "display", class "pane" ] <|
        List.map dot <|
            Dict.values dots


dot : Bool -> Html msg
dot on =
    div [ classList [ ( "dot", True ), ( "dot-on", on ) ] ] []
