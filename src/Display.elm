module Display exposing (Display, draw, init, view)

import Dict exposing (Dict)
import Html exposing (Html, div)
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
            List.map (\i -> ( i, False )) <|
                List.range 0 (width * height - 1)


draw : ( Int, Int ) -> List (List Bool) -> Display -> ( Display, Bool )
draw ( x, y ) sprite disp =
    List.foldl (\( j, line ) -> drawLine x j line) ( disp, False ) <|
        List.indexedMap (\offset line -> ( y + offset, line )) sprite


drawLine : Int -> Int -> List Bool -> ( Display, Bool ) -> ( Display, Bool )
drawLine x y line ( disp, coll ) =
    List.foldl (\( i, d ) -> drawDot i y d) ( disp, coll ) <|
        List.indexedMap (\offset d -> ( x + offset, d )) line


drawDot : Int -> Int -> Bool -> ( Display, Bool ) -> ( Display, Bool )
drawDot x y d ( disp, coll ) =
    let
        prev =
            getDot x y disp
    in
    ( setDot x y (xor d prev) disp, coll || d && prev )


getDot : Int -> Int -> Display -> Bool
getDot x y (Display disp) =
    Maybe.withDefault False <| Dict.get (wrap x y) disp


setDot : Int -> Int -> Bool -> Display -> Display
setDot x y d (Display disp) =
    Display <| Dict.insert (wrap x y) d disp


wrap : Int -> Int -> Int
wrap x y =
    width * modBy height y + modBy width x


view : Display -> Html msg
view (Display dots) =
    div [ id "display", class "pane" ] <|
        List.map dot <|
            Dict.values dots


dot : Bool -> Html msg
dot on =
    div [ classList [ ( "dot", True ), ( "dot-on", on ) ] ] []
