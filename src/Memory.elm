module Memory exposing
    ( Address
    , Memory
    , fromNibble
    , init
    , loadRom
    , next
    , read
    , readSprite
    , viewAddress
    )

import Bytes exposing (Bytes)
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Dict exposing (Dict)
import Html exposing (Html, text)
import Memory.Word as Word exposing (Nibble(..), Word)


type Memory
    = Memory (Dict Int Word)


type Address
    = Address Int


init : ( Memory, Address )
init =
    ( Memory <| Dict.fromList <| List.indexedMap Tuple.pair Word.font
    , Address 0x0200
    )


fromNibble : Nibble -> Nibble -> Nibble -> Address
fromNibble (Nibble x1) (Nibble x2) (Nibble x3) =
    Address <| (16 * 16 * x1) + (16 * x2) + x3


next : Address -> Address
next (Address addr) =
    Address (addr + 1)


read : Address -> Memory -> Word
read (Address addr) (Memory mem) =
    Maybe.withDefault Word.undefined <| Dict.get addr mem


readSprite : Nibble -> Address -> Memory -> List (List Bool)
readSprite size origin =
    Word.toSprite << readChunk size origin


readChunk : Nibble -> Address -> Memory -> List Word
readChunk (Nibble size) (Address origin) mem =
    List.map (\a -> read a mem) <|
        List.map Address <|
            List.range origin (origin + size - 1)


loadRom : Bytes -> Maybe Memory
loadRom rom =
    case init of
        ( Memory mem, Address addr ) ->
            let
                words =
                    Decode.decode (list (Bytes.width rom) Word.decoder) rom

                offset =
                    List.indexedMap (\i w -> ( i + addr, w ))
            in
            Maybe.map
                (Memory
                    << (\d -> Dict.union d mem)
                    << Dict.fromList
                    << offset
                )
                words


list : Int -> Decoder a -> Decoder (List a)
list len decoder =
    Decode.map List.reverse <|
        Decode.loop ( len, [] ) (listStep decoder)


listStep :
    Decoder a
    -> ( Int, List a )
    -> Decoder (Step ( Int, List a ) (List a))
listStep decoder ( n, xs ) =
    if n <= 0 then
        Decode.succeed (Done xs)

    else
        Decode.map (\x -> Loop ( n - 1, x :: xs )) decoder


viewAddress : Address -> Html msg
viewAddress (Address addr) =
    text <| String.fromInt addr
