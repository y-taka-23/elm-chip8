module Memory exposing (Memory, init, loadRom)

import Bytes exposing (Bytes)
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Dict exposing (Dict)


type Memory
    = Memory (Dict Int Word)


type Word
    = Word Int


init : Memory
init =
    Memory Dict.empty


loadRom : Bytes -> Maybe Memory
loadRom rom =
    let
        words =
            Decode.decode (list (Bytes.width rom) word) rom

        offset =
            List.indexedMap (\i w -> ( i + 0x0200, w ))
    in
    Maybe.map (Memory << Dict.fromList << offset) words


word : Decoder Word
word =
    Decode.map Word Decode.unsignedInt8


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
