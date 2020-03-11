module Memory exposing (Address, Memory, Word, init, loadRom, next, read)

import Bytes exposing (Bytes)
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Dict exposing (Dict)


type Memory
    = Memory (Dict Int Word)


type Address
    = Address Int


next : Address -> Address
next (Address addr) =
    Address (addr + 1)


type Word
    = Word Int


init : ( Memory, Address )
init =
    ( Memory Dict.empty, Address 0x0200 )


read : Address -> Memory -> Word
read _ _ =
    Word 0


loadRom : Bytes -> Maybe Memory
loadRom rom =
    let
        words =
            Decode.decode (list (Bytes.width rom) word) rom

        offset =
            case init of
                ( _, Address addr ) ->
                    List.indexedMap (\i w -> ( i + addr, w ))
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
