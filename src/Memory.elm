module Memory exposing
    ( Address
    , Memory
    , Nibble(..)
    , Word
    , fromNibble
    , init
    , loadRom
    , next
    , read
    , toNibble
    )

import Bytes exposing (Bytes)
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Dict exposing (Dict)


type Memory
    = Memory (Dict Int Word)


type Nibble
    = Nibble Int


type Word
    = Word Int


type Address
    = Address Int


init : ( Memory, Address )
init =
    ( Memory Dict.empty, Address 0x0200 )


fromNibble : Nibble -> Nibble -> Nibble -> Address
fromNibble (Nibble x1) (Nibble x2) (Nibble x3) =
    Address <| (16 * 16 * x1) + (16 * x2) + x3


toNibble : Word -> ( Nibble, Nibble )
toNibble (Word x) =
    ( Nibble (x // 16), Nibble (modBy 16 x) )


next : Address -> Address
next (Address addr) =
    Address (addr + 1)


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
