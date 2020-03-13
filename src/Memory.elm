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
    , undefined
    , viewAddress
    , viewWord
    )

import Bytes exposing (Bytes)
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Dict exposing (Dict)
import Html exposing (Html, text)


type Memory
    = Memory (Dict Int Word)


type Nibble
    = Nibble Int


type Word
    = Word Int


undefined : Word
undefined =
    Word 0


type Address
    = Address Int


init : ( Memory, Address )
init =
    ( Memory <|
        Dict.fromList <|
            List.indexedMap Tuple.pair <|
                List.concat font
    , Address 0x0200
    )


font : List (List Word)
font =
    [ [ Word 0xF0, Word 0x90, Word 0x90, Word 0x90, Word 0xF0 ]
    , [ Word 0x20, Word 0x60, Word 0x20, Word 0x20, Word 0x70 ]
    , [ Word 0xF0, Word 0x10, Word 0xF0, Word 0x80, Word 0xF0 ]
    , [ Word 0xF0, Word 0x10, Word 0xF0, Word 0x10, Word 0xF0 ]
    , [ Word 0x90, Word 0x90, Word 0xF0, Word 0x10, Word 0x10 ]
    , [ Word 0xF0, Word 0x80, Word 0xF0, Word 0x10, Word 0xF0 ]
    , [ Word 0xF0, Word 0x80, Word 0xF0, Word 0x90, Word 0xF0 ]
    , [ Word 0xF0, Word 0x10, Word 0x20, Word 0x40, Word 0x40 ]
    , [ Word 0xF0, Word 0x90, Word 0xF0, Word 0x90, Word 0xF0 ]
    , [ Word 0xF0, Word 0x90, Word 0xF0, Word 0x10, Word 0xF0 ]
    , [ Word 0xF0, Word 0x90, Word 0xF0, Word 0x90, Word 0x90 ]
    , [ Word 0xE0, Word 0x90, Word 0xE0, Word 0x90, Word 0xE0 ]
    , [ Word 0xF0, Word 0x80, Word 0x80, Word 0x80, Word 0xF0 ]
    , [ Word 0xE0, Word 0x90, Word 0x90, Word 0x90, Word 0xE0 ]
    , [ Word 0xF0, Word 0x80, Word 0xF0, Word 0x80, Word 0xF0 ]
    , [ Word 0xF0, Word 0x80, Word 0xF0, Word 0x80, Word 0x80 ]
    ]


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
    undefined


loadRom : Bytes -> Maybe Memory
loadRom rom =
    case init of
        ( Memory mem, Address addr ) ->
            let
                words =
                    Decode.decode (list (Bytes.width rom) word) rom

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


viewWord : Word -> Html msg
viewWord (Word x) =
    text <| String.fromInt x


viewAddress : Address -> Html msg
viewAddress (Address addr) =
    text <| String.fromInt addr
