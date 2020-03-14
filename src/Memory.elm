module Memory exposing
    ( Address
    , Memory
    , Nibble(..)
    , Word
    , add
    , and
    , fromCollision
    , fromNibble
    , init
    , loadRom
    , next
    , random
    , read
    , readSprite
    , toCoordinate
    , toNibble
    , undefined
    , viewAddress
    , viewWord
    )

import Bitwise
import Bytes exposing (Bytes)
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Dict exposing (Dict)
import Html exposing (Html, text)
import Random exposing (Generator)


type Memory
    = Memory (Dict Int Word)


type Nibble
    = Nibble Int


type Word
    = Word Int


undefined : Word
undefined =
    Word 0x00


toNibble : Word -> ( Nibble, Nibble )
toNibble (Word x) =
    ( Nibble (x // 16), Nibble (modBy 16 x) )


toCoordinate : Word -> Word -> ( Int, Int )
toCoordinate (Word x) (Word y) =
    ( x, y )


toSprite : List Word -> List (List Bool)
toSprite ws =
    let
        toLine (Word x) =
            List.map ((/=) 0 << Bitwise.and x)
                [ 128, 64, 32, 16, 8, 4, 2, 1 ]
    in
    List.map toLine ws


fromCollision : Bool -> Word
fromCollision coll =
    if coll then
        Word 0x01

    else
        Word 0x00


random : Generator Word
random =
    Random.map Word <| Random.int 0x00 0xFF


and : Word -> Word -> Word
and (Word x1) (Word x2) =
    Word <| Bitwise.and x1 x2


add : Word -> Word -> Word
add (Word x1) (Word x2) =
    Word <| modBy 0xFF <| x1 + x2


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


next : Address -> Address
next (Address addr) =
    Address (addr + 1)


read : Address -> Memory -> Word
read (Address addr) (Memory mem) =
    Maybe.withDefault undefined <| Dict.get addr mem


readSprite : Nibble -> Address -> Memory -> List (List Bool)
readSprite size origin =
    toSprite << readChunk size origin


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
