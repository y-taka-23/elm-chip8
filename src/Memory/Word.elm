module Memory.Word exposing
    ( Nibble(..)
    , Word
    , add
    , and
    , decoder
    , font
    , fromFlag
    , fuzzer
    , random
    , toCoordinate
    , toNibbles
    , toSprite
    , undefined
    , view
    )

import Bitwise
import Bytes.Decode as Decode exposing (Decoder)
import Fuzz exposing (Fuzzer)
import Html exposing (Html, text)
import Random exposing (Generator)


type Nibble
    = Nibble Int


type Word
    = Word Int


undefined : Word
undefined =
    Word 0x00


font : List Word
font =
    List.concat
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


random : Generator Word
random =
    Random.map Word <| Random.int 0x00 0xFF


decoder : Decoder Word
decoder =
    Decode.map Word Decode.unsignedInt8


fuzzer : Fuzzer Word
fuzzer =
    Fuzz.map Word <| Fuzz.intRange 0x00 0xFF


fromFlag : Bool -> Word
fromFlag flag =
    if flag then
        Word 0x01

    else
        Word 0x00


toNibbles : Word -> ( Nibble, Nibble )
toNibbles (Word x) =
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


add : Word -> Word -> Word
add (Word x1) (Word x2) =
    Word <| modBy 0x0100 <| x1 + x2


and : Word -> Word -> Word
and (Word x1) (Word x2) =
    Word <| Bitwise.and x1 x2


view : Word -> Html msg
view (Word x) =
    text <| String.fromInt x
