module Cpu exposing (Cpu, decode, execute, fetch, init, view)

import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, id)
import Memory exposing (Address, Memory, Nibble(..), Word)


type Cpu
    = Cpu
        { step : Int
        , pc : Address
        , index : Address
        , registers : Registers
        , delayTimer : Word
        , soundTimer : Word
        }


type Register
    = Register Int


type Registers
    = Registers (Dict Int Word)


read : Nibble -> Registers -> Word
read (Nibble x) (Registers regs) =
    Maybe.withDefault Memory.undefined <| Dict.get x regs


type Instruction
    = Clear
    | Return
    | Jump Address
    | Call Address
    | SkipEq Register Word
    | SkipNeq Register Word
    | SkipRegEq Register Register
    | Load Register Word
    | Add Register Word
    | Move Register Register
    | Or Register Register
    | And Register Register
    | Xor Register Register
    | AddReg Register Register
    | SubReg Register Register
    | ShiftR Register Register
    | ShiftL Register Register
    | SkipRegNeq Register Register
    | LoadIdx Address
    | JumpIdx Address
    | Rand Register Word
    | Draw Register Register Nibble
    | SkipPress Register
    | SkipUp Register
    | MoveDelay Register
    | KeyDelay Register
    | LoadDelay Register
    | LoadSound Register
    | AddIdx Register
    | LoadSprite Register
    | Bcd Register
    | Store Register
    | Read Register
    | Unknown


init : Cpu
init =
    Cpu
        { step = 0
        , pc = Tuple.second Memory.init
        , index = Tuple.second Memory.init
        , registers = Registers Dict.empty
        , delayTimer = Memory.undefined
        , soundTimer = Memory.undefined
        }


fromNibble : Nibble -> Register
fromNibble (Nibble x) =
    Register x


fetch : Cpu -> Memory -> ( Word, Word )
fetch (Cpu cpu) mem =
    ( Memory.read cpu.pc mem
    , Memory.read (Memory.next cpu.pc) mem
    )


decode : ( Word, Word ) -> Instruction
decode ( w1, w2 ) =
    case ( Memory.toNibble w1, Memory.toNibble w2 ) of
        ( ( Nibble 0x00, Nibble 0x00 ), ( Nibble 0x0E, Nibble 0x00 ) ) ->
            Clear

        ( ( Nibble 0x00, Nibble 0x00 ), ( Nibble 0x0E, Nibble 0x0E ) ) ->
            Return

        ( ( Nibble 0x01, n2 ), ( n3, n4 ) ) ->
            Jump (Memory.fromNibble n2 n3 n4)

        ( ( Nibble 0x02, n2 ), ( n3, n4 ) ) ->
            Call (Memory.fromNibble n2 n3 n4)

        ( ( Nibble 0x03, n2 ), ( _, _ ) ) ->
            SkipEq (fromNibble n2) w2

        ( ( Nibble 0x04, n2 ), ( _, _ ) ) ->
            SkipNeq (fromNibble n2) w2

        ( ( Nibble 0x05, n2 ), ( n3, Nibble 0x00 ) ) ->
            SkipRegEq (fromNibble n2) (fromNibble n3)

        ( ( Nibble 0x06, n2 ), ( _, _ ) ) ->
            Load (fromNibble n2) w2

        ( ( Nibble 0x07, n2 ), ( _, _ ) ) ->
            Add (fromNibble n2) w2

        ( ( Nibble 0x08, n2 ), ( n3, Nibble 0x00 ) ) ->
            Move (fromNibble n2) (fromNibble n3)

        ( ( Nibble 0x08, n2 ), ( n3, Nibble 0x01 ) ) ->
            Or (fromNibble n2) (fromNibble n3)

        ( ( Nibble 0x08, n2 ), ( n3, Nibble 0x02 ) ) ->
            And (fromNibble n2) (fromNibble n3)

        ( ( Nibble 0x08, n2 ), ( n3, Nibble 0x03 ) ) ->
            Xor (fromNibble n2) (fromNibble n3)

        ( ( Nibble 0x08, n2 ), ( n3, Nibble 0x04 ) ) ->
            AddReg (fromNibble n2) (fromNibble n3)

        ( ( Nibble 0x08, n2 ), ( n3, Nibble 0x05 ) ) ->
            SubReg (fromNibble n2) (fromNibble n3)

        ( ( Nibble 0x08, n2 ), ( n3, Nibble 0x06 ) ) ->
            ShiftR (fromNibble n2) (fromNibble n3)

        ( ( Nibble 0x08, n2 ), ( n3, Nibble 0x0E ) ) ->
            ShiftL (fromNibble n2) (fromNibble n3)

        ( ( Nibble 0x09, n2 ), ( n3, Nibble 0x00 ) ) ->
            SkipRegNeq (fromNibble n2) (fromNibble n3)

        ( ( Nibble 0x0A, n2 ), ( n3, n4 ) ) ->
            LoadIdx (Memory.fromNibble n2 n3 n4)

        ( ( Nibble 0x0B, n2 ), ( n3, n4 ) ) ->
            JumpIdx (Memory.fromNibble n2 n3 n4)

        ( ( Nibble 0x0C, n2 ), _ ) ->
            Rand (fromNibble n2) w2

        ( ( Nibble 0x0D, n2 ), ( n3, n4 ) ) ->
            Draw (fromNibble n2) (fromNibble n3) n4

        ( ( Nibble 0x0E, n2 ), ( Nibble 0x09, Nibble 0x0E ) ) ->
            SkipPress (fromNibble n2)

        ( ( Nibble 0x0E, n2 ), ( Nibble 0x0A, Nibble 0x01 ) ) ->
            SkipUp (fromNibble n2)

        ( ( Nibble 0x0F, n2 ), ( Nibble 0x00, Nibble 0x07 ) ) ->
            MoveDelay (fromNibble n2)

        ( ( Nibble 0x0F, n2 ), ( Nibble 0x00, Nibble 0x0A ) ) ->
            KeyDelay (fromNibble n2)

        ( ( Nibble 0x0F, n2 ), ( Nibble 0x01, Nibble 0x05 ) ) ->
            LoadDelay (fromNibble n2)

        ( ( Nibble 0x0F, n2 ), ( Nibble 0x01, Nibble 0x08 ) ) ->
            LoadSound (fromNibble n2)

        ( ( Nibble 0x0F, n2 ), ( Nibble 0x01, Nibble 0x0E ) ) ->
            AddIdx (fromNibble n2)

        ( ( Nibble 0x0F, n2 ), ( Nibble 0x02, Nibble 0x09 ) ) ->
            LoadSprite (fromNibble n2)

        ( ( Nibble 0x0F, n2 ), ( Nibble 0x03, Nibble 0x03 ) ) ->
            Bcd (fromNibble n2)

        ( ( Nibble 0x0F, n2 ), ( Nibble 0x05, Nibble 0x05 ) ) ->
            Store (fromNibble n2)

        ( ( Nibble 0x0F, n2 ), ( Nibble 0x06, Nibble 0x05 ) ) ->
            Read (fromNibble n2)

        _ ->
            Unknown


execute : Cpu -> Memory -> Instruction -> ( Cpu, Memory )
execute (Cpu cpu) mem _ =
    ( Cpu cpu, mem )


view : Cpu -> Html msg
view (Cpu cpu) =
    div [ id "cpu", class "pane" ]
        [ div [ class "raw" ]
            [ stretchCell "Step Count" <| text <| String.fromInt cpu.step ]
        , div [ class "raw" ]
            [ stretchCell "PC" <| Memory.viewAddress cpu.pc
            , stretchCell "I" <| Memory.viewAddress cpu.index
            , stretchCell "DT" <| Memory.viewWord cpu.delayTimer
            , stretchCell "ST" <| Memory.viewWord cpu.soundTimer
            ]
        , div [ class "raw" ]
            [ unitCell "V0" (Nibble 0x00) cpu.registers
            , unitCell "V1" (Nibble 0x01) cpu.registers
            , unitCell "V2" (Nibble 0x02) cpu.registers
            , unitCell "V3" (Nibble 0x03) cpu.registers
            , unitCell "V4" (Nibble 0x04) cpu.registers
            , unitCell "V5" (Nibble 0x05) cpu.registers
            , unitCell "V6" (Nibble 0x06) cpu.registers
            , unitCell "V7" (Nibble 0x07) cpu.registers
            ]
        , div [ class "raw" ]
            [ unitCell "V8" (Nibble 0x08) cpu.registers
            , unitCell "V9" (Nibble 0x09) cpu.registers
            , unitCell "VA" (Nibble 0x0A) cpu.registers
            , unitCell "VB" (Nibble 0x0B) cpu.registers
            , unitCell "VC" (Nibble 0x0C) cpu.registers
            , unitCell "VD" (Nibble 0x0D) cpu.registers
            , unitCell "VE" (Nibble 0x0E) cpu.registers
            , unitCell "VF" (Nibble 0x0F) cpu.registers
            ]
        ]


stretchCell : String -> Html msg -> Html msg
stretchCell label val =
    div [ class "cell", class "stretch" ]
        [ div [] [ text label ], div [] [ val ] ]


unitCell : String -> Nibble -> Registers -> Html msg
unitCell label n regs =
    let
        val =
            Memory.viewWord <| read n regs
    in
    div [ class "cell" ]
        [ div [] [ text label ], div [] [ val ] ]